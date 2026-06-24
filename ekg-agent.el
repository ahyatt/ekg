;;; ekg-agent.el --- Agentic actions for ekg -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Package-Requires: ((emacs "28.1") (ekg "0.8.0") (futur "1.2"))
;; Keywords: outlines, hypermedia
;; Version: 0.0.1
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module provides agentic actions for ekg, allowing an LLM to
;; interact with the ekg note-taking system to help the user.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-provider-utils)
(require 'llm-vertex)
(require 'ekg-llm)
(require 'ekg-embedding)
(require 'ekg-org)
(require 'seq)
(require 'json)
(require 'subr-x)
(require 'shr)
(require 'url)
(require 'info)
(require 'help-fns)
(require 'apropos)

(when (featurep 'ns)
  ;; Disable futur's background thread before loading: on macOS NS port,
  ;; `message' (or any redisplay) from a non-main thread causes an NSException →
  ;; GIL deadlock.  With this nil, futur dispatches all callbacks via
  ;; `run-with-timer' on the main thread instead.
  (defvar futur-use-threads)
  (setq futur-use-threads nil))
(require 'futur)

;; Forward declarations for variables defined later in this file.
(defvar ekg-agent-base-tools)
(defvar ekg-agent-extra-tools)

;; Forward declarations for optional external packages.
(declare-function flycheck-mode "flycheck")
(declare-function org-ql-search "org-ql")
(declare-function markdown-mode "markdown-mode")
(defvar eww-search-prefix)

(defgroup ekg-agent nil
  "Agentic actions for ekg."
  :group 'ekg)

(defcustom ekg-agent-author-tag "agent"
  "The tag used to identify notes created by the agent."
  :type 'string
  :group 'ekg-agent)

(defcustom ekg-agent-self-info-tag "agent/self-info"
  "The tag used to identify notes with agent information for itself."
  :type 'string
  :group 'ekg-agent)

(add-to-list 'ekg-hidden-tags ekg-agent-self-info-tag)

(defcustom ekg-agent-daily-time "09:00"
  "Time of day to run the daily agent evaluation, in HH:MM format (24-hour).
For example, \"09:00\" for 9:00 AM, \"14:30\" for 2:30 PM.
Changes to this variable will take effect the next time you call
`ekg-agent-schedule-daily`."
  :type 'string
  :group 'ekg-agent)

(defcustom ekg-agent-log-buffer-name-format "*ekg agent log: %s*"
  "Name of the buffer used for logging ekg agent progress.

The string var will be filled in with the name of the buffer, so
this MUST have a %s in it."
  :type 'string
  :group 'ekg-agent)

(defcustom ekg-agent-log-window-side 'bottom
  "Side of the frame where the agent log window is displayed."
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Right" right)
                 (const :tag "Left" left)
                 (const :tag "Top" top))
  :group 'ekg-agent)

(defcustom ekg-agent-log-window-size 12
  "Size of the agent log window (lines for top/bottom, columns for left/right)."
  :type 'integer
  :group 'ekg-agent)

(defcustom ekg-agent-timeout-seconds -1
  "Maximum time in seconds for an agent run before it is stopped.

If non-positive, no timeout is applied."
  :type 'integer
  :group 'ekg-agent)

(defcustom ekg-agent-status-reminder-seconds 50
  "Maximum quiet time before the agent is reminded to summarize state.

When positive, each new LLM turn checks how long it has been since the
last explicit `summarize_state' call.  If this threshold has been
exceeded, the prompt is augmented with a reminder to summarize progress
before doing more work."
  :type 'number
  :group 'ekg-agent)

(defcustom ekg-agent-llm-max-retries 3
  "Maximum number of retries for transient LLM errors.
Transient errors include overloaded servers, rate limiting, and
temporary network failures.  Set to 0 to disable retries."
  :type 'integer
  :group 'ekg-agent)

(defcustom ekg-agent-llm-max-tokens nil
  "Maximum output tokens for agent LLM requests.
When nil, leave the provider default unchanged."
  :type '(choice (const :tag "Provider default" nil)
                 (integer :tag "Maximum output tokens"))
  :group 'ekg-agent)

(defcustom ekg-agent-llm-retry-base-delay 2
  "Base delay in seconds between LLM retry attempts.
The actual delay uses exponential backoff: base * 2^attempt."
  :type 'number
  :group 'ekg-agent)

(defcustom ekg-agent-code-command nil
  "Command to run for the coding tool.

This should be a shell-style command string (for example, \"claude -p
--dangerously-skip-permissions\").  The prompt is passed according to
`ekg-agent-code-command-prompt-method'; stdout will be returned."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'ekg-agent)

(defcustom ekg-agent-code-command-prompt-method 'stdin
  "How `ekg-agent-code-command' receives the prompt.

When set to `stdin', send the prompt on standard input.  This is
the default because it avoids exposing prompt contents in process
listings.  When set to `argument', append the prompt as the final
command-line argument for tools that require that interface."
  :type '(choice (const :tag "Send prompt on standard input" stdin)
                 (const :tag "Append prompt as command argument" argument))
  :group 'ekg-agent)

(defvar-local ekg-agent--prompt nil
  "The prompt for the agent section in the current buffer.")

(defvar-local ekg-agent--end-tools nil
  "The end tools for the current agent run in this buffer.")

(defvar-local ekg-agent--running-p nil
  "Non-nil when the agent is actively running in this buffer.")

(defvar-local ekg-agent--cancelled-p nil
  "Non-nil when the user has requested cancellation of the agent.")

(defvar-local ekg-agent--current-request nil
  "The in-flight LLM request handle, for cancellation.")

(defvar-local ekg-agent--tool-processes nil
  "List of active tool subprocesses, for force cancellation.")

(defvar-local ekg-agent--tool-call-history nil
  "Successful tool calls in the current agent run, newest first.
Each entry is a plist with :name, :args, :result, and :time.")

(defvar-local ekg-agent--status-callback nil
  "The status callback for the current agent run.
Stored so that `ekg-agent-force-cancel' can invoke it.")

(defvar-local ekg-agent--current-activity nil
  "Short description of what the agent is currently doing.
Shown in the header line; updated by tool wrappers and the iterate loop.")

(defvar-local ekg-agent--last-status-update-time nil
  "Float time when the last explicit `summarize_state' call was logged.")

(defvar-local ekg-agent--last-status-reminder-time nil
  "Float time when the agent was last reminded to summarize state.")

(defvar-local ekg-agent--origin-buffer nil
  "The buffer from which the agent was originally invoked.
Tool functions execute in this buffer so that `major-mode',
`default-directory', and other buffer-local state are correct.")

(defvar ekg-agent--current-log-buffer nil
  "Dynamically bound to the current agent log buffer.
Logging functions consult this variable instead of relying on
`current-buffer', so that logging always targets the log buffer
even when tool functions execute in the origin buffer.")

(defvar ekg-agent--daily-timer nil
  "Timer object for the daily agent evaluation.")

(defface ekg-agent-log-timestamp
  '((t :inherit shadow))
  "Face for timestamps in agent log."
  :group 'ekg-agent)

(defface ekg-agent-log-started
  '((((background light))
     :background "#fff3cd" :foreground "#664d03" :weight bold
     :box (:line-width (-1 . -1)))
    (((background dark))
     :background "#664d03" :foreground "#fff3cd" :weight bold
     :box (:line-width (-1 . -1)))
    (t :inherit warning :weight bold :box (:line-width (-1 . -1))))
  "Face for STARTED status in agent log tool lines."
  :group 'ekg-agent)

(defface ekg-agent-log-done
  '((((background light))
     :background "#d1e7dd" :foreground "#0a3622" :weight bold
     :box (:line-width (-1 . -1)))
    (((background dark))
     :background "#0a3622" :foreground "#d1e7dd" :weight bold
     :box (:line-width (-1 . -1)))
    (t :inherit success :weight bold :box (:line-width (-1 . -1))))
  "Face for DONE status in agent log tool lines."
  :group 'ekg-agent)

(defface ekg-agent-log-tool-name
  '((t :weight bold))
  "Face for tool names in agent log."
  :group 'ekg-agent)

(defface ekg-agent-log-header-running
  '((t :inherit warning :inverse-video t :weight bold))
  "Face for agent log header line when the agent is running."
  :group 'ekg-agent)

(defface ekg-agent-log-header-finished
  '((t :inherit success :inverse-video t :weight bold))
  "Face for agent log header line when the agent has finished."
  :group 'ekg-agent)

(defun ekg-agent--format-header-line ()
  "Return the formatted header line string for the agent log buffer."
  (let* ((running ekg-agent--running-p)
         (text (cond
                ((not running) " ✓ Agent Finished")
                (ekg-agent--current-activity
                 (format " ⟳ %s" ekg-agent--current-activity))
                (t " ⟳ Agent Running")))
         (face (if running 'ekg-agent-log-header-running
                 'ekg-agent-log-header-finished))
         (width (max (length text) (window-width))))
    (propertize (concat text (make-string (- width (length text)) ?\s))
                'face face)))

(defun ekg-agent--provider ()
  "Return the provider for the LLM.

If there is a list of providers, find the first one that has tool
calling, or if none have tool calling, just return the first provider."
  (if (listp ekg-llm-provider)
    (or (car (seq-filter (lambda (provider) (member 'tool-use (llm-capabilities provider)))
                         ekg-llm-provider))
        (car ekg-llm-provider))
    ekg-llm-provider))

(defun ekg-agent--read-provider-for-prefix (arg)
  "Read an LLM provider when ARG is non-nil and providers are configured.
Return nil when ARG is nil or `ekg-llm-provider' is not a non-empty list."
  (when (and arg (consp ekg-llm-provider))
    (let ((provider-alist (mapcar (lambda (provider)
                                    (cons (llm-name provider)
                                          provider))
                                  ekg-llm-provider)))
      (assoc-default (completing-read "Provider: "
                                      provider-alist
                                      nil t)
                     provider-alist))))

(defun ekg-agent--transient-error-p (err-string)
  "Return non-nil if ERR-STRING indicates a transient LLM error.
Transient errors are those likely to succeed on retry, such as
overloaded servers, rate limits, and temporary network failures."
  (and (stringp err-string)
       (let ((lower (downcase err-string)))
         (or (string-match-p "overloaded" lower)
             (string-match-p "rate.limit" lower)
             (string-match-p "too many requests" lower)
             (string-match-p "timed? out" lower)
             (string-match-p "timeout" lower)
             (string-match-p "429" lower)
             (string-match-p "529" lower)
             (string-match-p "503" lower)
             (string-match-p "server.*error" lower)
             (string-match-p "temporarily unavailable" lower)))))

(defun ekg-agent--llm-chat-async-with-retry (provider prompt on-success on-error
                                                      multi-output log-buf)
  "Call `llm-chat-async' with automatic retry on transient errors.
PROVIDER, PROMPT, ON-SUCCESS, ON-ERROR, and MULTI-OUTPUT are as
for `llm-chat-async'.  LOG-BUF is the agent log buffer, used for
logging retries and checking cancellation.  Returns the request
handle from the current attempt."
  (let ((attempt 0)
        (max-retries ekg-agent-llm-max-retries)
        (base-delay ekg-agent-llm-retry-base-delay))
    (cl-labels
        ((try ()
           (llm-chat-async
            provider prompt on-success
            (lambda (type err)
              (if (and (< attempt max-retries)
                       (ekg-agent--transient-error-p (format "%s" err))
                       (buffer-live-p log-buf)
                       (not (buffer-local-value
                             'ekg-agent--cancelled-p log-buf)))
                  (let* ((delay (* base-delay (expt 2 attempt)))
                         (ekg-agent--current-log-buffer log-buf))
                    (cl-incf attempt)
                    (ekg-agent--log
                     "Transient error (attempt %d/%d): %s — retrying in %ds…"
                     attempt (1+ max-retries) err delay)
                    (run-at-time delay nil
                                 (lambda ()
                                   (when (and (buffer-live-p log-buf)
                                              (not (buffer-local-value
                                                    'ekg-agent--cancelled-p
                                                    log-buf)))
                                     (let ((req (try)))
                                       (when (buffer-live-p log-buf)
                                         (with-current-buffer log-buf
                                           (setq ekg-agent--current-request
                                                 req))))))))
                (funcall on-error type err)))
            multi-output)))
      (try))))

(defmacro ekg-agent--with-error-as-text (&rest body)
  "Execute BODY, returning any error as a descriptive string.
If BODY signals an error, return \"Error: MESSAGE\" instead of
propagating the signal.  This is used in tool functions so that
the LLM sees the error and can react, rather than aborting the
tool call."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (error (format "Error: %s" (error-message-string err)))))

(defconst ekg-agent-tool-all-tags
  (make-llm-tool :function (lambda (tags num)
                             (ekg-agent--with-error-as-text
                               (let* ((ekg-llm-note-numwords 500)
                                      (tag-list (append tags nil))
                                      (notes (ekg-agent--get-notes
                                              :tags (or tag-list nil)
                                              :latest (null tag-list)
                                              :num num)))
                                 (if notes
                                     (mapconcat #'ekg-llm-note-to-text notes "\n\n")
                                   "No notes found."))))
                 :name "get_notes_with_all_tags"
                 :description "Retrieve notes that have all the specified tags.  Results are returned newest first."
                 :args '((:name "tags" :type array :items (:type string) :description "List of tags to filter notes by.  Each tag may have spaces or non-alphanumeric characters.  May be empty to retrieve the latest notes.")
                         (:name "num" :type integer :description "Maximum number of notes to retrieve."))))

(defconst ekg-agent-tool-any-tags
  (make-llm-tool :function (lambda (tags num)
                             (ekg-agent--with-error-as-text
                               (let* ((ekg-llm-note-numwords 500)
                                      (tag-list (append tags nil))
                                      (notes (ekg-agent--get-notes
                                              :any-tags (or tag-list nil)
                                              :latest (null tag-list)
                                              :num num)))
                                 (if notes
                                     (mapconcat #'ekg-llm-note-to-text notes "\n\n")
                                   "No notes found."))))
                 :name "get_notes_with_any_tags"
                 :description "Retrieve notes that have any of the specified tags.  Results are returned newest first."
                 :args '((:name "tags" :type array :items (:type string) :description "List of tags to filter notes by.  Each tag may have spaces or non-alphanumeric characters.  May be empty to retrieve the latest notes.")
                         (:name "num" :type integer :description "Maximum number of notes to retrieve."))))

(defconst ekg-agent-tool-get-note-by-id
  (make-llm-tool :function (lambda (id)
                             (ekg-agent--with-error-as-text
                               (let ((notes (ekg-agent--get-notes :note-id id)))
                                 (if notes
                                     (ekg-llm-note-to-text (car notes))
                                   (error "Note with ID %s not found" id)))))
                 :name "get_note_by_id"
                 :description "Retrieve a note by its unique identifier, such as a concrete note ID or file:<absolute-path>. This lookup is for identifiers, not tags."
                 :args '((:name "id" :type string :description "The unique identifier of the note."))))

(defconst ekg-agent-tool-search-notes
  (make-llm-tool :function (lambda (query num)
                             (ekg-agent--with-error-as-text
                               (let ((ekg-llm-note-numwords 500)
                                     (notes (ekg-agent--get-notes :semantic-search query :num num)))
                                 (if notes
                                     (mapconcat #'ekg-llm-note-to-text notes "\n\n")
                                   "No notes found matching that search."))))
                 :name "search_notes"
                 :description "Search notes by a query string, retrieving by semantic similarity."
                 :args '((:name "query" :type string :description "The search query string.")
                         (:name "num" :type integer :description "Maximum number of notes to retrieve."))))

(defconst ekg-agent-tool-ask-user
  (make-llm-tool :function (lambda (question)
                             (ekg-agent--with-error-as-text
                               (read-string (format "Question from ekg-agent: %s\nResponse: " question))))
                 :name "ask_user"
                 :description "Ask the user a question and get their response."
                 :args '((:name "question" :type string :description "The question to ask the user."))))

(defconst ekg-agent-tool-list-tags
  (make-llm-tool :function (lambda (&optional regex-filter)
                             (ekg-agent--with-error-as-text
                               (let* ((tags (ekg-tags))
                                      (filtered (if regex-filter
                                                    (seq-filter (lambda (tag)
                                                                  (string-match-p regex-filter tag))
                                                                tags)
                                                  tags)))
                                 (if filtered
                                     (mapconcat #'identity filtered "\n")
                                   "No tags found."))))
                 :name "list_all_tags"
                 :description "List all existing tags in the ekg database."
                 :args '((:name "regex_filter" :type string :description "Optional regex to filter the tags by"))))

(defun ekg-agent--get-note-with-id (id)
  "Retrieve the note with string ID, handling different ID types.

This tries a few different things, since ekg ids can be of various
types, but we'll only get strings from the LLM."
  (or (ekg-get-note-with-id id)
      (let ((int-id (string-to-number id)))
        (when (> int-id 0)
          (ekg-get-note-with-id int-id)))
      (ekg-get-note-with-id (intern id))))

(defconst ekg-agent-tool-append-to-note
  (make-llm-tool :function (lambda (id content)
                             (ekg-agent--with-error-as-text
                               (let ((note (ekg-agent--get-note-with-id id)))
                                 (unless note
                                   (error "Note with ID %s not found" id))
                                 (let* ((enclosure (assoc-default (ekg-note-mode note) ekg-llm-format-output nil '("_BEGIN_" . "_END_")))
                                        (new-text (concat (ekg-note-text note) "\n"
                                                          (car enclosure) "\n"
                                                          content "\n"
                                                          (cdr enclosure))))
                                   (setf (ekg-note-text note) new-text)
                                   (ekg-save-note note)
                                   (format "Appended content to note ID %s" id)))))
                 :name "append_to_note"
                 :description "Append content to an existing note by its ID."
                 :args '((:name "id" :type string :description "The unique identifier of the note.")
                         (:name "content" :type string :description "The content to append to the note."))))

(defconst ekg-agent-tool-replace-note
  (make-llm-tool :function (lambda (id content)
                             (ekg-agent--with-error-as-text
                               (let ((note (ekg-agent--get-note-with-id id)))
                                 (unless note
                                   (error "Note with ID %s not found" id))
                                 (let* ((enclosure (assoc-default (ekg-note-mode note) ekg-llm-format-output nil '("_BEGIN_" . "_END_")))
                                        (new-text (concat (car enclosure) "\n"
                                                          content "\n"
                                                          (cdr enclosure))))
                                   (setf (ekg-note-text note) new-text)
                                   (ekg-save-note note)
                                   (format "Replaced content of note ID %s" id)))))
                 :name "replace_note"
                 :description "Replace the content of an existing note by its ID."
                 :args '((:name "id" :type string :description "The unique identifier of the note.")
                         (:name "content" :type string :description "The new content for the note."))))

(defun ekg-agent--org-todo-keyword-name (keyword)
  "Return the bare Org TODO keyword name from KEYWORD.
This strips fast-selection and logging specs such as \"TODO(t)\"
or \"WAIT(w@/!)\"."
  (when (and (stringp keyword)
             (not (string= keyword "|"))
             (string-match "\\`\\([^[:space:]()]+\\)" keyword))
    (match-string 1 keyword)))

(defun ekg-agent--org-todo-keywords ()
  "Return bare TODO keyword names from `org-todo-keywords'."
  (let (keywords)
    (dolist (entry org-todo-keywords)
      (cond
       ((stringp entry)
        (when-let* ((keyword (ekg-agent--org-todo-keyword-name entry)))
          (push keyword keywords)))
       ((consp entry)
        (dolist (item (cdr entry))
          (when-let* ((keyword (ekg-agent--org-todo-keyword-name item)))
            (push keyword keywords))))))
    (delete-dups (nreverse keywords))))

(defun ekg-agent--org-default-todo-keyword ()
  "Return the default TODO keyword for new org task items."
  (or (car (ekg-agent--org-todo-keywords))
      "TODO"))

(defun ekg-agent--org-todo-keyword-regexp ()
  "Return a regexp matching any configured Org TODO keyword."
  (when-let* ((keywords (ekg-agent--org-todo-keywords)))
    (regexp-opt keywords)))

(defconst ekg-agent-tool-create-note
  (make-llm-tool :function (lambda (tags content mode)
                             (ekg-agent--with-error-as-text
                               (unless (member mode '("org-mode" "markdown-mode" "text-mode"))
                                 (error "Unsupported mode: %s" mode))
                               ;; Use ekg-agent-add-note for consistency (includes auto-tags)
                               (format "Created note with ID %s"
                                       (ekg-agent-add-note
                                        content (append tags nil) mode))))
                 :name "create_note"
                 :description "Create a new note with specified tags and content."
                 :args `((:name "tags" :type array :items (:type string)
                                :description "List of tags to assign to the new note.  The tags should preferably be preexisting tags, but new tags can be created as well.")
                         (:name "content"
                                :type string
                                :description "The content of the new note.  This must be written in the format specified by the 'mode' parameter.  Do not include the tags in the content; they will be added automatically.")
                         (:name "mode" :type string :enum ["markdown-mode" "org-mode" "text-mode"]
                                :description
                                ,(concat "The emacs mode of the note content (e.g., 'org-mode', 'markdown-mode', 'text-mode'). "
                                         "Prefer org-mode to markdown-mode, and either to text-mode.  Use existing notes as "
                                         "a guide to what the user prefers.")))))

(defconst ekg-agent-tool-end
  (make-llm-tool :function (lambda () 'done)
                 :name "end"
                 :description "End the session."
                 :args '()))

(defconst ekg-agent-tool-subagent-end
  (make-llm-tool :function #'identity
                 :name "subagent_end"
                 :description "Indicate that a sub-agent has completed its task and is returning control to the parent agent.  The return value will be passed back to the parent agent as the result of the sub-agent's `run_subagent` call."
                 :args '((:name "result" :type string :description "The result to return to the parent agent."))))

(defun ekg-agent--popup-result-in-buffer (result)
  "Display RESULT in a new buffer and pop up to it."
  (ekg-agent--with-error-as-text
    (let ((buf (get-buffer-create "*ekg agent result*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert result)
        (when (featurep 'markdown-mode)
          (markdown-mode)
          (when (featurep 'flycheck)
            (flycheck-mode 0))
          (when (featurep 'flymake)
            (flymake-mode 0)))
        (goto-char (point-min)))
      (pop-to-buffer buf)
      (format "Popup displayed in buffer %s" (buffer-name)))))

(defconst ekg-agent-tool-popup-result
  (make-llm-tool :function #'ekg-agent--popup-result-in-buffer
                 :name "display_result_in_popup"
                 :description "Display a user-visible result in a popup buffer."
                 :args '((:name "result" :type string :description "The result to display."))))

(defun ekg-agent--run-code (callback prompt)
  "Run the configured `ekg-agent-code-command' asynchronously with PROMPT.
CALLBACK is called with the result string when the process finishes."
  (condition-case err
      (progn
        (unless (and (stringp ekg-agent-code-command)
                     (string-match-p "\\S-" ekg-agent-code-command))
          (error "Ekg-agent-code-command is not configured"))
        (let* ((args (split-string-and-unquote ekg-agent-code-command))
               (program (car args))
               (program-args (if (eq ekg-agent-code-command-prompt-method
                                     'argument)
                                 (append (cdr args) (list prompt))
                               (cdr args)))
               (output-buf (generate-new-buffer " *ekg-agent-code*" t))
               (proc (make-process
                      :name "ekg-agent-code"
                      :buffer output-buf
                      :connection-type 'pipe
                      :command (cons program program-args)
                      :sentinel (lambda (process _event)
                                  (when (memq (process-status process) '(exit signal))
                                    (let* ((exit-code (process-exit-status process))
                                           (output (with-current-buffer (process-buffer process)
                                                     (buffer-string))))
                                      (kill-buffer (process-buffer process))
                                      (funcall callback
                                               (if (zerop exit-code)
                                                   (string-trim-right output)
                                                 (format "Error: Command failed (%d): %s"
                                                         exit-code
                                                         (string-trim-right output))))))))))
          (push proc ekg-agent--tool-processes)
          (when (eq ekg-agent-code-command-prompt-method 'stdin)
            (process-send-string proc prompt))
          (process-send-eof proc)))
    (error (funcall callback (format "Error: %s" (error-message-string err))))))

(defconst ekg-agent-tool-run-elisp
  (make-llm-tool :function (lambda (callback elisp return)
                             (condition-case err
                                 (let* ((output-buf (generate-new-buffer " *ekg-agent-elisp*" t))
                                        (eval-expr
                                         (prin1-to-string
                                          `(princ
                                            (let* (e
                                                   (result
                                                    (condition-case err
                                                        (eval (read ,elisp))
                                                      (error (setq e (format "%S" err))))))
                                              (or e
                                                  (if (equal ,return "result")
                                                      (format "%S" result)
                                                    (buffer-substring-no-properties
                                                     (point-min) (point-max))))))))
                                        (f (futur-process-call
                                            (expand-file-name invocation-name
                                                              invocation-directory)
                                            nil output-buf nil
                                            "--batch" "--eval" eval-expr)))
                                   (push f ekg-agent--tool-processes)
                                   (futur-bind
                                    f
                                    (lambda (exit-code)
                                      (let ((output (with-current-buffer output-buf
                                                      (buffer-string))))
                                        (kill-buffer output-buf)
                                        (run-at-time
                                         0 nil
                                         (lambda ()
                                           (funcall callback
                                                    (if (zerop exit-code)
                                                        output
                                                      (format "Error: Process exited with %d: %s"
                                                              exit-code output)))))
                                        nil))
                                    (lambda (err)
                                      (when (buffer-live-p output-buf)
                                        (kill-buffer output-buf))
                                      (run-at-time
                                       0 nil
                                       (lambda ()
                                         (funcall callback (format "Error: %S" err))))
                                      nil)))
                               (error
                                (funcall callback (format "Error: %s" (error-message-string err))))))
                 :name "run_elisp"
                 :description "Evaluate arbitrary Emacs Lisp and return the printed result of the final form.  Use this to test out elisp and for Emacs state changes such as adding to `load-path', requiring libraries, setting variables, or calling Emacs functions.  If a form returns `void-function' or `file-missing', diagnose with checks such as `fboundp', `featurep', `locate-library', package directories, and `load-path' before retrying the same form.  Do not use this when another tool is more appropriate.  If that other tool is giving errors, report that to the user instead."
                 :args '((:name "elisp" :type string :description "The Emacs Lisp code to evaluate." :required t)
                         (:name "return" :type string :enum ["result" "buffer"]
                                :description "Whether to return the result of the evaluated elisp, or the buffer after the elisp has been evaluated. If there is an error, it will be returned regardless of this value."
                                :required t))
                 :async t))

(defconst ekg-agent-tool-code
  (make-llm-tool :function #'ekg-agent--run-code
                 :name "run_code_tool"
                 :description "Run the configured external coding command with a prompt and return its result."
                 :args '((:name "prompt" :type string :description "The prompt to pass to the tool."))
                 :async t))

(defun ekg-agent--tools (extra-tools)
  "Return the list of tools available to the agent.
EXTRA-TOOLS is a list of additional tools beyond
`ekg-agent-base-tools' and `ekg-agent-extra-tools' to include."
  (seq-uniq
   (seq-remove
    (lambda (tool)
      (and (string= (llm-tool-name tool) "search_notes")
           (not ekg-embedding-provider)))
    (append ekg-agent-base-tools
            ekg-agent-extra-tools
            extra-tools))
   (lambda (a b) (string-equal (llm-tool-name a) (llm-tool-name b)))))

(defconst ekg-agent-tool-subagent
  (make-llm-tool
   :function (lambda (callback instructions)
               (ekg-agent--log "Sub-agent started: %s"
                               (truncate-string-to-width instructions 80))
               (let ((prompt (llm-make-chat-prompt
                              instructions
                              :context (concat
                                        (ekg-agent-instructions-intro)
                                        "\n\nYou are a sub-agent working on a specific task. "
                                        "Complete the given task using the tools available to you. "
                                        "When you are done, call the subagent_end tool.\n\n"
                                        (format "The current date and time is %s."
                                                (format-time-string "%F %R")))
                              :tools (ekg-agent--tools (list ekg-agent-tool-subagent-end))
                              :tool-options (make-llm-tool-options :tool-choice 'any))))
                 (ekg-agent--iterate prompt
                                     0
                                     (lambda (status)
                                       (funcall callback
                                                (format "Subagent result: %s" status)))
                                     '("subagent_end")
                                     (ekg-agent--timeout-deadline))))
   :name "run_subagent"
   :description "Run a sub-agent with the given instructions and return its result."
   :args '((:name "instructions" :type string :description "Detailed instructions for the sub-agent to follow."))
   :async t))

(defun ekg-agent--ensure-log-window ()
  "Ensure the agent log buffer is displayed in a side window.
`ekg-agent--current-log-buffer' must be bound to the log buffer."
  (let* ((buf ekg-agent--current-log-buffer)
         (params (append
                  `((side . ,ekg-agent-log-window-side)
                    (slot . 1))
                  (if (memq ekg-agent-log-window-side '(left right))
                      `((window-width . ,ekg-agent-log-window-size))
                    `((window-height . ,ekg-agent-log-window-size))))))
    (let ((win (display-buffer-in-side-window buf params)))
      (set-window-dedicated-p win t)
      win)))

(defun ekg-agent--log-raw (line)
  "Append LINE to the agent log buffer without a timestamp.
`ekg-agent--current-log-buffer' must be bound to the log buffer."
  (when-let ((buf ekg-agent--current-log-buffer))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert line "\n"))
        (let ((win (get-buffer-window buf t)))
          (when win
            (set-window-point win (point-max))))))))

(defun ekg-agent--log (format-string &rest args)
  "Append a log line built from FORMAT-STRING and ARGS to the agent log buffer.
`ekg-agent--current-log-buffer' must be bound to the log buffer."
  (when-let ((buf ekg-agent--current-log-buffer))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format-time-string "%F %T "))
          (insert (apply #'format format-string args))
          (insert "\n"))
        (let ((win (get-buffer-window buf t)))
          (when win
            (set-window-point win (point-max))))))))

(defun ekg-agent--log-session-start (label &optional detail)
  "Start a new session in the agent log with LABEL and optional DETAIL."
  (ekg-agent--ensure-log-window)
  (ekg-agent--log-raw (make-string 72 ?-))
  (ekg-agent--log "`%s`%s" label (if detail (format ": %s" detail) "")))

(defconst ekg-agent--status-width 7
  "Character width of status labels in tool log lines.
Both \"STARTED\" and \" DONE  \" are this width for in-place replacement.")

(defun ekg-agent--log-tool-start (log-buf tool-name)
  "Log that TOOL-NAME has started in LOG-BUF.
Returns a marker at the status text position, used by
`ekg-agent--log-tool-done' to update the line in place."
  (when (and log-buf (buffer-live-p log-buf))
    (with-current-buffer log-buf
      (setq ekg-agent--current-activity (format "Running: %s" tool-name))
      (force-mode-line-update)
      (let ((inhibit-read-only t)
            (marker (make-marker)))
        (goto-char (point-max))
        (insert (propertize (format-time-string "%F %T")
                            'face 'ekg-agent-log-timestamp))
        (insert " ")
        (set-marker marker (point))
        (insert (propertize "STARTED" 'face 'ekg-agent-log-started))
        (insert " ")
        (insert (propertize tool-name 'face 'ekg-agent-log-tool-name))
        (insert "\n")
        (let ((win (get-buffer-window log-buf t)))
          (when win
            (set-window-point win (point-max))))
        marker))))

(defun ekg-agent--log-tool-done (log-buf marker)
  "Update the tool log line at MARKER in LOG-BUF to show done.
Replaces the STARTED status text with DONE, keeping the rest of
the line intact."
  (when (and log-buf (buffer-live-p log-buf) marker (marker-buffer marker))
    (with-current-buffer log-buf
      (setq ekg-agent--current-activity nil)
      (force-mode-line-update)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (delete-char ekg-agent--status-width)
          (insert (propertize " DONE  " 'face 'ekg-agent-log-done)))))))

(defun ekg-agent--sanitize-tool-result (result)
  "Ensure RESULT is safe to include in a JSON-serialized LLM request.
Strips Emacs raw-byte characters (U+3FFF80 and above) that cause
`json-serialize' to fail, and converts non-string results to
strings."
  (let ((s (if (stringp result)
               result
             (format "%s" result))))
    (replace-regexp-in-string "[\x3fff80-\x3fffff]" "" s)))

(defun ekg-agent--log-tool-error-result (tool-name result &optional args)
  "Log RESULT and ARGS for TOOL-NAME when it is a tool-level error."
  (when (and (stringp result)
             (string-prefix-p "Error:" result))
    (ekg-agent--log "Tool %s returned %s" tool-name result)
    (when args
      (ekg-agent--log "Tool %s args: %S" tool-name args))))

(defun ekg-agent--last-tool-name ()
  "Return the most recently completed successful tool name."
  (plist-get (car ekg-agent--tool-call-history) :name))

(defun ekg-agent--tool-called-p (tool-name)
  "Return non-nil if TOOL-NAME completed successfully in this run."
  (cl-some (lambda (call)
             (string= (plist-get call :name) tool-name))
           ekg-agent--tool-call-history))

(defun ekg-agent--any-tool-called-p (tool-names)
  "Return non-nil if any tool in TOOL-NAMES completed successfully."
  (cl-some #'ekg-agent--tool-called-p tool-names))

(defun ekg-agent--tool-available-p (tool-name)
  "Return non-nil if TOOL-NAME is available in the current prompt."
  (and ekg-agent--prompt
       (cl-some (lambda (tool)
                  (string= (llm-tool-name tool) tool-name))
                (llm-chat-prompt-tools ekg-agent--prompt))))

(defun ekg-agent--any-tool-available-p (tool-names)
  "Return non-nil if any tool in TOOL-NAMES is available."
  (cl-some #'ekg-agent--tool-available-p tool-names))

(defconst ekg-agent--repeatable-read-only-tools
  '("get_notes_with_all_tags"
    "get_notes_with_any_tags"
    "get_note_by_id"
    "search_notes"
    "list_all_tags"
    "read_agents_md"
    "read_file"
    "list_buffers"
    "read_buffer"
    "list_org_items")
  "Tool names whose identical repeated calls can reuse prior context.")

(defconst ekg-agent--state-changing-tools
  '("append_to_note"
    "replace_note"
    "create_note"
    "run_elisp"
    "run_code_tool"
    "run_subagent"
    "edit_file"
    "write_file"
    "edit_buffer"
    "run_interactive_command"
    "run_command"
    "append_to_current_note"
    "replace_current_note"
    "add_org_item"
    "set_org_item_status")
  "Tool names that may change file, buffer, note, or task state.")

(defun ekg-agent--repeat-read-only-result (log-buf tool-name args)
  "Return a repeat result in LOG-BUF for read-only TOOL-NAME ARGS.
Only repeats before any intervening state-changing tool are
short-circuited; after a write or note mutation, the read is run
normally."
  (when (and (member tool-name ekg-agent--repeatable-read-only-tools)
             (buffer-live-p log-buf))
    (with-current-buffer log-buf
      (cl-loop for call in ekg-agent--tool-call-history
               for call-name = (plist-get call :name)
               if (member call-name ekg-agent--state-changing-tools)
               return nil
               if (and (string= call-name tool-name)
                       (equal (plist-get call :args) args))
               return
               (format
                "This exact %s call already completed earlier in this run. Use the earlier result, or call a lookup with different arguments if you need different information."
                tool-name)))))

(defun ekg-agent--record-tool-completion (log-buf tool-name result &optional args)
  "Record TOOL-NAME with ARGS as complete in LOG-BUF unless RESULT is an error."
  (when (and (buffer-live-p log-buf)
             (stringp result)
             (not (string-prefix-p "Error:" result)))
    (with-current-buffer log-buf
      (push (list :name tool-name
                  :args args
                  :result result
                  :time (float-time))
            ekg-agent--tool-call-history))))

(defun ekg-agent--repeated-status-tool-p (log-buf tool-name)
  "Return non-nil if TOOL-NAME is a repeated status update in LOG-BUF."
  (and (string= tool-name "summarize_state")
       (buffer-live-p log-buf)
       (with-current-buffer log-buf
         (equal (ekg-agent--last-tool-name) "summarize_state"))))

(defun ekg-agent--blocked-tool-error (log-buf tool-name &optional _args)
  "Return an error string if TOOL-NAME should be blocked in LOG-BUF."
  (cond
   ((ekg-agent--repeated-status-tool-p log-buf tool-name)
    "summarize_state was already the previous successful tool; do substantive work with another tool before summarizing again")
   (t nil)))

(defun ekg-agent--wrap-tool-function (tool log-buf origin-buf)
  "Return a copy of TOOL with its function wrapped for resilience.

LOG-BUF is the buffer to log to.  ORIGIN-BUF is the buffer from
which the agent was invoked; tool functions execute there so that
buffer-local state such as `major-mode' and `default-directory'
are correct.

The wrapper provides four layers of protection:

1. ORIGIN BUFFER: Tool functions run in ORIGIN-BUF while
   `ekg-agent--current-log-buffer' is dynamically bound to LOG-BUF,
   so that any logging from within the tool targets the log buffer.

2. LOGGING: Logs STARTED when the tool is called and DONE when it
   returns (sync) or calls back (async).

3. ONCE-ONLY CALLBACK: For async tools, the callback is guarded so
   it can only fire once.  If a tool\\'s error handling accidentally
   calls the callback a second time, it is silently ignored.

4. RESULT SANITIZATION: All tool results are passed through
   `ekg-agent--sanitize-tool-result' to strip characters that would
   cause `json-serialize' to fail downstream."
  (let* ((original-fn (llm-tool-function tool))
         (tool-name (llm-tool-name tool))
         (async-p (llm-tool-async tool)))
    (make-llm-tool
     :function (if async-p
                   (lambda (callback &rest args)
                     (let ((marker (ekg-agent--log-tool-start log-buf tool-name))
                           (called nil)
                           (ekg-agent--current-log-buffer log-buf))
                       (if-let ((repeat-result
                                 (ekg-agent--repeat-read-only-result
                                  log-buf tool-name args)))
                           (progn
                             (setq called t)
                             (ekg-agent--log-tool-done log-buf marker)
                             (ekg-agent--record-tool-completion
                              log-buf tool-name repeat-result args)
                             (funcall callback repeat-result))
                         (condition-case err
                             (if-let ((blocked
                                       (ekg-agent--blocked-tool-error
                                        log-buf tool-name args)))
                                 (error "%s" blocked)
                               (if (buffer-live-p origin-buf)
                                   (with-current-buffer origin-buf
                                     (apply original-fn
                                            (lambda (result)
                                              (unless called
                                                (setq called t)
                                                (let ((sanitized
                                                       (ekg-agent--sanitize-tool-result result)))
                                                  (ekg-agent--log-tool-done log-buf marker)
                                                  (ekg-agent--log-tool-error-result
                                                   tool-name sanitized args)
                                                  (ekg-agent--record-tool-completion
                                                   log-buf tool-name sanitized args)
                                                  (funcall callback sanitized))))
                                            args))
                                 (apply original-fn
                                        (lambda (result)
                                          (unless called
                                            (setq called t)
                                            (let ((sanitized
                                                   (ekg-agent--sanitize-tool-result result)))
                                              (ekg-agent--log-tool-done log-buf marker)
                                              (ekg-agent--log-tool-error-result
                                               tool-name sanitized args)
                                              (ekg-agent--record-tool-completion
                                               log-buf tool-name sanitized args)
                                              (funcall callback sanitized))))
                                        args)))
                           (error
                            (unless called
                              (setq called t)
                              (let ((sanitized
                                     (format "Error: %s"
                                             (error-message-string err))))
                                (ekg-agent--log-tool-done log-buf marker)
                                (ekg-agent--log-tool-error-result
                                 tool-name sanitized args)
                                (ekg-agent--record-tool-completion
                                 log-buf tool-name sanitized args)
                                (funcall callback sanitized))))))))
                 (lambda (&rest args)
                   (let ((marker (ekg-agent--log-tool-start log-buf tool-name))
                         (ekg-agent--current-log-buffer log-buf))
                     (if-let ((repeat-result
                               (ekg-agent--repeat-read-only-result
                                log-buf tool-name args)))
                         (progn
                           (ekg-agent--log-tool-done log-buf marker)
                           (ekg-agent--record-tool-completion
                            log-buf tool-name repeat-result args)
                           repeat-result)
                       (condition-case err
                           (let ((result (if-let ((blocked
                                                   (ekg-agent--blocked-tool-error
                                                    log-buf tool-name args)))
                                             (error "%s" blocked)
                                           (if (buffer-live-p origin-buf)
                                               (with-current-buffer origin-buf
                                                 (apply original-fn args))
                                             (apply original-fn args)))))
                             (let ((sanitized
                                    (ekg-agent--sanitize-tool-result result)))
                               (ekg-agent--log-tool-done log-buf marker)
                               (ekg-agent--log-tool-error-result
                                tool-name sanitized args)
                               (ekg-agent--record-tool-completion
                                log-buf tool-name sanitized args)
                               sanitized))
                         (error
                          (let ((sanitized
                                 (format "Error: %s"
                                         (error-message-string err))))
                            (ekg-agent--log-tool-done log-buf marker)
                            (ekg-agent--log-tool-error-result
                             tool-name sanitized args)
                            (ekg-agent--record-tool-completion
                             log-buf tool-name sanitized args)
                            sanitized)))))))
     :name (llm-tool-name tool)
     :description (llm-tool-description tool)
     :args (llm-tool-args tool)
     :async async-p)))

(defun ekg-agent--wrap-prompt-tools (prompt log-buf origin-buf)
  "Wrap all tools on PROMPT for logging and origin-buffer execution.
LOG-BUF is the agent log buffer.  ORIGIN-BUF is the buffer from
which the agent was invoked; tool functions execute there."
  (setf (llm-chat-prompt-tools prompt)
        (mapcar (lambda (tool)
                  (ekg-agent--wrap-tool-function tool log-buf origin-buf))
                (llm-chat-prompt-tools prompt))))

(defun ekg-agent--log-status (status)
  "Log final STATUS from the agent."
  (if (eq status 'error)
      (ekg-agent--log "Error")
    (ekg-agent--log "Done: %s" status)))

(defun ekg-agent--make-status-callback (&optional extra-callback)
  "Return a status callback that logs, then call EXTRA-CALLBACK."
  (lambda (status)
    (ekg-agent--log-status status)
    (when extra-callback
      (funcall extra-callback status))))

(defun ekg-agent--timeout-deadline ()
  "Return the timeout deadline as a float, or nil if no timeout is set."
  (when (and (numberp ekg-agent-timeout-seconds)
             (> ekg-agent-timeout-seconds 0))
    (+ (float-time) ekg-agent-timeout-seconds)))

(defun ekg-agent--clean-orphaned-tool-interactions (prompt)
  "Remove a trailing assistant tool-use interaction from PROMPT.
When cancelling mid-tool-execution, the prompt may end with an
assistant interaction containing structured tool-use data but no
matching `tool-results' interaction.  LLM providers will reject
this, so remove it.  A tool-use interaction is detected by having
non-string content in the assistant role.

Return non-nil when an interaction was removed."
  (let ((interactions (llm-chat-prompt-interactions prompt)))
    (when interactions
      (let ((last-interaction (car (last interactions))))
        (when (and (eq (llm-chat-prompt-interaction-role last-interaction)
                       'assistant)
                   (not (stringp
                         (llm-chat-prompt-interaction-content
                          last-interaction))))
          (setf (llm-chat-prompt-interactions prompt)
                (butlast interactions))
          t)))))

(defun ekg-agent--unknown-tool-name (err)
  "Return the unavailable tool name from ERR, or nil."
  (let ((err-string (format "%s" err)))
    (when (string-match "Unknown tool [`']\\([^`']+\\)[`'] called" err-string)
      (match-string 1 err-string))))

(defun ekg-agent--available-tool-names (prompt)
  "Return available tool names for PROMPT."
  (sort (mapcar #'llm-tool-name (llm-chat-prompt-tools prompt))
        #'string<))

(defun ekg-agent--recover-unknown-tool-error (err prompt log-buf)
  "Recover from unknown tool ERR for PROMPT in LOG-BUF.
Return non-nil if the agent loop should continue."
  (let ((tool-name (ekg-agent--unknown-tool-name err)))
    (when tool-name
      (let ((removed (ekg-agent--clean-orphaned-tool-interactions prompt)))
        (ekg-agent--log "Unavailable tool requested: %s" tool-name)
        (ekg-agent--prompt-append-user-message
         prompt
         (format
          (concat "Your previous response called unavailable tool `%s'. "
                  "That tool is not available, so the invalid tool call was "
                  "%s. Continue by calling one of the available tools: "
                  "%s.")
          tool-name
          (if removed "discarded" "not added to the conversation")
          (string-join (ekg-agent--available-tool-names prompt) ", ")))
        (when (buffer-live-p log-buf)
          (not (buffer-local-value 'ekg-agent--cancelled-p log-buf)))))))

(defun ekg-agent--prompt-append-user-message (prompt message)
  "Append a user MESSAGE to PROMPT."
  (condition-case err
      (setf (llm-chat-prompt-interactions prompt)
            (append (llm-chat-prompt-interactions prompt)
                    (list (make-llm-chat-prompt-interaction
                           :role 'user
                           :content message))))
    (error
     (ekg-agent--log "Unable to append message to prompt: %s"
                     (error-message-string err)))))

(defun ekg-agent--timeout-warning-message ()
  "Return a message instructing the agent to save state before stopping."
  (format "Time limit reached. Before this session ends, immediately create a note with your current state using `create_note` (tag it with `%s`). Also call `summarize_state` with a brief update. Then finish by calling `end` or the appropriate completion tool."
          ekg-agent-self-info-tag))

(defun ekg-agent--record-status-update (&optional timestamp)
  "Record TIMESTAMP as the latest status update time for the log buffer."
  (when-let ((buf ekg-agent--current-log-buffer))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq ekg-agent--last-status-update-time
              (or timestamp (float-time)))
        (setq ekg-agent--last-status-reminder-time nil)))))

(defun ekg-agent--status-reminder-message ()
  "Return a prompt reminder asking for a meaningful status update."
  (concat
   "Before doing more work, call `summarize_state` with a brief but "
   "meaningful progress update. Include: (1) what you finished, (2) what "
   "you are doing now, and (3) what you will do next or what is blocked."))

(defun ekg-agent--maybe-remind-status-update (prompt)
  "Add a reminder to PROMPT when the agent owes the user a status update."
  (when-let ((buf ekg-agent--current-log-buffer))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((threshold ekg-agent-status-reminder-seconds)
              (last ekg-agent--last-status-update-time)
              (now (float-time)))
          (when (and ekg-agent--running-p
                     (numberp threshold)
                     (> threshold 0)
                     (numberp last)
                     (>= (- now last) threshold)
                     (or (null ekg-agent--last-status-reminder-time)
                         (< ekg-agent--last-status-reminder-time last)
                         (>= (- now ekg-agent--last-status-reminder-time)
                             threshold)))
            (setq ekg-agent--last-status-reminder-time now)
            (ekg-agent--prompt-append-user-message
             prompt
             (ekg-agent--status-reminder-message))
            t))))))

(defun ekg-agent--summarize-state (state)
  "Write STATE to the agent log window."
  (ekg-agent--with-error-as-text
    (ekg-agent--ensure-log-window)
    (ekg-agent--record-status-update)
    (ekg-agent--log "State: %s" state)
    "ok."))

(defconst ekg-agent-tool-summarize-state
  (make-llm-tool :function #'ekg-agent--summarize-state
                 :name "summarize_state"
                 :description "Record a concise progress update in the agent log window."
                 :args '((:name "state" :type string :description "Brief but meaningful summary of completed work, current step, and next step or blocker."))))

(defconst ekg-agent-tool-read-agents-md
  (make-llm-tool :function (lambda (dir)
                             (ekg-agent--with-error-as-text
                               (or (ekg-agent--read-agents-md dir)
                                   (format "No AGENTS.md found in %s" dir))))
                 :name "read_agents_md"
                 :description "Read the AGENTS.md file from a specified directory.  AGENTS.md files contain user instructions and preferences for agents."
                 :args '((:name "dir" :type string :description "The directory path to read AGENTS.md from."))))

(defun ekg-agent--line-id (path line-num)
  "Return a 3-char base64 identifier unique to PATH and LINE-NUM."
  (let* ((input (format "%s:%d" (file-truename path) line-num))
         (hash (secure-hash 'md5 input))
         ;; Take first 2 bytes of the hex hash (4 hex chars = 2 bytes),
         ;; base64-encode them to get a short identifier, then truncate
         ;; to 3 chars.  2 bytes → 4 base64 chars, we use the first 3.
         (raw (unibyte-string (string-to-number (substring hash 0 2) 16)
                              (string-to-number (substring hash 2 4) 16))))
    (substring (base64-encode-string raw t) 0 3)))

(defun ekg-agent--file-content (path)
  "Return the text content of the file at PATH as a property-free string.
Reads from an existing buffer if one is visiting PATH.
PATH should already be resolved via `file-truename'."
  (let ((buf (find-buffer-visiting path)))
    (if buf
        (with-current-buffer buf
          (substring-no-properties (buffer-string)))
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun ekg-agent--resolve-line-id (path id)
  "Resolve line identifier ID to a line number for the file at PATH."
  (let ((total (length (split-string (ekg-agent--file-content path) "\n"))))
    (or (cl-loop for i from 1 to total
                 when (string= (ekg-agent--line-id path i) id)
                 return i)
        (error "Line identifier %s not found in %s" id path))))

(defun ekg-agent--nonempty-string-p (s)
  "Return non-nil if S is a non-nil, non-empty string."
  (and (stringp s) (not (string-empty-p s))))

(defun ekg-agent--read-file (path &optional begin end range-type)
  "Read file at PATH, returning contents with line identifiers.

Each line is prefixed with a 3-char identifier derived from the
file path and line number.  If the file is visiting a buffer,
read from the buffer.

BEGIN and END restrict the output to a range.  RANGE-TYPE is
either \"line_number\" or \"identifier\" and indicates how to
interpret BEGIN and END.  Empty strings for BEGIN, END, or
RANGE-TYPE are treated as nil.  Returns a string without text
properties."
  (ekg-agent--with-error-as-text
    (let* ((truepath (file-truename path))
           ;; LLMs sometimes pass empty strings for omitted optional args.
           (begin (and (ekg-agent--nonempty-string-p begin) begin))
           (end (and (ekg-agent--nonempty-string-p end) end))
           (range-type (and (ekg-agent--nonempty-string-p range-type)
                            range-type)))
      (unless (or (find-buffer-visiting truepath) (file-exists-p truepath))
        (error "File not found: %s" path))
      (unless (or (find-buffer-visiting truepath) (file-readable-p truepath))
        (error "File not readable: %s" path))
      (let* ((lines (split-string (ekg-agent--file-content truepath) "\n"))
             (total (length lines))
             (start (cond
                     ((null begin) 1)
                     ((or (null range-type) (string= range-type "line_number"))
                      (max 1 (if (stringp begin) (string-to-number begin) begin)))
                     ((string= range-type "identifier")
                      (ekg-agent--resolve-line-id truepath begin))
                     (t (error "Unknown range_type: %s" range-type))))
             (finish (cond
                      ((null end) total)
                      ((or (null range-type) (string= range-type "line_number"))
                       (min total (if (stringp end) (string-to-number end) end)))
                      ((string= range-type "identifier")
                       (ekg-agent--resolve-line-id truepath end))
                      (t total)))
             (selected (cl-loop for i from start to finish
                                for line in (nthcdr (1- start) lines)
                                collect (format "%s: %s"
                                                (ekg-agent--line-id truepath i)
                                                line))))
        (substring-no-properties (mapconcat #'identity selected "\n"))))))

(defun ekg-agent--adjust-indentation (text target-column)
  "Adjust indentation of TEXT so its first line aligns to TARGET-COLUMN.
The relative indentation between lines is preserved.  Only
leading whitespace is removed; lines with less whitespace than
the delta are left unchanged."
  (let* ((lines (split-string text "\n"))
         (first-line (car lines))
         (_ (string-match "\\`[ \t]*" first-line))
         (first-indent (length (match-string 0 first-line)))
         (delta (- first-indent target-column)))
    (if (zerop delta)
        text
      (mapconcat
       (lambda (line)
         (if (string-empty-p line)
             line
           (if (> delta 0)
               ;; Strip delta leading whitespace chars.
               (let ((prefix (substring line 0 (min delta (length line)))))
                 (if (string-match-p "\\`[ \t]*\\'" prefix)
                     (substring line (length prefix))
                   line))
             ;; Add whitespace to reach target.
             (concat (make-string (- delta) ?\s) line))))
       lines "\n"))))

(defun ekg-agent--edit-file (path begin-id begin-text end-id end-text replacement)
  "Edit file at PATH by replacing a region identified by boundary markers.

BEGIN-ID is the line identifier where BEGIN-TEXT starts.
END-ID is the line identifier where END-TEXT starts.
REPLACEMENT replaces from the start of BEGIN-TEXT through the end
of END-TEXT (inclusive).

If the file is already open in a buffer, edit the buffer in place
without saving.  Otherwise, edit in a temp buffer and save to
disk.

Returns the file content around the edited region with line
identifiers."
  (ekg-agent--with-error-as-text
    (let* ((truepath (file-truename path))
           (begin-line (ekg-agent--resolve-line-id truepath begin-id))
           (end-line (ekg-agent--resolve-line-id truepath end-id)))
      (unless (file-exists-p truepath)
        (error "File not found: %s" path))
      (let* ((existing-buf (find-buffer-visiting truepath))
             (edit-buf (or existing-buf
                           (generate-new-buffer " *ekg-agent-edit*"))))
        (unwind-protect
            (progn
              (with-current-buffer edit-buf
                (unless existing-buf
                  (insert-file-contents truepath))
                (cl-labels
                    ((find-boundary
                       (line text role)
                       (goto-char (point-min))
                       (forward-line (1- line))
                       (if (string-empty-p text)
                           (list (line-beginning-position)
                                 (line-end-position)
                                 line)
                         (let ((line-end (line-end-position)))
                           (if (search-forward text line-end t)
                               (list (match-beginning 0)
                                     (match-end 0)
                                     (line-number-at-pos
                                      (match-beginning 0)))
                             (let ((window-start
                                    (save-excursion
                                      (goto-char (point-min))
                                      (forward-line (max 0 (- line 6)))
                                      (point)))
                                   (window-end
                                    (save-excursion
                                      (goto-char (point-min))
                                      (forward-line (+ line 5))
                                      (line-end-position))))
                               (goto-char window-start)
                               (if (search-forward text window-end t)
                                   (list (match-beginning 0)
                                         (match-end 0)
                                         (line-number-at-pos
                                          (match-beginning 0)))
                                 (error "%s text not found near line %d"
                                        role line))))))))
                  (pcase-let* ((`(,begin-pos ,_begin-end ,actual-begin-line)
                                (find-boundary begin-line begin-text "Begin"))
                               (`(,_end-start ,end-pos ,actual-end-line)
                                (find-boundary end-line end-text "End"))
                               (region-start begin-pos)
                               (line-indent (save-excursion
                                              (goto-char begin-pos)
                                              (current-indentation))))
                    (when (> region-start end-pos)
                      (error "Begin boundary occurs after end boundary"))
                    (setq begin-line actual-begin-line)
                    (setq end-line actual-end-line)
                    ;; Expand to include leading whitespace so the
                    ;; replacement controls the full indentation.
                    (goto-char region-start)
                    (let ((line-start (line-beginning-position)))
                      (when (string-match-p
                             "\\`[ \t]*\\'"
                             (buffer-substring-no-properties
                              line-start region-start))
                        (setq region-start line-start)))
                    (delete-region region-start end-pos)
                    (goto-char region-start)
                    (insert (ekg-agent--adjust-indentation
                             replacement line-indent)))))
              (unless existing-buf
                (with-current-buffer edit-buf
                  (write-region (point-min) (point-max)
                                truepath nil 'silent))))
          (unless existing-buf
            (kill-buffer edit-buf)))
        (ekg-agent--read-file truepath
                              (max 1 (- begin-line 2))
                              (+ end-line 5))))))

(defconst ekg-agent-tool-read-file
  (make-llm-tool
   :function #'ekg-agent--read-file
   :name "read_file"
   :description "Read a file and return its contents. Each line is prefixed with a unique 3-character identifier. Optionally restrict to a range by line number or identifier. If the file is open in an Emacs buffer, reads from the buffer."
   :args '((:name "path" :type string :description "The file path to read." :required t)
           (:name "begin" :type string :description "Start of range: a line number or a line identifier.  Omit to start from the beginning.")
           (:name "end" :type string :description "End of range: a line number or a line identifier.  Omit to read to the end.")
           (:name "range_type" :type string :enum ["line_number" "identifier"]
                  :description "How to interpret begin and end.  Required when begin or end is set."))))

(defconst ekg-agent-tool-edit-file
  (make-llm-tool
   :function #'ekg-agent--edit-file
   :name "edit_file"
   :description "Edit a file by replacing the inclusive region between two line identifiers and matching boundary strings. Returns the edited region with surrounding context and new line identifiers. If the file is open in an Emacs buffer, edits the buffer in place without saving; otherwise saves to disk."
   :args '((:name "path" :type string :description "The file path to edit." :required t)
           (:name "begin_id" :type string :description "The 3-character line identifier where the replacement region starts." :required t)
           (:name "begin_text" :type string :description "The text on the begin line that marks the start of the region to replace." :required t)
           (:name "end_id" :type string :description "The 3-character line identifier where the replacement region ends." :required t)
           (:name "end_text" :type string :description "The text on the end line that marks the end of the region to replace (inclusive)." :required t)
           (:name "replacement" :type string :description "The new text to insert in place of the matched region." :required t))))

(defun ekg-agent--write-file (path content)
  "Write CONTENT to the file at PATH, creating it if necessary.
If PATH is open in an Emacs buffer, replace the buffer contents
without saving.  Otherwise write directly to disk, creating parent
directories as needed.  Returns the file content with line
identifiers, like `ekg-agent--read-file'."
  (ekg-agent--with-error-as-text
    (let ((truepath (file-truename
                     (expand-file-name path))))
      (let ((buf (find-buffer-visiting truepath)))
        (if buf
            (with-current-buffer buf
              (erase-buffer)
              (insert content))
          (let ((dir (file-name-directory truepath)))
            (unless (file-directory-p dir)
              (make-directory dir t))
            (write-region content nil truepath))))
      (ekg-agent--read-file truepath))))

(defconst ekg-agent-tool-write-file
  (make-llm-tool
   :function #'ekg-agent--write-file
   :name "write_file"
   :description "Write full content to a file, creating it and parent directories if needed. If the file is open in an Emacs buffer, replaces the buffer contents without saving. Returns the new file content with line identifiers."
   :args '((:name "path" :type string :description "The file path to write." :required t)
           (:name "content" :type string :description "The full text content to write to the file." :required t))))

(defun ekg-agent--buffer-line-id (buffer-name line-num)
  "Return a 3-char base64 identifier unique to BUFFER-NAME and LINE-NUM."
  (let* ((input (format "buf:%s:%d" buffer-name line-num))
         (hash (secure-hash 'md5 input))
         (raw (unibyte-string (string-to-number (substring hash 0 2) 16)
                              (string-to-number (substring hash 2 4) 16))))
    (substring (base64-encode-string raw t) 0 3)))

(defun ekg-agent--resolve-buffer-line-id (buffer-name id)
  "Resolve line identifier ID to a line number for the buffer BUFFER-NAME."
  (let* ((buf (get-buffer buffer-name))
         (total (with-current-buffer buf
                  (count-lines (point-min) (point-max)))))
    ;; count-lines can undercount when the buffer doesn't end with a
    ;; newline, so add 1 to ensure we check the last line.
    (or (cl-loop for i from 1 to (1+ total)
                 when (string= (ekg-agent--buffer-line-id buffer-name i) id)
                 return i)
        (error "Line identifier %s not found in buffer %s" id buffer-name))))

(defun ekg-agent--list-buffers (&optional regex-filter)
  "List buffers, optionally filtering names by REGEX-FILTER.
Returns a string with one buffer per line showing name, size, major
mode, and file (if any).  Internal buffers (names starting with space)
are excluded."
  (ekg-agent--with-error-as-text
    (let* ((regex-filter (and (ekg-agent--nonempty-string-p regex-filter)
                              regex-filter))
           (bufs (seq-filter
                  (lambda (b)
                    (let ((name (buffer-name b)))
                      (and (not (string-prefix-p " " name))
                           (or (null regex-filter)
                               (string-match-p regex-filter name)))))
                  (buffer-list)))
           (lines (mapcar
                   (lambda (b)
                     (with-current-buffer b
                       (format "%s  (%d bytes, %s%s)"
                               (buffer-name b)
                               (buffer-size)
                               major-mode
                               (if buffer-file-name
                                   (format ", file: %s" buffer-file-name)
                                 ""))))
                   bufs)))
      (if lines
          (mapconcat #'identity lines "\n")
        "No buffers found."))))

(defconst ekg-agent-tool-list-buffers
  (make-llm-tool
   :function #'ekg-agent--list-buffers
   :name "list_buffers"
   :description "List open Emacs buffers.  Returns one line per buffer showing name, size, major mode, and associated file (if any).  Internal buffers (names starting with a space) are excluded."
   :args '((:name "regex_filter" :type string
                  :description "Optional regex to filter buffer names by."))))

(defun ekg-agent--read-buffer (buffer-name &optional begin end range-type)
  "Read BUFFER-NAME, returning contents with line identifiers.

Each line is prefixed with a 3-char identifier derived from the
buffer name and line number.  The overall begin and end buffer
positions of the returned content are included at the top as a
header line.

BEGIN and END restrict the output to a range.  RANGE-TYPE is
either \"line_number\" or \"identifier\" and indicates how to
interpret BEGIN and END.  Empty strings for BEGIN, END, or
RANGE-TYPE are treated as nil."
  (ekg-agent--with-error-as-text
    (let* ((begin (and (ekg-agent--nonempty-string-p begin) begin))
           (end (and (ekg-agent--nonempty-string-p end) end))
           (range-type (and (ekg-agent--nonempty-string-p range-type)
                            range-type))
           (buf (get-buffer buffer-name)))
      (unless buf
        (error "Buffer not found: %s" buffer-name))
      (with-current-buffer buf
        (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
               (lines (split-string content "\n"))
               (total (length lines))
               (start (cond
                       ((null begin) 1)
                       ((or (null range-type) (string= range-type "line_number"))
                        (max 1 (if (stringp begin) (string-to-number begin) begin)))
                       ((string= range-type "identifier")
                        (ekg-agent--resolve-buffer-line-id buffer-name begin))
                       (t (error "Unknown range_type: %s" range-type))))
               (finish (cond
                        ((null end) total)
                        ((or (null range-type) (string= range-type "line_number"))
                         (min total (if (stringp end) (string-to-number end) end)))
                        ((string= range-type "identifier")
                         (ekg-agent--resolve-buffer-line-id buffer-name end))
                        (t total)))
               ;; Compute buffer positions for the returned range.
               (begin-pos (save-excursion
                            (goto-char (point-min))
                            (forward-line (1- start))
                            (point)))
               (end-pos (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- finish))
                          (line-end-position)))
               (selected (cl-loop for i from start to finish
                                  for line in (nthcdr (1- start) lines)
                                  collect (format "%s: %s"
                                                  (ekg-agent--buffer-line-id buffer-name i)
                                                  line))))
          (format "begin_pos: %d  end_pos: %d\n%s"
                  begin-pos end-pos
                  (substring-no-properties
                   (mapconcat #'identity selected "\n"))))))))

(defconst ekg-agent-tool-read-buffer
  (make-llm-tool
   :function #'ekg-agent--read-buffer
   :name "read_buffer"
   :description "Read an Emacs buffer and return its contents. Each line is prefixed with a unique 3-character identifier. The first line reports the begin_pos and end_pos buffer positions. Optionally restrict to a range by line number or identifier."
   :args '((:name "buffer_name" :type string :description "The name of the buffer to read." :required t)
           (:name "begin" :type string :description "Start of range: a line number or a line identifier.  Omit to start from the beginning.")
           (:name "end" :type string :description "End of range: a line number or a line identifier.  Omit to read to the end.")
           (:name "range_type" :type string :enum ["line_number" "identifier"]
                  :description "How to interpret begin and end.  Required when begin or end is set."))))

(defun ekg-agent--edit-buffer (buffer-name begin-id begin-text
                                           end-id end-text replacement)
  "Edit BUFFER-NAME by replacing a region between boundary markers.

BEGIN-ID is the line identifier where BEGIN-TEXT starts.
END-ID is the line identifier where END-TEXT starts.
REPLACEMENT replaces from the start of BEGIN-TEXT through the end
of END-TEXT (inclusive).

Returns the buffer content around the edited region with line
identifiers."
  (ekg-agent--with-error-as-text
    (let ((buf (get-buffer buffer-name)))
      (unless buf
        (error "Buffer not found: %s" buffer-name))
      (let ((begin-line (ekg-agent--resolve-buffer-line-id buffer-name begin-id))
            (end-line (ekg-agent--resolve-buffer-line-id buffer-name end-id)))
        (with-current-buffer buf
          (goto-char (point-min))
          (forward-line (1- begin-line))
          (let ((region-start (point))
                (line-indent (current-indentation)))
            (unless (search-forward begin-text (line-end-position) t)
              (error "Begin text not found on line %d" begin-line))
            (setq region-start (match-beginning 0))
            ;; Expand to include leading whitespace so the replacement
            ;; controls the full indentation.
            (let ((line-start (line-beginning-position)))
              (when (string-match-p
                     "\\`[ \t]*\\'"
                     (buffer-substring-no-properties line-start region-start))
                (setq region-start line-start)))
            (goto-char (point-min))
            (forward-line (1- end-line))
            (unless (search-forward end-text (line-end-position) t)
              (error "End text not found on line %d" end-line))
            (let ((region-end (match-end 0)))
              (delete-region region-start region-end)
              (goto-char region-start)
              (insert (ekg-agent--adjust-indentation
                       replacement line-indent)))))
        (ekg-agent--read-buffer buffer-name
                                (number-to-string (max 1 (- begin-line 2)))
                                (number-to-string (+ end-line 5))
                                "line_number")))))

(defconst ekg-agent-tool-edit-buffer
  (make-llm-tool
   :function #'ekg-agent--edit-buffer
   :name "edit_buffer"
   :description "Edit a buffer by replacing the inclusive region between two line identifiers and matching boundary strings. Returns the edited region with surrounding context, including begin_pos and end_pos."
   :args '((:name "buffer_name" :type string :description "The name of the buffer to edit." :required t)
           (:name "begin_id" :type string :description "The 3-character line identifier where the replacement region starts." :required t)
           (:name "begin_text" :type string :description "The text on the begin line that marks the start of the region to replace." :required t)
           (:name "end_id" :type string :description "The 3-character line identifier where the replacement region ends." :required t)
           (:name "end_text" :type string :description "The text on the end line that marks the end of the region to replace (inclusive)." :required t)
           (:name "replacement" :type string :description "The new text to insert in place of the matched region." :required t))))

(defun ekg-agent--resolve-buffer-point (buffer-name point line-id text)
  "Resolve a point in BUFFER-NAME from either POINT, or LINE-ID + TEXT.
Returns a buffer position (integer).  At least one of POINT or
LINE-ID must be provided."
  (let ((point (and (ekg-agent--nonempty-string-p point) point))
        (line-id (and (ekg-agent--nonempty-string-p line-id) line-id))
        (text (and (ekg-agent--nonempty-string-p text) text)))
    (cond
     (point
      (let ((pos (if (stringp point) (string-to-number point) point)))
        (with-current-buffer (get-buffer buffer-name)
          (min (point-max) (max (point-min) pos)))))
     (line-id
      (let ((line-num (ekg-agent--resolve-buffer-line-id buffer-name line-id)))
        (with-current-buffer (get-buffer buffer-name)
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line-num))
            (if text
                (progn
                  (unless (search-forward text (line-end-position) t)
                    (error "Text %S not found on line %d" text line-num))
                  (match-beginning 0))
              (point))))))
     (t (error "Must provide either point or line_id")))))

(defun ekg-agent--run-interactive-command (buffer-name command &optional point line-id text)
  "Run interactive COMMAND in BUFFER-NAME at a resolved position.

The position is resolved from either POINT (a buffer position) or
LINE-ID (a 3-char identifier from read_buffer) optionally refined by
TEXT on that line.  If both POINT and LINE-ID are given, POINT takes
precedence.

Returns the buffer content around the position after the command
executes, including begin_pos/end_pos."
  (ekg-agent--with-error-as-text
    (let ((buf (get-buffer buffer-name)))
      (unless buf
        (error "Buffer not found: %s" buffer-name))
      (let ((pos (ekg-agent--resolve-buffer-point buffer-name point line-id text))
            (cmd (if (stringp command) (intern command) command)))
        (unless (commandp cmd)
          (error "Not an interactive command: %s" command))
        (with-current-buffer buf
          (goto-char (min (point-max) (max (point-min) pos)))
          (if (eq cmd 'eval-buffer)
              (eval-buffer)
            (call-interactively cmd))
          ;; Return context: ~10 lines around the post-command point,
          ;; since the command may have moved point.
          (let ((current-line (line-number-at-pos (point))))
            (ekg-agent--read-buffer buffer-name
                                    (number-to-string (max 1 (- current-line 5)))
                                    (number-to-string (+ current-line 5))
                                    "line_number")))))))

(defconst ekg-agent-tool-run-interactive-command
  (make-llm-tool
   :function #'ekg-agent--run-interactive-command
   :name "run_interactive_command"
   :description "Execute an interactive Emacs command in a buffer at a specific position. The position can be specified as a buffer point or as a line identifier optionally refined by text on that line. If both point and line_id are given, point takes precedence. Returns buffer content around the final position."
   :args '((:name "buffer_name" :type string :description "The name of the buffer." :required t)
           (:name "command" :type string :description "The interactive Emacs command to run (e.g. \"org-todo\", \"indent-region\")." :required t)
           (:name "point" :type string :description "Buffer position (integer as string) where the command should be executed.  Takes precedence over line_id if both are given.")
           (:name "line_id" :type string :description "A 3-character line identifier for the buffer line.")
           (:name "text" :type string :description "Text on the identified line to refine the position.  Point is placed at the start of this text.  Only used with line_id."))))

(defun ekg-agent--run-command (callback command &optional directory)
  "Run shell COMMAND asynchronously and call CALLBACK with the result.
DIRECTORY, if given, is used as `default-directory' for the process."
  (condition-case err
      (let* ((default-directory (if directory
                                    (file-truename directory)
                                  default-directory))
             (output-buf (generate-new-buffer " *ekg-agent-cmd*" t))
             (proc (make-process
                    :name "ekg-agent-command"
                    :buffer output-buf
                    :command (list shell-file-name shell-command-switch command)
                    :sentinel (lambda (process _event)
                                (when (memq (process-status process) '(exit signal))
                                  (let ((result
                                         (with-current-buffer (process-buffer process)
                                           (format "Exit code: %d\n%s"
                                                   (process-exit-status process)
                                                   (buffer-string)))))
                                    (kill-buffer (process-buffer process))
                                    (funcall callback (substring-no-properties result))))))))
        (push proc ekg-agent--tool-processes))
    (error (funcall callback (format "Error: %s" (error-message-string err))))))

(defconst ekg-agent-tool-run-command
  (make-llm-tool
   :function #'ekg-agent--run-command
   :name "run_command"
   :description "Run a shell command and return its combined stdout/stderr and exit code."
   :args '((:name "command" :type string :description "The shell command to run." :required t)
           (:name "directory" :type string :description "Working directory for the command.  Defaults to the current buffer's directory."))
   :async t))

(defcustom ekg-agent-web-max-chars 20000
  "Maximum number of characters to return from web page content.
Content beyond this limit is truncated with a notice."
  :type 'integer
  :group 'ekg-agent)

(defun ekg-agent--web-render-html (html-string url)
  "Render HTML-STRING as readable text using shr, as eww does.
URL is used for resolving relative links."
  (with-temp-buffer
    (insert html-string)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (erase-buffer)
      (let ((shr-width 80)
            (shr-use-fonts nil)
            (shr-bullet "- ")
            (shr-current-font 'default)
            (shr-base (when url (url-generic-parse-url url))))
        (shr-insert-document dom))
      ;; shr can produce Emacs "raw byte" characters (U+3FFF80 and
      ;; above) that aren't valid Unicode, causing json-serialize to
      ;; fail with json-value-p errors downstream.  Strip them.
      (let ((text (string-trim
                   (replace-regexp-in-string
                    "[\x3fff80-\x3fffff]" ""
                    (buffer-substring-no-properties
                     (point-min) (point-max))))))
        (if (> (length text) ekg-agent-web-max-chars)
            (concat (substring text 0 ekg-agent-web-max-chars)
                    "\n\n[Content truncated at "
                    (number-to-string ekg-agent-web-max-chars) " characters]")
          text)))))

(defun ekg-agent--web-browse (callback url)
  "Fetch URL and return its rendered text content via CALLBACK.
Uses `url-retrieve' for async fetching and `shr' for rendering,
the same engine that powers eww.  Only http and https URLs are
supported."
  (if (not (string-match-p "\\`https?://" url))
      (funcall callback "Error: Only http and https URLs are supported.")
    (condition-case err
        (let ((buf (url-retrieve
                    url
                    (lambda (status)
                      (let ((url-buf (current-buffer)))
                        ;; Build the result string inside condition-case,
                        ;; then call the callback OUTSIDE it.  This prevents
                        ;; errors from within the callback chain (e.g.
                        ;; json-serialize in a subsequent LLM request) from
                        ;; being caught here and re-triggering the callback.
                        (let ((result
                               (condition-case err
                                   (if-let ((err-val (plist-get status :error)))
                                       (format "Error fetching URL: %S"
                                               err-val)
                                     (goto-char (point-min))
                                     (let ((header-end
                                            (or (search-forward "\n\n" nil t)
                                                (point-min))))
                                       (let* ((html (buffer-substring-no-properties
                                                     header-end (point-max)))
                                              (text (ekg-agent--web-render-html
                                                     html url)))
                                         (if (string-empty-p text)
                                             "Page loaded but no readable text content found."
                                           (format "Content from %s:\n\n%s"
                                                   url text)))))
                                 (error
                                  (format "Error rendering page: %s"
                                          (error-message-string err))))))
                          (kill-buffer url-buf)
                          (funcall callback result))))
                    nil t)))
          (when-let ((proc (get-buffer-process buf)))
            (push proc ekg-agent--tool-processes)))
      (error (funcall callback (format "Error: %s"
                                       (error-message-string err)))))))

(defconst ekg-agent-tool-web-browse
  (make-llm-tool
   :function #'ekg-agent--web-browse
   :name "web_browse"
   :description "Fetch a web page and return its content as readable text.  Uses the same rendering engine as Emacs eww browser."
   :args '((:name "url" :type string :description "The URL to fetch." :required t))
   :async t))

(defun ekg-agent--web-search (callback query)
  "Search the web for QUERY and return results via CALLBACK.
Uses `eww-search-prefix' to construct the search URL."
  (require 'eww)
  (let ((search-url (concat eww-search-prefix (url-hexify-string query))))
    (ekg-agent--web-browse callback search-url)))

(defconst ekg-agent-tool-web-search
  (make-llm-tool
   :function #'ekg-agent--web-search
   :name "web_search"
   :description "Search the web for a query and return the search results page as text.  Uses the search engine configured in `eww-search-prefix' (DuckDuckGo by default)."
   :args '((:name "query" :type string :description "The search query." :required t))
   :async t))

(defcustom ekg-agent-emacs-info-default-manuals
  '("emacs" "elisp" "cl")
  "Info manuals searched when `emacs_info_search' has no manual.
The default set covers the Emacs user manual, the Emacs Lisp
reference, and the Common Lisp extensions manual."
  :type '(repeat string)
  :group 'ekg-agent)

(defcustom ekg-agent-emacs-reference-max-chars 20000
  "Maximum characters returned by Emacs help and Info tools."
  :type 'integer
  :group 'ekg-agent)

(defun ekg-agent--limit-reference-text (text)
  "Return TEXT truncated for use as an Emacs reference tool result."
  (if (> (length text) ekg-agent-emacs-reference-max-chars)
      (concat (substring text 0 ekg-agent-emacs-reference-max-chars)
              "\n\n[Content truncated at "
              (number-to-string ekg-agent-emacs-reference-max-chars)
              " characters]")
    text))

(defun ekg-agent--reference-count (value default)
  "Return VALUE as a positive integer, falling back to DEFAULT."
  (let ((n (cond
            ((integerp value) value)
            ((and (stringp value) (not (string-empty-p value)))
             (string-to-number value))
            (t default))))
    (max 1 (min 20 (or n default)))))

(defun ekg-agent--capture-help-buffer (thunk)
  "Run THUNK and return the resulting Help buffer text."
  (let ((help-window-select nil))
    (save-window-excursion
      (let ((buf (get-buffer (help-buffer))))
        (when buf
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)))))
      (funcall thunk)
      (if-let* ((buf (get-buffer (help-buffer))))
          (with-current-buffer buf
            (ekg-agent--limit-reference-text
             (string-trim
              (buffer-substring-no-properties (point-min) (point-max)))))
        "No help buffer was produced."))))

(defun ekg-agent--emacs-help-symbol (symbol &optional kind)
  "Return Emacs help for SYMBOL.
KIND may be \"auto\", \"function\", \"variable\", or \"face\"."
  (ekg-agent--with-error-as-text
    (let* ((sym (intern-soft symbol))
           (kind (downcase
                  (or (and (ekg-agent--nonempty-string-p kind) kind)
                      "auto"))))
      (unless sym
        (error "No symbol named %s is known in this Emacs session" symbol))
      (ekg-agent--capture-help-buffer
       (lambda ()
         (pcase kind
           ((or "auto" "symbol")
            (if (fboundp 'describe-symbol)
                (describe-symbol sym)
              (if (fboundp sym)
                  (describe-function sym)
                (describe-variable sym))))
           ("function" (describe-function sym))
           ("variable" (describe-variable sym))
           ("face" (describe-face sym))
           (_ (error "Unknown help kind: %s" kind))))))))

(defconst ekg-agent-tool-emacs-help-symbol
  (make-llm-tool
   :function #'ekg-agent--emacs-help-symbol
   :name "emacs_help_symbol"
   :description "Show Emacs help for a symbol using the same help facilities as describe-symbol, describe-function, describe-variable, and describe-face.  Use this for elisp functions, macros, variables, faces, and symbols before guessing from memory."
   :args '((:name "symbol" :type string :description "The Emacs Lisp symbol to describe, such as \"cl-defstruct\"." :required t)
           (:name "kind" :type string :enum ["auto" "function" "variable" "face"]
                  :description "Which help facility to use.  Defaults to auto."))))

(defun ekg-agent--symbol-summary-line (symbol)
  "Return a compact apropos summary line for SYMBOL."
  (let* ((kinds (delq nil
                      (list (and (fboundp symbol) "function")
                            (and (boundp symbol) "variable")
                            (and (facep symbol) "face"))))
         (doc (cond
               ((fboundp symbol) (documentation symbol t))
               ((boundp symbol)
                (documentation-property symbol 'variable-documentation t))
               ((facep symbol) (face-documentation symbol))))
         (first-line (and doc (car (split-string doc "\n")))))
    (format "%s%s%s"
            symbol
            (if kinds (format " (%s)" (string-join kinds ", ")) "")
            (if first-line (format " -- %s" first-line) ""))))

(defun ekg-agent--emacs-help-search (query &optional search-docs max-results)
  "Search Emacs help/apropos for QUERY.
When SEARCH-DOCS is \"yes\", search documentation text with
`apropos-documentation'.  Otherwise search symbol names with
`apropos-internal'.  MAX-RESULTS limits symbol-name results."
  (ekg-agent--with-error-as-text
    (unless (ekg-agent--nonempty-string-p query)
      (error "Query must be a non-empty string"))
    (let ((search-docs (and (ekg-agent--nonempty-string-p search-docs)
                            search-docs))
          (max-results (ekg-agent--reference-count max-results 20)))
      (if (string= search-docs "yes")
          (let ((text (ekg-agent--capture-help-buffer
                       (lambda () (apropos-documentation query)))))
            (if (string-empty-p text)
                "No apropos documentation matches found."
              text))
        (let* ((symbols (apropos-internal query))
               (limited (seq-take symbols (min max-results
                                               (length symbols)))))
          (if limited
              (mapconcat #'ekg-agent--symbol-summary-line limited "\n")
            "No apropos symbol matches found."))))))

(defconst ekg-agent-tool-emacs-help-search
  (make-llm-tool
   :function #'ekg-agent--emacs-help-search
   :name "emacs_help_search"
   :description "Search Emacs help/apropos for symbols or documentation.  Use this to discover relevant Emacs Lisp functions, macros, variables, or concepts when you do not know the exact symbol name."
   :args '((:name "query" :type string :description "Emacs regexp to search for, such as \"defstruct\" or \"window\"." :required t)
           (:name "search_docs" :type string :enum ["no" "yes"]
                  :description "Use \"yes\" to search documentation text with apropos-documentation; defaults to symbol-name search.")
           (:name "max_results" :type integer
                  :description "Maximum symbol-name results to return.  Defaults to 20; capped at 20."))))

(defun ekg-agent--info-manual-name ()
  "Return a compact name for the current Info manual."
  (if (and (boundp 'Info-current-file) Info-current-file)
      (file-name-base Info-current-file)
    "unknown"))

(defun ekg-agent--info-node-text ()
  "Return the current Info node as plain text."
  (format "Manual: %s\nNode: %s\n\n%s"
          (ekg-agent--info-manual-name)
          (or Info-current-node "unknown")
          (ekg-agent--limit-reference-text
           (buffer-substring-no-properties (point-min) (point-max)))))

(defun ekg-agent--emacs-info-node-target (node manual)
  "Return an Info node target from NODE and optional MANUAL."
  (let ((node (and (ekg-agent--nonempty-string-p node) node))
        (manual (and (ekg-agent--nonempty-string-p manual) manual)))
    (cond
     ((and node (string-prefix-p "(" node)) node)
     (manual (format "(%s)%s" manual (or node "Top")))
     (node node)
     (t (error "Must provide either node or manual")))))

(defun ekg-agent--emacs-info-node (node &optional manual)
  "Return Info NODE text, optionally resolving NODE in MANUAL."
  (ekg-agent--with-error-as-text
    (info-initialize)
    (save-window-excursion
      (Info-goto-node (ekg-agent--emacs-info-node-target node manual))
      (ekg-agent--info-node-text))))

(defconst ekg-agent-tool-emacs-info-node
  (make-llm-tool
   :function #'ekg-agent--emacs-info-node
   :name "emacs_info_node"
   :description "Read an Emacs Info node and return its text.  Pass either a full node like \"(cl)Structures\" or a manual plus node, such as manual \"elisp\" and node \"Processes\"."
   :args '((:name "node" :type string :description "Info node name, such as \"Top\", \"Structures\", or \"(cl)Structures\".")
           (:name "manual" :type string :description "Optional Info manual name, such as \"emacs\", \"elisp\", or \"cl\"."))))

(defun ekg-agent--info-snippet (&optional matched-regexp)
  "Return an Info search result snippet around point.
MATCHED-REGEXP is the regexp that produced the match."
  (let ((start (save-excursion
                 (forward-line -3)
                 (point)))
        (end (save-excursion
               (forward-line 8)
               (point))))
    (format "Manual: %s\nNode: %s%s\nSnippet:\n%s"
            (ekg-agent--info-manual-name)
            (or Info-current-node "unknown")
            (if matched-regexp
                (format "\nMatched regexp: %s" matched-regexp)
              "")
            (string-trim
             (buffer-substring-no-properties start end)))))

(defun ekg-agent--emacs-info-search-one (regexp manual max-results)
  "Search MANUAL for REGEXP and return up to MAX-RESULTS snippets."
  (let (results seen done)
    (condition-case nil
        (save-window-excursion
          (Info-goto-node (format "(%s)Top" manual))
          (goto-char (point-min))
          (while (and (not done) (< (length results) max-results))
            (if (Info-search regexp nil t)
                (let ((key (format "%s:%s:%d"
                                   (ekg-agent--info-manual-name)
                                   Info-current-node
                                   (point))))
                  (unless (member key seen)
                    (push key seen)
                    (push (ekg-agent--info-snippet regexp) results))
                  (if (< (point) (point-max))
                      (forward-char 1)
                    (setq done t)))
              (setq done t))))
      (error nil))
    (nreverse results)))

(defun ekg-agent--emacs-info-search (query &optional manual max-results)
  "Search Emacs Info manuals for QUERY.
QUERY is an Emacs regexp.  MANUAL restricts the search to one Info
manual; otherwise `ekg-agent-emacs-info-default-manuals' is used.
MAX-RESULTS is capped at 20."
  (ekg-agent--with-error-as-text
    (unless (ekg-agent--nonempty-string-p query)
      (error "Query must be a non-empty string"))
    (info-initialize)
    (let* ((manuals (if (ekg-agent--nonempty-string-p manual)
                        (list manual)
                      ekg-agent-emacs-info-default-manuals))
           (max-results (ekg-agent--reference-count max-results 8))
           (remaining max-results)
           results)
      (dolist (manual manuals)
        (when (> remaining 0)
          (let ((manual-results
                 (ekg-agent--emacs-info-search-one query manual remaining)))
            (setq results (append results manual-results))
            (setq remaining (- max-results (length results))))))
      (if results
          (ekg-agent--limit-reference-text
           (mapconcat #'identity results "\n\n---\n\n"))
        (format "No Info matches found for %S in manuals: %s"
                query
                (string-join manuals ", "))))))

(defconst ekg-agent-tool-emacs-info-search
  (make-llm-tool
   :function #'ekg-agent--emacs-info-search
   :name "emacs_info_search"
   :description "Search Emacs Info manuals and return matching manual/node snippets.  Use this for Emacs behavior, built-in facilities, and Emacs Lisp reference material.  The query is an Emacs regexp; omit manual to search the default Emacs, Elisp, and CL manuals."
   :args '((:name "query" :type string :description "Emacs regexp to search for in Info manuals, such as \":include\" or \"cl-defmethod\"." :required t)
           (:name "manual" :type string :description "Optional Info manual name, such as \"emacs\", \"elisp\", or \"cl\".")
           (:name "max_results" :type integer
                  :description "Maximum search results to return.  Defaults to 8; capped at 20."))))

(defconst ekg-agent-base-tools
  (list
   ekg-agent-tool-all-tags
   ekg-agent-tool-any-tags
   ekg-agent-tool-get-note-by-id
   ekg-agent-tool-search-notes
   ekg-agent-tool-list-tags
   ekg-agent-tool-append-to-note
   ekg-agent-tool-replace-note
   ekg-agent-tool-create-note
   ekg-agent-tool-summarize-state
   ekg-agent-tool-subagent
   ekg-agent-tool-ask-user
   ekg-agent-tool-read-agents-md
   ekg-agent-tool-read-file
   ekg-agent-tool-edit-file
   ekg-agent-tool-write-file
   ekg-agent-tool-list-buffers
   ekg-agent-tool-read-buffer
   ekg-agent-tool-edit-buffer
   ekg-agent-tool-run-interactive-command
   ekg-agent-tool-web-browse
   ekg-agent-tool-web-search
   ekg-agent-tool-emacs-help-symbol
   ekg-agent-tool-emacs-help-search
   ekg-agent-tool-emacs-info-node
   ekg-agent-tool-emacs-info-search)
  "List of base tools available to the agent.
These tools are necessary for basic agent functionality.")

(defcustom ekg-agent-extra-tools nil
  "List of additional tools available to the agent.
These tools are used in addition to `ekg-agent-base-tools' in
`ekg-agent-evaluate-status' and `ekg-agent-note-response'.

This is the place to opt into tools like `ekg-agent-tool-run-elisp',
or `ekg-agent-tool-code'."
  :type '(repeat (sexp :tag "Tool"))
  :group 'ekg-agent)

(defun ekg-agent--ask (question context &optional extra-tools provider)
  "Ask the ekg agent a QUESTION and display the result.

CONTEXT is what we'll display to the agent as context.  If nil no
additional context is added.

EXTRA-TOOLS is a list of additional tools used for this request.

PROVIDER is the LLM provider to use.  If nil, use
`ekg-agent--provider'.

The agent has access to all the defined tools, and can create notes or
display results in a popup buffer, or ask the user a question.  The
agent will decide which is best."
  (let* ((prompt (concat (ekg-agent-instructions-intro)
                         "\n\nYour instructions are to answer the user's question, given below. "
                         "You have access to tools to help you. "
                         "After each tool call you will be given a chance to make more tool calls. "
                         "Do the user's concrete task first. Use `create_note` when the task "
                         "asks you to store memory, when you discover reusable knowledge, or "
                         "when a longer task needs a checkpoint; it will NOT end your session. "
                         "Action verbs such as fix, add, write, create, update, store, or "
                         "follow require actual tool actions; reading and summarizing are "
                         "not enough. If you find a file issue and the user asked you to fix "
                         "it, call `write_file` or `edit_file` before the final response. "
                         "For file changes, verify the change before finishing: inspect the "
                         "tool result, re-read the file, or run a suitable check. "
                         "If the user asks you to store a skill or memory, create the requested "
                         "note immediately after the concrete work is verified; do not spend "
                         "extra turns re-verifying an already-correct tool result. "
                         "When you are finished, call `display_result_in_popup` with a concise "
                         "summary, or call `end` if there is nothing useful to display."
                         (when context
                           "\n\nHere is some additional context to help you:\n")
                         context
                         "\n\n"
                         (format "The current date and time is %s."
                                 (format-time-string "%F %R")))))
    (ekg-agent--iterate (llm-make-chat-prompt
                         question
                         :context prompt
                         :tools
                         (ekg-agent--tools (append
                                            (list ekg-agent-tool-end
                                                  ekg-agent-tool-popup-result)
                                            extra-tools))
                         :tool-options (make-llm-tool-options :tool-choice 'any))
                        0
                        (ekg-agent--make-status-callback)
                        '("display_result_in_popup" "end")
                        nil
                        nil
                        provider)))

(defun ekg-agent-ask (question &optional arg)
  "Ask the ekg agent a QUESTION and display the result.
The agent has access to all the ekg tools, and can create notes
or display results in a popup buffer.  The agent will decide
which is best.

With prefix ARG, prompt for the LLM provider when `ekg-llm-provider'
is a list."
  (interactive (list (read-string "Question: ") current-prefix-arg))
  (ekg-connect)
  (ekg-agent--ask question
                  (concat
                   "The last 10 notes:\n\n"
                   (mapconcat #'ekg-llm-note-to-text
                              (ekg-get-latest-modified 10) "\n\n"))
                  nil
                  (ekg-agent--read-provider-for-prefix arg)))

(defun ekg-agent-ask-with-note (question &optional id extra-tools arg)
  "Ask the agent QUESTION with the note with ID as context.

If ID is nil, we will use the current buffer's associated note, or the
note at point if in a `ekg-notes-mode` buffer.

EXTRA-TOOLS is a list of additional tools to make available to the
agent.

With prefix ARG, prompt for the LLM provider when `ekg-llm-provider'
is a list."
  (interactive (list (read-string "Question: ") nil nil current-prefix-arg))
  (ekg-connect)
  (let* ((note (or (and id (ekg-agent--get-note-with-id id))
                   (ekg-current-note-or-error-expanded)))
         (note-text (ekg-llm-note-to-text note))
         (prompt-notes (ekg-get-notes-cotagged-with-tags
                        (ekg-note-tags note) ekg-llm-prompt-tag))
         (prompt-context (when prompt-notes
                           (concat "\n\nPrompt instructions from co-tagged notes:\n"
                                   (mapconcat (lambda (n)
                                                (string-trim
                                                 (substring-no-properties
                                                  (ekg-display-note-text n nil 'plaintext))))
                                              prompt-notes "\n")))))
    (ekg-agent--ask question
                    (concat
                     "The user is issuing instructions with a note as context. This is the text of that note:\n"
                     note-text
                     prompt-context)
                    extra-tools
                    (ekg-agent--read-provider-for-prefix arg))))

(defun ekg-agent-ask-with-buffer (instructions &optional arg)
  "Issue INSTRUCTIONS to the agent, with the current buffer as context.

With prefix ARG, prompt for the LLM provider when `ekg-llm-provider'
is a list."
  (interactive (list (read-string "Instructions: ") current-prefix-arg))
  (ekg-connect)
  (ekg-agent--ask instructions
                  (concat
                   (format "The current buffer is named %s%s, the major mode is %s.  The content is:\n"
                           (buffer-name)
                           (if buffer-file-name
                               (format " (file: %s)" buffer-file-name)
                             "")
                           major-mode)
                   (buffer-substring-no-properties (point-min) (point-max)))
                  nil
                  (ekg-agent--read-provider-for-prefix arg)))

(defun ekg-agent-ask-with-region (instructions start end &optional arg)
  "Issue INSTRUCTIONS to the agent, with the region from START to END as context.

With prefix ARG, prompt for the LLM provider when `ekg-llm-provider'
is a list."
  (interactive (list (read-string "Instructions: ")
                     (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (ekg-connect)
  (ekg-agent--ask instructions
                  (concat
                   (format "The current buffer is named %s%s, the major mode is %s.  The content is the selected region:\n"
                           (buffer-name)
                           (if buffer-file-name
                               (format " (file: %s)" buffer-file-name)
                             "")
                           major-mode)
                   (buffer-substring-no-properties start end))
                  nil
                  (ekg-agent--read-provider-for-prefix arg)))

(defun ekg-agent-latest-notes-context ()
  "Return the context for an agent for new sessions.

This includes the latest 10 notes and the org agenda, and the last 10
self info."
  (concat
   "The last 10 notes:\n"
   (let ((ekg-llm-note-numwords 300))
     (mapconcat #'ekg-llm-note-to-text
                (ekg-get-latest-modified 10) "\n\n"))
   (when (featurep 'org-ql)
     (concat
      "\n\nTODO items from org-mode:\n"
      (with-temp-buffer (org-ql-search org-agenda-files '(todo) :buffer (current-buffer))
                        (buffer-substring-no-properties (point-min) (point-max)))))
   "\n\nThe last 10 self-info notes:\n"
   (mapconcat #'ekg-llm-note-to-text
              (seq-take (ekg-get-notes-with-tag ekg-agent-self-info-tag)
                        10)
              "\n\n")))

(defun ekg-agent--read-agents-md (dir)
  "Read AGENTS.md from DIR, returning its contents or nil if not found."
  (let ((file (expand-file-name "AGENTS.md" dir)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun ekg-agent--agents-md-context ()
  "Return AGENTS.md content from the current and home directories.
Returns nil if no AGENTS.md files are found."
  (let* ((home-dir (expand-file-name "~"))
         (current-dir (expand-file-name default-directory))
         (home-content (ekg-agent--read-agents-md home-dir))
         (current-content (unless (string= current-dir (file-name-as-directory home-dir))
                            (ekg-agent--read-agents-md current-dir)))
         (parts nil))
    (when home-content
      (push (format "From %s:\n%s" (expand-file-name "AGENTS.md" home-dir) home-content) parts))
    (when current-content
      (push (format "From %s:\n%s" (expand-file-name "AGENTS.md" current-dir) current-content) parts))
    (when parts
      (concat "\n\nUser instructions from AGENTS.md files:\n\n"
              (string-join (nreverse parts) "\n\n")))))

(defun ekg-agent-cotagged-prompt-tags ()
  "Return a list of all tags that are cotagged with the prompt tag."
  (let (result)
    (dolist (note (ekg-get-notes-with-tag ekg-llm-prompt-tag))
      (dolist (tag (ekg-note-tags note))
        (when (not (string-equal tag ekg-llm-prompt-tag))
          (push tag result))))
    result))

(defun ekg-agent-instructions-intro ()
  "Introductory instructions for the ekg agent.

These instructions guide the agent on how to effectively use ekg
notes as memory for tasks.  Follow these rules to ensure proper
note-taking and knowledge retention."
  (let ((timeout-desc (if (and (numberp ekg-agent-timeout-seconds)
                               (> ekg-agent-timeout-seconds 0))
                          (format "%s seconds" ekg-agent-timeout-seconds)
                        "no timeout configured")))
    (format
     "IMPORTANT: Complete the user's concrete task before optional
memory work. Use ekg memory to improve the work, but do not let
memory bookkeeping delay or replace the requested outcome.

ekg is an external memory system that stores notes in a database
organized by tags. Use ekg to remember important context, share
knowledge across sessions, and learn from past work when that is useful
for the task or explicitly requested.

== TASK ORDER ==

1. Do and verify concrete work first.
   - For file tasks, read the relevant file, make the change, then verify
     by inspecting the result or running the requested check.
   - A request to fix, add, write, update, create, store, or follow
     something is not complete until you have performed that action with
     the appropriate tool.
   - If the task also asks you to create notes or skills, do that after
     the concrete work is complete unless the note itself is the main task.
     If the file tool's returned content shows the requested edit, that is
     enough verification for small changes; move on to the requested note.
   - Do not call `search_notes` just because you plan to create a note.
     Semantic search may be unavailable; when exact memory lookup is
     needed, prefer tag tools such as `list_all_tags`,
     `get_notes_with_all_tags`, or `get_notes_with_any_tags`.

2. Retrieve memory only when it affects the task.
   - If the user asks you to follow notes, skills, conventions, or prior
     context, look up the relevant prompt-tagged notes first using tag
     tools such as `get_notes_with_all_tags` with tags like `prompt`
     plus the topic tag. Do not call `get_note_by_id` with a tag name.
   - Existing tags cotagged with the prompt tag are: %s.
   - For file resource notes, use `get_note_by_id` with
     `file:<absolute-path>` or exact `doc/<filename>` tags.
   - Direct current user instructions override stored memory for this
     session. However, if a prompt-tagged note states a CRITICAL,
     MUST, or NEVER convention and the current task explicitly asks for
     the opposite, call `ask_user` once to clarify which instruction to
     follow before proceeding.

3. Create task/journal notes only when useful or requested.
   - Use an `agent-task` note for long-running, restartable, org-task, or
     explicitly journaled work.
   - If you create a task note, append progress when you discover
     important facts, hit a problem, or make a decision.
   - For a small concrete edit, a final documentation note is enough when
     the task or convention expects memory documentation.

4. Create skill notes for reusable knowledge.
   - When the task asks you to store a skill, or when you discover a
     generally reusable pattern, convention, gotcha, best practice, or
     technique, create a skill note.
   - A skill note is tagged with `%s` plus one or more topic tags such as
     `elisp`, `performance`, `error-handling`, or a project name. Notes
     tagged with `%s` can be included as instructions on future related
     tasks.

In org-mode, you can link to a note with `[[ekg-note:<id>][<link display
text>]]`.  Tags can be linked with `[[ekg-tag:<tag>][<link display
text>]]`.

In markdown mode, there's no way to link directly to notes, but you can
use [[tag]] to link to a tag.

== SUBAGENT MEMORY HANDOFF ==

When delegating work to a sub-agent via `run_subagent`:
- **Before** spawning the subagent, ensure all relevant context
  (project conventions, decisions, file locations) is stored in ekg
  notes with appropriate tags so the subagent can find them.
- In the subagent instructions, mention which ekg tags to search for
  context (e.g., \"Check notes tagged with 'widget-maker' for project
  conventions\").
- The subagent has full access to ekg tools and follows the same memory
  workflow, so it will search for and create notes.

== FINAL STEPS (DO NOT SKIP) ==

When the task is complete:

- Verify the requested outcome.
- Create or update any requested task, memory, or skill notes.
- End immediately with `display_result_in_popup` or `end`. Do not keep
  investigating once the work is verified and requested notes are saved.

Use the `summarize_state` tool regularly to write brief but meaningful
status updates to the user-visible agent log window.  During long
tasks, do this at least once a minute, and also whenever your plan
changes or a step may take a while.  Each update should say what you
finished, what you are doing now, and what comes next or what is
blocked.

Do not narrate intended next actions as plain assistant text.  If the
user needs a progress update, call `summarize_state`, then call the
action tool needed for the next step.

Do not batch so much work between status updates that the user waits
more than a minute for the next one.  If a set of edits or tool calls
may take a while, break it into smaller chunks and call
`summarize_state` between chunks.  For repetitive file edits, prefer
working one file at a time unless you are confident the batch will stay
well under a minute.

The session may end if a total task timeout (%s) has been configured. Or,
some error may interrupt the processing. Because your processing can end
unexpected, you MUST occasionally write out your current state to an ekg
note (tag with `%s`, and a tag you choose to represent the
task). If you receive a timeout warning, do this immediately and then
finish. When executing a long-running task (more than a couple of tool
calls), start saving your state every few tool calls to a note.

When creating a note, text that you add will automatically have the tags
surrounding it to indicate that it was written by an LLM.  Do not add
these tags manually."
     (concat "[" (string-join (ekg-agent-cotagged-prompt-tags) " ") "]")
     ekg-llm-prompt-tag
     ekg-llm-prompt-tag
     timeout-desc
     ekg-agent-self-info-tag)))

(defun ekg-agent--load-org-instructions ()
  "Legacy function - kept for backwards compatibility.
This function now returns the default instructions as a fallback.")

(defun ekg-agent-instructions-evaluate-status ()
  "Return instructions for the agent to evaluate user status and help them."
  (format
   "Your goal is to help the user.  Act as a coach.  When
applicable, write notes to the user that will help them.  For example:

1. Check in with the user about their current progress on their goals.
2. Respond to the latest notes on their progress.
3. Point out a relevant note that the user may have forgotten about, or facts that they didn't know.
4. If you find gaps or errors in the notes, point them out to the user.
5. Suggest books, articles or resource papers that the user may find useful.

Provided below is a list of their recent notes and org-mode TODO
items (if any).  If you need to explore the notes, you can do that with
the tools available to you.  After each tool call you will be given a
chance to make more tool calls, one of which is to create a new note.
When you are done, you can call the end tool to indicate that you are
finished.  Try to make no more than 4 tool calls before calling the end
tool to finish your work.

Following these steps:

1. Based on the recent messages and org-mode tasks, see if there's
something obvious to follow up on.  If not, call the end tool.
2. If necessary to get more information, look at relevant tags or search.
This can be one or two tool calls but no more.
3. After getting more information, you may want to write a note to
yourself with relevant findings.
4. Write a note to the user, using the relevant tags, with the content
you think is appropriate.
5. Call the end tool to finish your work.

The date and time is %s.\n"
   (format-time-string "%F %R")))

(defun ekg-agent-evaluate-status ()
  "Run the ekg agent to evaluate status and help the user.
The agent will review recent notes and TODO items, then decide whether
to create new notes or perform other actions to help the user."
  (interactive)
  (ekg-agent--iterate (llm-make-chat-prompt
                       (ekg-agent-latest-notes-context)
                       :context
                       (concat (ekg-agent-instructions-intro) "\n"
                               (ekg-agent-instructions-evaluate-status))
                       :tools
                       (ekg-agent--tools (list ekg-agent-tool-end))
                       :tool-options (make-llm-tool-options :tool-choice 'any))
                      0
                      (ekg-agent--make-status-callback)
                      '("end")
                      nil))

(defun ekg-agent--prompt-user-text (prompt)
  "Return the first user text from PROMPT."
  (or (when-let ((interaction
                  (car (seq-filter
                        (lambda (interaction)
                          (eq (llm-chat-prompt-interaction-role interaction)
                              'user))
                        (llm-chat-prompt-interactions prompt)))))
        (format "%s" (llm-chat-prompt-interaction-content interaction)))
      "ekg agent task"))

(defun ekg-agent--fallback-prompt-id (prompt)
  "Return a deterministic short identifier for PROMPT."
  (let* ((words (split-string
                 (downcase
                  (replace-regexp-in-string
                   "[^[:alnum:]]+" " "
                   (ekg-agent--prompt-user-text prompt)))
                 "[[:space:]]+" t))
         (slug (string-join (seq-take words 5) "-")))
    (if (string-empty-p slug)
        "ekg-agent-task"
      slug)))

(defun ekg-agent--prompt-id (prompt)
  "From llm PROMPT, return a short identifier.
Falls back to a deterministic slug if the LLM naming call fails."
  (let (id)
    (condition-case err
        (llm-chat (ekg-agent--provider)
                  (llm-make-chat-prompt
                   (ekg-agent--prompt-user-text prompt)
                   :context "From user input of what they are instructing an agent to do, call the tool to report a short name."
                   :tools (list
                           (make-llm-tool
                            :function (lambda (result) (setq id result))
                            :name "report_id"
                            :description "Report the decided id so the agent can use it to name an Emacs status buffer. Use lowercase words separated by hyphens, about 3-5 words."
                            :args '((:name "id" :type string
                                           :description "Lowercase hyphenated buffer id, e.g. check-email-and-respond."))))
                   :tool-options (make-llm-tool-options :tool-choice 'any)))
      (error
       (message "ekg-agent prompt-id fallback: %s"
                (error-message-string err))))
    (if (and (stringp id) (not (string-empty-p id)))
        id
      (ekg-agent--fallback-prompt-id prompt))))

(defun ekg-agent--json-object (&rest pairs)
  "Return a JSON object from alternating string keys and values in PAIRS."
  (let ((object (make-hash-table :test 'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) object))
    object))

(defun ekg-agent--json-array (values)
  "Return VALUES as a JSON array vector."
  (apply #'vector values))

(defun ekg-agent--json-bool (value)
  "Return VALUE as a JSON boolean sentinel."
  (if value t :json-false))

(defun ekg-agent--json-safe-string (value)
  "Return VALUE as a property-free JSON-safe string."
  (replace-regexp-in-string
   "[\x3fff80-\x3fffff]" ""
   (substring-no-properties (format "%s" value))))

(defun ekg-agent--json-key (key)
  "Return KEY as a stable JSON object key."
  (cond
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)
   (t (ekg-agent--json-safe-string key))))

(defun ekg-agent--json-time (time)
  "Return TIME as an ISO-like timestamp string, or nil."
  (when (numberp time)
    (format-time-string "%FT%T%z" (seconds-to-time time))))

(defun ekg-agent--json-plist-p (value)
  "Return non-nil if VALUE looks like a keyword plist."
  (and (listp value)
       (zerop (mod (length value) 2))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun ekg-agent--json-alist-p (value)
  "Return non-nil if VALUE looks like an object-like alist."
  (and (consp value)
       (cl-every
        (lambda (cell)
          (and (consp cell)
               (not (keywordp (car cell)))
               (or (stringp (car cell))
                   (symbolp (car cell))
                   (numberp (car cell)))))
        value)))

(defun ekg-agent--debug-tool-use-to-json (tool-use)
  "Return a JSON object for TOOL-USE."
  (ekg-agent--json-object
   "id" (ekg-agent--json-safe-value
         (llm-provider-utils-tool-use-id tool-use))
   "name" (ekg-agent--json-safe-value
           (llm-provider-utils-tool-use-name tool-use))
   "args" (ekg-agent--json-safe-value
           (llm-provider-utils-tool-use-args tool-use))))

(defun ekg-agent--debug-tool-result-to-json (tool-result)
  "Return a JSON object for TOOL-RESULT."
  (let ((result (llm-chat-prompt-tool-result-result tool-result)))
    (ekg-agent--json-object
     "call_id" (ekg-agent--json-safe-value
                (llm-chat-prompt-tool-result-call-id tool-result))
     "tool_name" (ekg-agent--json-safe-value
                  (llm-chat-prompt-tool-result-tool-name tool-result))
     "result" (ekg-agent--json-safe-value result)
     "result_char_count" (if (stringp result) (length result) nil))))

(defun ekg-agent--json-hash-table (value)
  "Return a JSON-safe copy of hash table VALUE."
  (let ((object (make-hash-table :test 'equal)))
    (maphash (lambda (key item)
               (puthash (ekg-agent--json-key key)
                        (ekg-agent--json-safe-value item)
                        object))
             value)
    object))

(defun ekg-agent--json-plist (value)
  "Return VALUE, a plist, as a JSON object."
  (let ((object (make-hash-table :test 'equal)))
    (while value
      (puthash (ekg-agent--json-key (pop value))
               (ekg-agent--json-safe-value (pop value))
               object))
    object))

(defun ekg-agent--json-alist (value)
  "Return VALUE, an alist, as a JSON object."
  (let ((object (make-hash-table :test 'equal)))
    (dolist (cell value)
      (puthash (ekg-agent--json-key (car cell))
               (ekg-agent--json-safe-value (cdr cell))
               object))
    object))

(defun ekg-agent--json-safe-value (value)
  "Return VALUE converted to a structure accepted by `json-encode'."
  (cond
   ((null value) nil)
   ((eq value :json-false) :json-false)
   ((eq value t) t)
   ((stringp value) (ekg-agent--json-safe-string value))
   ((numberp value) value)
   ((llm-provider-utils-tool-use-p value)
    (ekg-agent--debug-tool-use-to-json value))
   ((llm-chat-prompt-tool-result-p value)
    (ekg-agent--debug-tool-result-to-json value))
   ((llm-multipart-p value)
    (ekg-agent--json-object
     "type" "multipart"
     "parts" (ekg-agent--json-array
              (mapcar #'ekg-agent--json-safe-value
                      (llm-multipart-parts value)))))
   ((llm-media-p value)
    (ekg-agent--json-object
     "type" "media"
     "mime_type" (llm-media-mime-type value)
     "byte_count" (length (llm-media-data value))))
   ((hash-table-p value) (ekg-agent--json-hash-table value))
   ((vectorp value)
    (ekg-agent--json-array
     (mapcar #'ekg-agent--json-safe-value (append value nil))))
   ((ekg-agent--json-plist-p value) (ekg-agent--json-plist value))
   ((ekg-agent--json-alist-p value) (ekg-agent--json-alist value))
   ((listp value)
    (ekg-agent--json-array (mapcar #'ekg-agent--json-safe-value value)))
   ((symbolp value) (symbol-name value))
   (t (ekg-agent--json-safe-string (format "%S" value)))))

(defun ekg-agent--debug-content-type (content)
  "Return a short type label for interaction CONTENT."
  (cond
   ((null content) "null")
   ((stringp content) "string")
   ((llm-multipart-p content) "multipart")
   ((and (listp content)
         (cl-some #'llm-provider-utils-tool-use-p content))
    "tool_uses")
   ((listp content) "list")
   ((vectorp content) "vector")
   (t (symbol-name (type-of content)))))

(defun ekg-agent--debug-tool-uses (content)
  "Return any tool-use structs from interaction CONTENT as JSON."
  (ekg-agent--json-array
   (if (listp content)
       (mapcar #'ekg-agent--debug-tool-use-to-json
               (seq-filter #'llm-provider-utils-tool-use-p content))
     nil)))

(defun ekg-agent--debug-interaction-multi-turn-plist (interaction)
  "Return provider multi-turn metadata from INTERACTION when available."
  (when (fboundp 'llm-chat-prompt-interaction-multi-turn-plist)
    (llm-chat-prompt-interaction-multi-turn-plist interaction)))

(defun ekg-agent--debug-interaction-to-json (interaction index)
  "Return a JSON object for PROMPT INTERACTION at INDEX."
  (let* ((role (llm-chat-prompt-interaction-role interaction))
         (content (llm-chat-prompt-interaction-content interaction))
         (tool-results
          (llm-chat-prompt-interaction-tool-results interaction)))
    (ekg-agent--json-object
     "index" index
     "role" (ekg-agent--json-safe-value role)
     "content_type" (ekg-agent--debug-content-type content)
     "content" (ekg-agent--json-safe-value content)
     "content_char_count" (if (stringp content) (length content) nil)
     "tool_uses" (ekg-agent--debug-tool-uses content)
     "tool_results"
     (ekg-agent--json-array
      (mapcar #'ekg-agent--debug-tool-result-to-json tool-results))
     "multi_turn_plist"
     (ekg-agent--json-safe-value
      (ekg-agent--debug-interaction-multi-turn-plist interaction)))))

(defun ekg-agent--debug-tool-to-json (tool)
  "Return a JSON object for TOOL."
  (ekg-agent--json-object
   "name" (llm-tool-name tool)
   "description" (llm-tool-description tool)
   "args" (ekg-agent--json-safe-value (llm-tool-args tool))
   "async" (ekg-agent--json-bool (llm-tool-async tool))))

(defun ekg-agent--debug-example-to-json (example)
  "Return a JSON object for prompt EXAMPLE."
  (ekg-agent--json-object
   "user" (ekg-agent--json-safe-value (car example))
   "assistant" (ekg-agent--json-safe-value (cdr example))))

(defun ekg-agent--debug-tool-options-to-json (tool-options)
  "Return TOOL-OPTIONS as a JSON object, or nil."
  (when tool-options
    (ekg-agent--json-object
     "tool_choice"
     (ekg-agent--json-safe-value
      (llm-tool-options-tool-choice tool-options)))))

(defun ekg-agent--debug-prompt-to-json (prompt)
  "Return PROMPT as a JSON object for debugging."
  (let ((context (llm-chat-prompt-context prompt))
        (interactions (llm-chat-prompt-interactions prompt)))
    (ekg-agent--json-object
     "context" (ekg-agent--json-safe-value context)
     "context_char_count" (if (stringp context) (length context) nil)
     "user_text" (ekg-agent--prompt-user-text prompt)
     "examples"
     (ekg-agent--json-array
      (mapcar #'ekg-agent--debug-example-to-json
              (llm-chat-prompt-examples prompt)))
     "interactions"
     (ekg-agent--json-array
      (cl-loop for interaction in interactions
               for index from 0
               collect
               (ekg-agent--debug-interaction-to-json interaction index)))
     "tools"
     (ekg-agent--json-array
      (mapcar #'ekg-agent--debug-tool-to-json
              (llm-chat-prompt-tools prompt)))
     "temperature" (ekg-agent--json-safe-value
                    (llm-chat-prompt-temperature prompt))
     "max_tokens" (ekg-agent--json-safe-value
                   (llm-chat-prompt-max-tokens prompt))
     "response_format" (ekg-agent--json-safe-value
                        (llm-chat-prompt-response-format prompt))
     "reasoning" (ekg-agent--json-safe-value
                  (llm-chat-prompt-reasoning prompt))
     "non_standard_params"
     (ekg-agent--json-safe-value
      (llm-chat-prompt-non-standard-params prompt))
     "tool_options"
     (ekg-agent--debug-tool-options-to-json
      (llm-chat-prompt-tool-options prompt)))))

(defun ekg-agent--debug-history-entry-to-json (entry index)
  "Return a JSON object for tool history ENTRY at INDEX."
  (let ((time (plist-get entry :time))
        (result (plist-get entry :result)))
    (ekg-agent--json-object
     "index" index
     "name" (ekg-agent--json-safe-value (plist-get entry :name))
     "args" (ekg-agent--json-safe-value (plist-get entry :args))
     "result" (ekg-agent--json-safe-value result)
     "result_char_count" (if (stringp result) (length result) nil)
     "time" time
     "time_iso" (ekg-agent--json-time time))))

(defun ekg-agent--debug-config-to-json ()
  "Return agent configuration values relevant to debugging."
  (ekg-agent--json-object
   "timeout_seconds" ekg-agent-timeout-seconds
   "status_reminder_seconds" ekg-agent-status-reminder-seconds
   "llm_max_retries" ekg-agent-llm-max-retries
   "llm_retry_base_delay" ekg-agent-llm-retry-base-delay
   "llm_max_tokens" (ekg-agent--json-safe-value ekg-agent-llm-max-tokens)
   "code_command" (ekg-agent--json-safe-value ekg-agent-code-command)
   "code_command_prompt_method"
   (ekg-agent--json-safe-value ekg-agent-code-command-prompt-method)
   "web_max_chars" ekg-agent-web-max-chars
   "emacs_reference_max_chars" ekg-agent-emacs-reference-max-chars
   "log_buffer_name_format" ekg-agent-log-buffer-name-format))

(defun ekg-agent--debug-session-to-json ()
  "Return the current agent log buffer state as a JSON object."
  (unless ekg-agent--prompt
    (user-error "No agent prompt available in this buffer"))
  (let* ((log-text (buffer-substring-no-properties (point-min) (point-max)))
         (origin-name (and (buffer-live-p ekg-agent--origin-buffer)
                           (buffer-name ekg-agent--origin-buffer)))
         (origin-directory
          (and (buffer-live-p ekg-agent--origin-buffer)
               (buffer-local-value 'default-directory
                                   ekg-agent--origin-buffer)))
         (history (reverse ekg-agent--tool-call-history)))
    (ekg-agent--json-object
     "schema" "ekg-agent-debug-session"
     "schema_version" 1
     "generated_at" (format-time-string "%FT%T%z")
     "emacs_version" emacs-version
     "session"
     (ekg-agent--json-object
      "buffer_name" (buffer-name)
      "running" (ekg-agent--json-bool ekg-agent--running-p)
      "cancelled" (ekg-agent--json-bool ekg-agent--cancelled-p)
      "current_activity" (ekg-agent--json-safe-value
                          ekg-agent--current-activity)
      "origin_buffer_name" (ekg-agent--json-safe-value origin-name)
      "origin_default_directory" (ekg-agent--json-safe-value
                                  origin-directory)
      "current_default_directory" (ekg-agent--json-safe-value
                                   default-directory)
      "end_tools" (ekg-agent--json-safe-value ekg-agent--end-tools)
      "last_status_update_time" ekg-agent--last-status-update-time
      "last_status_update_time_iso"
      (ekg-agent--json-time ekg-agent--last-status-update-time)
      "last_status_reminder_time" ekg-agent--last-status-reminder-time
      "last_status_reminder_time_iso"
      (ekg-agent--json-time ekg-agent--last-status-reminder-time)
      "active_tool_process_count" (length ekg-agent--tool-processes)
      "current_request" (ekg-agent--json-safe-value
                         (and ekg-agent--current-request
                              (format "%S" ekg-agent--current-request))))
     "configuration" (ekg-agent--debug-config-to-json)
     "prompt" (ekg-agent--debug-prompt-to-json ekg-agent--prompt)
     "tool_call_history"
     (ekg-agent--json-array
      (cl-loop for entry in history
               for index from 0
               collect (ekg-agent--debug-history-entry-to-json
                        entry index)))
     "log"
     (ekg-agent--json-object
      "text" (ekg-agent--json-safe-string log-text)
      "char_count" (length log-text)
      "line_count" (length (split-string log-text "\n"))))))

(defun ekg-agent-export-debug-json (&optional file)
  "Write current agent prompt and log state to FILE as JSON.
When FILE is nil, write to a new temporary file.  Return the file
path.  The command is intended to be run from an agent log buffer."
  (interactive)
  (let* ((path (or file (make-temp-file "ekg-agent-debug-" nil ".json")))
         (payload (ekg-agent--debug-session-to-json))
         (coding-system-for-write 'utf-8-unix)
         (json-encoding-pretty-print t))
    (with-temp-file path
      (insert (json-encode payload))
      (insert "\n"))
    (when (called-interactively-p 'interactive)
      (kill-new path)
      (let ((ekg-agent--current-log-buffer (current-buffer)))
        (ekg-agent--log "Debug JSON exported to %s" path))
      (message "Wrote EKG agent debug JSON to %s" path))
    path))

(defvar ekg-agent-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ekg-agent-continue)
    (define-key map (kbd "C-c C-k") #'ekg-agent-cancel)
    (define-key map (kbd "C-c C-q") #'ekg-agent-force-cancel)
    (define-key map (kbd "C-c C-d") #'ekg-agent-export-debug-json)
    map)
  "Keymap for `ekg-agent-log-mode'.")

(define-minor-mode ekg-agent-log-mode
  "Minor mode for ekg agent log buffers.
Provides commands to continue or cancel agent work.

\\{ekg-agent-log-mode-map}"
  :lighter (:eval (if ekg-agent--running-p " Agent:run" " Agent:idle"))
  :keymap ekg-agent-log-mode-map)

(defun ekg-agent--set-stopped (log-buf)
  "Mark the agent as no longer running in LOG-BUF.
Return non-nil if the agent was actually running and is now stopped.
This acts as a once-only guard to ensure status callbacks fire exactly once."
  (when (and log-buf (buffer-live-p log-buf))
    (with-current-buffer log-buf
      (when ekg-agent--running-p
        (setq ekg-agent--running-p nil)
        (setq ekg-agent--current-request nil)
        (setq ekg-agent--current-activity nil)
        (force-mode-line-update)
        t))))

(defun ekg-agent--cancel-current-work ()
  "Cancel the current LLM request and any active tool subprocesses."
  (when ekg-agent--current-request
    (ignore-errors (llm-cancel-request ekg-agent--current-request))
    (setq ekg-agent--current-request nil))
  (dolist (item ekg-agent--tool-processes)
    (cond
     ((futur-p item)
      (ignore-errors (futur-abort item "cancelled")))
     ((processp item)
      (when (process-live-p item)
        (ignore-errors (delete-process item))))))
  (setq ekg-agent--tool-processes nil))

(defun ekg-agent--make-deadline-timer (log-buf deadline status-callback)
  "Return a timer that stops LOG-BUF at DEADLINE.
STATUS-CALLBACK is called with a timeout status when the timer
stops a still-running agent."
  (when deadline
    (run-at-time
     (max 0.1 (- deadline (float-time))) nil
     (lambda ()
       (when (and (buffer-live-p log-buf)
                  (buffer-local-value 'ekg-agent--running-p log-buf))
         (with-current-buffer log-buf
           (let ((ekg-agent--current-log-buffer log-buf))
             (ekg-agent--log "Timeout reached; stopping agent.")
             (ekg-agent--cancel-current-work)
             (when ekg-agent--prompt
               (ekg-agent--clean-orphaned-tool-interactions
                ekg-agent--prompt))
             (when (ekg-agent--set-stopped log-buf)
               (when status-callback
                 (funcall status-callback "stopped by timeout"))))))))))

(defun ekg-agent--handle-llm-result (result prompt iteration-num status-callback
                                            end-tools deadline timeout-final
                                            log-buf provider)
  "Handle async LLM RESULT for PROMPT at ITERATION-NUM.
STATUS-CALLBACK, END-TOOLS, DEADLINE, TIMEOUT-FINAL, LOG-BUF, and
PROVIDER are the active loop state."
  (if (and (buffer-live-p log-buf)
           (buffer-local-value 'ekg-agent--cancelled-p log-buf))
      (when (ekg-agent--set-stopped log-buf)
        (ekg-agent--log "Agent stopped (cancelled by user)")
        (when status-callback
          (funcall status-callback "stopped by user")))
    (let ((result-alist (plist-get result :tool-results))
          (end-tools (or end-tools '("end"))))
      (unless result-alist
        ;; Everything must be a tool call.  If the model emits plain
        ;; text, log it and continue with a direct instruction to use a
        ;; completion tool.
        (ekg-agent--log "%s" (plist-get result :text))
        (llm-chat-prompt-append-response
         prompt
         (format "That has been communicated to the user. Please always call tools. This session cannot end until you call one of the following tools: %s."
                 (string-join end-tools ", "))))
      (cond
       (timeout-final
        (when (ekg-agent--set-stopped log-buf)
          (when status-callback
            (funcall status-callback "stopped by timeout"))))
       ((seq-find (lambda (end-tool)
                    (assoc-default end-tool result-alist))
                  end-tools)
        (when (ekg-agent--set-stopped log-buf)
          (when status-callback
            (funcall
             status-callback
             (mapconcat (lambda (end-tool)
                          (format "%s"
                                  (assoc-default end-tool result-alist)))
                        (seq-filter
                         (lambda (end-tool)
                           (assoc-default end-tool result-alist))
                         end-tools)
                        ", ")))))
       (t
        (with-current-buffer log-buf
          (ekg-agent--iterate prompt
                              (+ 1 iteration-num)
                              status-callback
                              end-tools
                              deadline
                              timeout-final
                              provider)))))))

(defun ekg-agent--iterate (prompt iteration-num &optional status-callback
                                  end-tools deadline timeout-final provider)
  "Run an iteration of the ekg agent with PROMPT and ITERATION-NUM.

PROMPT is the chat prompt for the LLM.

ITERATION-NUM is the current iteration count.  0 is the setup iteration,
1 is the first iteration in which we call start the agentic loop.

STATUS-CALLBACK is called exactly once when the agent finishes.  The
argument is a string with the end-tool result on success, or the symbol
\\='error on failure.

END-TOOLS is a list of tool names that will end the iteration.

DEADLINE is a float time when the agent should stop.

If TIMEOUT-FINAL is non-nil, this is the final iteration before
stopping.

PROVIDER is the LLM provider to use.  If nil, use
`ekg-agent--provider'.

This is to start, and after every tool call to continue the agent
session.  At iteration 0 the log buffer is created and
`ekg-agent-log-mode' is enabled."
  (if (= iteration-num 0)
      ;; Set up everything.  Capture the current buffer as the origin
      ;; buffer before switching to the log buffer.
      (let* ((origin-buf (current-buffer))
             (id (ekg-agent--prompt-id prompt))
             (deadline (or deadline (ekg-agent--timeout-deadline)))
             (buf (get-buffer-create (format ekg-agent-log-buffer-name-format id))))
        (with-current-buffer buf
          (erase-buffer)
          ;; Set up major mode first since it calls
          ;; `kill-all-local-variables'.
          (when (featurep 'markdown-mode)
            (markdown-mode)
            (when (featurep 'flycheck)
              (flycheck-mode 0))
            (when (featurep 'flymake)
              (flymake-mode 0)))
          (let ((ekg-agent--current-log-buffer buf))
            (ekg-agent--log-session-start (buffer-name)))
          (insert (format "Agent session for: %s\n\n" id))
          (setq ekg-agent--prompt prompt)
          (setq ekg-agent--end-tools end-tools)
          (setq ekg-agent--running-p t)
          (setq ekg-agent--cancelled-p nil)
          (setq ekg-agent--current-request nil)
          (setq ekg-agent--tool-processes nil)
          (setq ekg-agent--tool-call-history nil)
          (setq ekg-agent--status-callback status-callback)
          (setq ekg-agent--origin-buffer origin-buf)
          (setq ekg-agent--last-status-update-time (float-time))
          (setq ekg-agent--last-status-reminder-time nil)
          (ekg-agent-log-mode 1)
          (setq header-line-format
                '(:eval (ekg-agent--format-header-line)))
          (ekg-agent--wrap-prompt-tools prompt buf origin-buf)
          (goto-char (point-min))
          (ekg-agent--iterate prompt 1
                              status-callback
                              end-tools
                              deadline
                              timeout-final
                              provider)))
    ;; iteration > 0: run the agent loop.  Prefer the dynamically
    ;; bound log buffer when present; async tools such as sub-agents can
    ;; enter here while `current-buffer' is the origin buffer.
    (let* ((log-buf (if (and ekg-agent--current-log-buffer
                             (buffer-live-p ekg-agent--current-log-buffer))
                        ekg-agent--current-log-buffer
                      (current-buffer)))
           (ekg-agent--current-log-buffer log-buf)
           (expired (and deadline (> (float-time) deadline)))
           (deadline-timer nil))
      (when (and expired (not timeout-final))
        (ekg-agent--log "Timeout reached; requesting final state note.")
        (ekg-agent--prompt-append-user-message
         prompt (ekg-agent--timeout-warning-message))
        (setq timeout-final t))
      (ekg-agent--maybe-remind-status-update prompt)
      (when (and (integerp ekg-agent-llm-max-tokens)
                 (> ekg-agent-llm-max-tokens 0)
                 (not (llm-chat-prompt-max-tokens prompt)))
        (setf (llm-chat-prompt-max-tokens prompt)
              ekg-agent-llm-max-tokens))
      (when (and log-buf (buffer-live-p log-buf))
        (with-current-buffer log-buf
          (setq ekg-agent--current-activity "Waiting for LLM response…")
          (force-mode-line-update)
          (ekg-agent--log "⟳ Waiting for LLM response…")))
      (condition-case err
          (let* ((request
                  (progn
                    (setq deadline-timer
                          (ekg-agent--make-deadline-timer
                           log-buf deadline status-callback))
                    (ekg-agent--llm-chat-async-with-retry
                     (or provider (ekg-agent--provider))
                     prompt
                     (lambda (result)
                       (when deadline-timer
                         (cancel-timer deadline-timer)
                         (setq deadline-timer nil))
                       (let ((ekg-agent--current-log-buffer log-buf))
                         (condition-case err
                             (ekg-agent--handle-llm-result
                              result
                              prompt
                              iteration-num
                              status-callback
                              end-tools
                              deadline
                              timeout-final
                              log-buf
                              provider)
                           (error
                            (when (ekg-agent--set-stopped log-buf)
                              (ekg-agent--log "Error in callback: %s"
                                              (error-message-string err))
                              (when status-callback
                                (funcall status-callback 'error)))))))
                     (lambda (_ err)
                       (when deadline-timer
                         (cancel-timer deadline-timer)
                         (setq deadline-timer nil))
                       (let ((ekg-agent--current-log-buffer log-buf))
                         (if (ekg-agent--recover-unknown-tool-error
                              err prompt log-buf)
                             (with-current-buffer log-buf
                               (ekg-agent--iterate prompt
                                                   (+ 1 iteration-num)
                                                   status-callback
                                                   end-tools
                                                   deadline
                                                   timeout-final
                                                   provider))
                           (when (ekg-agent--set-stopped log-buf)
                             (ekg-agent--log "LLM error: %s" err)
                             (when status-callback
                               (funcall status-callback 'error))))))
                     t
                     log-buf))))
            (when (buffer-live-p log-buf)
              (with-current-buffer log-buf
                (if ekg-agent--running-p
                    (setq ekg-agent--current-request request)
                  (ignore-errors (llm-cancel-request request))))))
        (error
         (when deadline-timer
           (cancel-timer deadline-timer))
         (when (ekg-agent--set-stopped log-buf)
           (ekg-agent--log "Error starting LLM request: %s"
                           (error-message-string err))
           (when status-callback (funcall status-callback 'error))))))))

(defun ekg-agent-continue (message)
  "Continue the agent from where it left off.
Prompts for a MESSAGE with additional instructions for the agent."
  (interactive (list (read-string "Instructions for agent: ")))
  (unless ekg-agent--prompt
    (user-error "No agent prompt available to continue"))
  (when ekg-agent--running-p
    (user-error "Agent is already running"))
  (setq ekg-agent--cancelled-p nil)
  (setq ekg-agent--running-p t)
  (setq ekg-agent--current-request nil)
  (setq ekg-agent--tool-processes nil)
  (force-mode-line-update)
  (ekg-agent--clean-orphaned-tool-interactions ekg-agent--prompt)
  (let ((cb (ekg-agent--make-status-callback))
        (ekg-agent--current-log-buffer (current-buffer)))
    (setq ekg-agent--status-callback cb)
    (setq ekg-agent--last-status-update-time (float-time))
    (setq ekg-agent--last-status-reminder-time nil)
    (ekg-agent--log-session-start "Continuing agent")
    (when (and message (not (string-empty-p message)))
      (ekg-agent--prompt-append-user-message ekg-agent--prompt message)
      (ekg-agent--log "User message: %s" message))
    (ekg-agent--iterate ekg-agent--prompt
                        1
                        cb
                        ekg-agent--end-tools
                        (ekg-agent--timeout-deadline))))

(defun ekg-agent-cancel ()
  "Cancel the current agent run, preserving the prompt for continuation.
The agent will stop after the current in-flight LLM call completes.
Use \\[ekg-agent-continue] to resume."
  (interactive)
  (unless ekg-agent--running-p
    (user-error "No agent is currently running"))
  (setq ekg-agent--cancelled-p t)
  (let ((ekg-agent--current-log-buffer (current-buffer)))
    (ekg-agent--log "Cancellation requested; will stop after current tool call")))

(defun ekg-agent-force-cancel ()
  "Force-cancel the current agent run by killing in-flight requests.
Unlike `ekg-agent-cancel', which waits for the current operation to
finish, this immediately kills the LLM request and any tool
subprocesses.  Use \\[ekg-agent-continue] to resume."
  (interactive)
  (unless ekg-agent--running-p
    (user-error "No agent is currently running"))
  (setq ekg-agent--cancelled-p t)
  (ekg-agent--cancel-current-work)
  ;; Clean up orphaned tool-use interactions from the prompt.
  (when ekg-agent--prompt
    (ekg-agent--clean-orphaned-tool-interactions ekg-agent--prompt))
  ;; Mark as stopped and fire the status callback exactly once.
  (let ((log-buf (current-buffer))
        (ekg-agent--current-log-buffer (current-buffer)))
    (when (ekg-agent--set-stopped log-buf)
      (ekg-agent--log "Agent force-cancelled")
      (when ekg-agent--status-callback
        (funcall ekg-agent--status-callback "force cancelled")))))

(defun ekg-agent-evaluate-status-daily ()
  "Run the ekg agent to evaluate status as a daily routine."
  (interactive)
  (message "Running the daily ekg agent...")
  (ekg-agent-evaluate-status))

(defun ekg-agent--parse-time (time-string)
  "Parse TIME-STRING in HH:MM format and return seconds until that time today.
If the time has already passed today, return seconds until that time tomorrow."
  (unless (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'" time-string)
    (error "Invalid time format: %s (expected HH:MM)" time-string))
  (let* ((hour (string-to-number (match-string 1 time-string)))
         (minute (string-to-number (match-string 2 time-string)))
         (now (decode-time))
         (target-time (encode-time 0 minute hour
                                   (nth 3 now) (nth 4 now) (nth 5 now))))
    (when (or (< hour 0) (>= hour 24) (< minute 0) (>= minute 60))
      (error "Invalid time: %s (hour must be 0-23, minute 0-59)" time-string))
    (let ((seconds-until (- (float-time target-time) (float-time))))
      (if (< seconds-until 0)
          ;; Time has passed today, schedule for tomorrow
          (+ seconds-until 86400)
        seconds-until))))

(defun ekg-agent-schedule-daily ()
  "Schedule the daily agent evaluation to run at `ekg-agent-daily-time'.
If already scheduled, cancel the existing timer and create a new one."
  (interactive)
  (ekg-agent-cancel-daily)
  (let ((seconds-until (ekg-agent--parse-time ekg-agent-daily-time)))
    (setq ekg-agent--daily-timer
          (run-at-time seconds-until 86400 #'ekg-agent-evaluate-status-daily))
    (message "EKG agent scheduled to run daily at %s" ekg-agent-daily-time)))

(defun ekg-agent-cancel-daily ()
  "Cancel the scheduled daily agent evaluation."
  (interactive)
  (when ekg-agent--daily-timer
    (cancel-timer ekg-agent--daily-timer)
    (setq ekg-agent--daily-timer nil)
    (message "EKG agent daily schedule cancelled")))

(defun ekg-agent-note-response (&optional arg)
  "Respond to the current note using the agent.
This is similar to `ekg-llm-send-and-append-note', but runs an
agent loop with tools, instead of just appending text.

The agent is given the context of the last 10 notes with similar
tags.

ARG, if non-nil, allows editing the instructions."
  (interactive "P")
  (unless ekg-note
    (error "No note in current buffer"))
  (ekg-note-update-from-buffer)
  (save-excursion
    (let* ((ekg-agent-tool-append-response
            (make-llm-tool
             :function (lambda (content)
                         (let* ((enclosure (assoc-default major-mode ekg-llm-format-output nil '("_BEGIN_" . "_END_")))
                                (new-text (concat
                                           (car enclosure) "\n"
                                           content "\n"
                                           (cdr enclosure))))
                           (save-excursion
                             (goto-char (point-max))
                             (insert new-text))))
             :name "append_to_current_note"
             :description "Append content to the current note."
             :args '((:name "content" :type string :description "The content to append to the current note."))))
           (ekg-agent-tool-replace-response
            (make-llm-tool
             :function (lambda (content)
                         (let* ((enclosure (assoc-default major-mode ekg-llm-format-output nil '("_BEGIN_" . "_END_")))
                                (new-text (concat
                                           (car enclosure) "\n"
                                           content "\n"
                                           (cdr enclosure))))
                           (erase-buffer)
                           (insert new-text)))
             :name "replace_current_note"
             :description "Replace the content of the current note."
             :args '((:name "content" :type string :description "The new content for the current note."))))
           (instructions (ekg-llm-instructions-for-note ekg-note))
           (instructions-for-use (if arg
                                     (read-string "Instructions: " instructions)
                                   instructions))
           (context-notes (seq-take (seq-remove (lambda (n) (equal (ekg-note-id n) (ekg-note-id ekg-note)))
                                                (ekg-get-notes-with-any-tags
                                                 (append
                                                  (ekg-note-tags ekg-note)
                                                  (list ekg-agent-self-info-tag))))
                                    10))
           (context-notes-json (let ((ekg-llm-note-numwords 100))
                                 (mapconcat #'ekg-llm-note-to-text context-notes "\n\n")))
           (current-note-json (let ((ekg-llm-note-numwords 10000))
                                (ekg-llm-note-to-text ekg-note)))
           (prompt (concat "You are a note-response agent for ekg, an Emacs knowledge base.
Your job is to respond to the user's current note by appending or
replacing its content.  You have tools to search existing notes for
context if needed, but your primary goal is to produce a response
for the current note.

Do NOT create separate notes (via `create_note`) unless there is a
compelling reason — your response belongs in the current note.

Your instructions:\n"
                           instructions-for-use
                           "\n\nYou have access to tools to search and read existing notes
for context.  After each tool call you will be given a chance to make
more tool calls.

IMPORTANT: You MUST end your session by calling one of these two tools:
- `append_to_current_note`: Appends your response to the current note.
- `replace_current_note`: Replaces the current note content entirely.

Calling either of these tools will end your session.  There is no other
way to end the session.  Prefer `append_to_current_note` by default,
unless the user is explicitly asking for a rewrite or replacement.

The user input will be the note they are currently editing.\n\n"
                           (format "The current date and time is %s.\n"
                                   (format-time-string "%F %R"))
                           (format "Some notes matching the tags or context: %s\n"
                                   context-notes-json))))
      (let ((overlay (make-overlay (point-max) (point-max) nil t t)))
        (overlay-put overlay 'after-string (propertize " [LLM response computing]" 'face 'shadow))
        (ekg-agent--iterate (llm-make-chat-prompt
                             current-note-json
                             :context prompt
                             :tools
                             (ekg-agent--tools (list
                                                ekg-agent-tool-append-response
                                                ekg-agent-tool-replace-response))
                             :tool-options (make-llm-tool-options :tool-choice 'any))
                            0
                            (ekg-agent--make-status-callback
                             (lambda (_status)
                               (delete-overlay overlay)))
                            '("append_to_current_note" "replace_current_note")
                            nil)))))

(defvar ekg-agent-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ekg-llm-capture-mode-map)
    (define-key map (kbd "C-c .") #'ekg-agent-note-response)
    map)
  "Keymap for ekg-agent bindings in capture and edit modes.
Inherits from `ekg-llm-capture-mode-map' but overrides
\\[ekg-agent-note-response] to use the agent instead of the plain LLM.")

(define-minor-mode ekg-agent-minor-mode
  "Minor mode providing agent keybindings in ekg capture/edit buffers.
This replaces `ekg-llm-minor-mode' when `ekg-agent' is loaded."
  :lighter nil
  :keymap ekg-agent-minor-mode-map)

;; Replace ekg-llm-minor-mode with ekg-agent-minor-mode on the hooks.
(remove-hook 'ekg-capture-mode-hook #'ekg-llm-minor-mode)
(remove-hook 'ekg-edit-mode-hook #'ekg-llm-minor-mode)
(add-hook 'ekg-capture-mode-hook #'ekg-agent-minor-mode)
(add-hook 'ekg-edit-mode-hook #'ekg-agent-minor-mode)

(defun ekg-agent-show-internal-notes ()
  "Show notes with agent internal tags."
  (interactive)
  (ekg-show-notes-with-tag ekg-agent-self-info-tag))

;;;###autoload
(defun ekg-agent-add-note (text tags mode)
  "Add a note from an agent with TEXT, TAGS, and MODE.

TAGS should be a list of tag strings.  MODE should be a symbol like
`org-mode', `markdown-mode', or `text-mode'.  Returns the note
ID on success, signals an error on failure.

This function automatically:
- Adds the `ekg-agent-author-tag' to the tags
- Applies all functions from `ekg-capture-auto-tag-funcs' (e.g., date tags)
- Wraps the text in the appropriate LLM output format based on MODE

This is intended to be used from the command-line so agents can easily
add properly formatted notes to ekg."
  (ekg-connect)
  (let* ((mode-sym (if (stringp mode) (intern mode) mode))
         (enclosure (assoc-default mode-sym ekg-llm-format-output nil '("_BEGIN_" . "_END_")))
         (formatted-text (concat (car enclosure) "\n"
                                 text "\n"
                                 (cdr enclosure)))
         ;; Apply auto-tag functions (e.g., date tags)
         (auto-tags (mapcan (lambda (f) (funcall f)) ekg-capture-auto-tag-funcs))
         (all-tags (seq-uniq (append tags auto-tags (list ekg-agent-author-tag))))
         (note (ekg-note-create :text formatted-text
                                :mode mode-sym
                                :tags all-tags)))
    (ekg-save-note note)
    (message "Agent note created with ID: %s" (ekg-note-id note))
    (ekg-note-id note)))

(defun ekg-agent--filter-properties (props)
  "Filter PROPS plist, removing large internal properties like embeddings."
  (let (result)
    (cl-loop for (key val) on props by #'cddr
             unless (eq key :embedding/embedding)
             do (setq result (plist-put result key val)))
    result))

(defun ekg-agent--note-to-alist (note &optional max-words)
  "Convert NOTE to an alist suitable for JSON encoding.
If MAX-WORDS is specified, truncate the text to that many words."
  (let* ((ekg-truncation-method 'word)
         (text (or (ekg-note-text note) ""))
         (truncated-text (if (and max-words (> (length text) 0))
                             (ekg-truncate-at text max-words "…")
                           text)))
    `((id . ,(ekg-note-id note))
      (text . ,truncated-text)
      (mode . ,(symbol-name (ekg-note-mode note)))
      (tags . ,(ekg-note-tags note))
      (creation_time . ,(ekg-note-creation-time note))
      (modified_time . ,(ekg-note-modified-time note))
      (properties . ,(ekg-agent--filter-properties (ekg-note-properties note))))))

(defun ekg-agent--notes-to-json (notes &optional max-words)
  "Convert list of NOTES to a JSON array string.
If MAX-WORDS is specified, truncate each note's text to that many words."
  (json-encode (mapcar (lambda (note)
                         (ekg-agent--note-to-alist note max-words))
                       notes)))

(cl-defun ekg-agent--get-notes (&key tags any-tags note-id semantic-search text-search latest (num 10))
  "Get notes from ekg based on search criteria.

This is a helper function that returns a list of note objects.
Use `ekg-agent-read-notes' for CLI access with JSON output.

This function supports multiple modes of operation:

1. By tags (AND): Provide TAGS as a list of tag strings.
2. By tags (OR): Provide ANY-TAGS as a list of tag strings.
3. By note ID: Provide NOTE-ID as a number or string.
4. By semantic search: Provide SEMANTIC-SEARCH as a query string.
5. By text search: Provide TEXT-SEARCH as a query string.
6. Latest modified: Provide LATEST as non-nil.

NUM is the maximum number of notes to return (default 10).

Returns a list of note objects."
  (ekg-connect)
  ;; The agent must be able to find its own self-info notes even when
  ;; searching by other tags.  ekg-hidden-tags causes notes with
  ;; agent/self-info to be filtered out of normal tag queries, so we
  ;; remove that tag from the hidden list within agent tool context.
  (let ((ekg-hidden-tags (remove ekg-agent-self-info-tag ekg-hidden-tags)))
    (cond
     ;; Latest modified notes
     (latest
      (ekg-get-latest-modified num))

     ;; Read by note ID
     (note-id
      (let ((note (ekg-get-note-with-id (if (stringp note-id)
                                            (string-to-number note-id)
                                          note-id))))
        (if note
            (list note)
          (error "Note with ID %s not found" note-id))))

     ;; Semantic search (requires embeddings)
     (semantic-search
      (unless ekg-embedding-provider
        (error "Semantic search requires ekg-embedding-provider to be configured"))
      (let ((embedding (llm-embedding ekg-embedding-provider semantic-search)))
        (delq nil (mapcar #'ekg-get-note-with-id
                          (ekg-embedding-n-most-similar-notes embedding num)))))

     ;; Text search (full-text search)
     (text-search
      (seq-take
       (seq-filter #'ekg-note-active-p
                   (delq nil (mapcar #'ekg-get-note-with-id
                                     (triples-fts-query-subject ekg-db text-search ekg-query-pred-abbrevs))))
       num))

     ;; Tag-based search with OR logic (already sorted by creation time)
     (any-tags
      (seq-take (ekg-get-notes-with-any-tags any-tags) num))

     ;; Tag-based search (AND logic)
     (tags
      (seq-take (sort (ekg-get-notes-with-tags tags)
                      #'ekg-sort-by-creation-time)
                num))

     ;; No search criteria
     (t
      (error "Must provide tags, any-tags, note-id, semantic-search, text-search, or latest")))))

;;;###autoload
(cl-defun ekg-agent-read-notes (&key tags note-id semantic-search text-search latest (num 10) (max-words 100))
  "Read notes from ekg and return as JSON.

This function supports multiple modes of operation:

1. By tags (AND): Provide TAGS as a list of tag strings.
2. By note ID: Provide NOTE-ID as a number or string.
3. By semantic search: Provide SEMANTIC-SEARCH as a query string.
4. By text search: Provide TEXT-SEARCH as a query string.
5. Latest modified: Provide LATEST as non-nil.

NUM is the maximum number of notes to return (default 10).
MAX-WORDS is the maximum number of words per note text (default 100).

Returns a JSON string containing an array of note objects.
Each note object contains: id, text, mode, tags, creation_time, modified_time.

This is intended to be used from the command-line so agents can read
notes from ekg."
  (let ((notes (ekg-agent--get-notes :tags tags
                                     :note-id note-id
                                     :semantic-search semantic-search
                                     :text-search text-search
                                     :latest latest
                                     :num num)))
    (ekg-agent--notes-to-json notes max-words)))

;; ekg-org integration

(defun ekg-agent-org--normalize-tool-tags (tags)
  "Return TAGS as a plain list of strings."
  (cond
   ((vectorp tags) (append tags nil))
   ((listp tags) tags)
   ((stringp tags) (list tags))
   (t nil)))

(defun ekg-agent-org--save-item (title content tags parent-id status
                                       deadline scheduled)
  "Create TITLE under PARENT-ID with CONTENT and TAGS, returning its note ID."
  (let* ((status (if (and status (not (string-empty-p status)))
                     status
                   (ekg-agent--org-default-todo-keyword)))
         (has-parent (and parent-id
                          (not (equal parent-id 0))
                          (not (string-empty-p (format "%s" parent-id)))))
         (note (ekg-note-create
                :text (or content "")
                :tags (cl-remove-duplicates
                       (append (list ekg-org-task-tag)
                               (ekg-agent-org--normalize-tool-tags tags)
                               (list (concat ekg-org-state-tag-prefix
                                             (downcase status))))
                       :test #'string=)
                :properties (list :titled/title (list title)))))
    (when has-parent
      (setf (ekg-note-properties note)
            (plist-put (ekg-note-properties note) :org/parent parent-id)))
    ;; Assign sort-order at the end of existing siblings.
    (let* ((siblings (if has-parent
                         (ekg-org-view--sorted-children parent-id)
                       (ekg-org-view--sorted-top-level)))
           (last-id (when siblings
                      (ekg-note-id (car (last siblings)))))
           (sort-order (if siblings
                           (ekg-org-view--assign-order-after siblings last-id)
                         0)))
      (setf (ekg-note-properties note)
            (plist-put (ekg-note-properties note) :org/sort-order sort-order)))
    (when (and deadline (not (string-empty-p deadline)))
      (setf (ekg-note-properties note)
            (plist-put (ekg-note-properties note)
                       :org/deadline (ekg-org--to-timestamp deadline))))
    (when (and scheduled (not (string-empty-p scheduled)))
      (setf (ekg-note-properties note)
            (plist-put (ekg-note-properties note)
                       :org/scheduled (ekg-org--to-timestamp scheduled))))
    (ekg-save-note note)
    (ekg-note-id note)))

(defun ekg-agent-org--org-note-title (content)
  "Return the #+TITLE from CONTENT, if present."
  (let ((case-fold-search t))
    (when (string-match "^#\\+TITLE:[\t ]*\\(.+\\)$" content)
      (string-trim (match-string 1 content)))))

(defun ekg-agent-org--clean-heading-title (title)
  "Strip trailing org tags from heading TITLE."
  (string-trim
   (replace-regexp-in-string
    "[\t ]+:[[:alnum:]_@#%]+\\(?::[[:alnum:]_@#%]+\\)*:[\t ]*\\'"
    ""
    title)))

(defun ekg-agent-org--parse-note-headings (content)
  "Parse org task headings from CONTENT.
Return a list of plists with :level, :status, :title, and :content."
  (let ((todo-regexp (or (ekg-agent--org-todo-keyword-regexp)
                         (regexp-quote
                          (ekg-agent--org-default-todo-keyword))))
        (default-status (ekg-agent--org-default-todo-keyword))
        headings current body)
    (dolist (line (split-string content "\n"))
      (if (string-match
           (format "^\\(\\*+\\)[\t ]+\\(?:\\(%s\\)[\t ]+\\)?\\(.+\\)$"
                   todo-regexp)
           line)
          (progn
            (when current
              (setq current
                    (plist-put current :content
                               (string-trim
                                (string-join (nreverse body) "\n"))))
              (push current headings))
            (setq current
                  (list :level (length (match-string 1 line))
                        :status (or (match-string 2 line) default-status)
                        :title (ekg-agent-org--clean-heading-title
                                (match-string 3 line))
                        :content ""))
            (setq body nil))
        (when current
          (push line body))))
    (when current
      (setq current
            (plist-put current :content
                       (string-trim (string-join (nreverse body) "\n"))))
      (push current headings))
    (nreverse headings)))

(defun ekg-agent-org--create-items-from-note-content (tags content)
  "Create org task items with TAGS from `org-mode' note CONTENT.
This is a compatibility path for models that call `create_note' with
TODO headings after the user asked for org task management."
  (let* ((tags (ekg-agent-org--normalize-tool-tags tags))
         (headings (ekg-agent-org--parse-note-headings content))
         (title (ekg-agent-org--org-note-title content))
         min-level
         needs-parent
         (created nil)
         (stack nil)
         parent-id)
    (unless headings
      (error "No org TODO headings found in create_note content"))
    (setq min-level (apply #'min (mapcar (lambda (heading)
                                           (plist-get heading :level))
                                         headings)))
    (setq needs-parent (and title (> (length headings) 1)))
    (when needs-parent
      (setq parent-id
            (ekg-agent-org--save-item
             title
             "Parent task created from org TODO note content."
             tags nil (ekg-agent--org-default-todo-keyword) nil nil))
      (push (cons parent-id title) created)
      (push (cons (1- min-level) parent-id) stack))
    (dolist (heading headings)
      (let* ((level (plist-get heading :level))
             (status (plist-get heading :status))
             (heading-title (plist-get heading :title))
             (body (plist-get heading :content))
             (parent (or (cdr (assoc (1- level) stack))
                         (and needs-parent (= level min-level) parent-id)))
             (id (ekg-agent-org--save-item heading-title body tags parent
                                           status nil nil)))
        (push (cons id heading-title) created)
        (setq stack (assq-delete-all level stack))
        (push (cons level id) stack)))
    (when (buffer-live-p ekg-agent--current-log-buffer)
      (ekg-agent--record-tool-completion
       ekg-agent--current-log-buffer "add_org_item"
       "Created org task items from create_note org headings"))
    (format
     (concat
      "Converted org TODO note content into org task items: %s. "
      "Use these IDs with set_org_item_status after completing the work.")
     (mapconcat (lambda (item)
                  (format "%s (%s)" (car item) (cdr item)))
                (nreverse created)
                ", "))))

(defun ekg-agent-org--tool-add-item (title content tags parent-id status deadline scheduled)
  "Add a new org task item to EKG.

TITLE is the task title.
CONTENT is the task content/description.
TAGS is a list of additional tags.
PARENT-ID is the parent task ID (nil for no parent)
STATUS is the task status (will be converted to uppercase).
DEADLINE is the deadline timestamp string (ignored if empty).
SCHEDULED is the scheduled timestamp string (ignored if empty)."
  (ekg-agent--with-error-as-text
    (format "Added note with ID %s"
            (ekg-agent-org--save-item title content tags parent-id status
                                      deadline scheduled))))

(defconst ekg-agent-org-tool-add-task
  (llm-make-tool
   :function #'ekg-agent-org--tool-add-item
   :name "add_org_item"
   :description "Add a new org-mode task item."
   :args
   '((:name "title" :type string :description "The title/headline of the task" :require t)
     (:name "content" :type string :description "The content/description of the task" :required t)
     (:name "tags" :type array :items (:type string)
            :description "Additional tags for the task (org tags and agent tags will be added automatically)")
     (:name "parent_id" :type integer :description "The parent task ID if exists")
     (:name "status" :type string :description "The task status (TODO, DONE, etc.), will default to TODO if not set")
     (:name "deadline" :type string :description "The deadline timestamp in ISO 8601 format")
     (:name "scheduled" :type string :description "The scheduled timestamp in ISO 8601 format"))))

(defun ekg-agent-org--tool-set-status (id status)
  "Set the status of an org task item.

ID is the note ID.
STATUS is the new status (will be converted to uppercase)."
  (ekg-agent--with-error-as-text
    (let ((note (ekg-get-note-with-id id)))
      (unless note
        (error "No note found with ID %s" id))
      (setf (ekg-note-tags note)
            (cons
             (concat ekg-org-state-tag-prefix (downcase status))
             (seq-remove
              (lambda (tag) (string-prefix-p ekg-org-state-tag-prefix tag))
              (ekg-note-tags note))))
      (ekg-save-note note)
      (format "Set status of note ID %s to %s" id status))))

(defconst ekg-agent-org-tool-set-status
  (llm-make-tool
   :function #'ekg-agent-org--tool-set-status
   :name "set_org_item_status"
   :description "Set the status keyword of an org-mode task item."
   :args
   '((:name "id" :type integer :description "The ID of the task item" :required t)
     (:name "status" :type string :description "The new status of the task (TODO, DONE, etc.)" :required t))))

(defun ekg-agent-org--tool-list-items (&optional state)
  "List all org task items.

STATE if non-nil, filter by status (e.g., \"TODO\", \"DONE\").
Returns text in Org format, as if they were in an Org file."
  (ekg-agent--with-error-as-text
    (let ((result (ekg-org-generate-org-content
                   nil (when state
                         (lambda (note)
                           (string-equal
                            (downcase state)
                            (downcase (ekg-org--state note))))))))
      (if (string-empty-p result)
          (if state
              (format "No org items found with state %s." state)
            "No org items found.")
        result))))

(defconst ekg-agent-org-tool-list-items
  (llm-make-tool
   :function #'ekg-agent-org--tool-list-items
   :name "list_org_items"
   :description "Return matching org-mode task items as an Org formatted string."
   :args
   '((:name "state" :type string
            :description "Filter tasks by state (TODO, DONE, etc.)"))))

(defun ekg-agent-org-plan-task ()
  "Plan the current task and add the plan as child tasks using the agent."
  (interactive)
  (let* ((ekg-note (ekg-current-note-or-error-expanded))
         (parent-id (ekg-note-id ekg-note))
         (parent-note (ekg-get-note-with-id parent-id))
         (question (format "Given the task '%s', create a plan to accomplish it by creating subtasks using the tool to add ekg org tasks or add information to existing ekg note tasks. The parent ekg note id is '%s'."
                           (ekg-org--note-title parent-note)
                           parent-id)))
    (ekg-agent-ask-with-note question parent-id (list ekg-agent-org-tool-add-task))))

(defun ekg-agent-org-run-task ()
  "Execute the current org task autonomously using the agent.
The agent receives the full org hierarchy as context (current task,
parent, grandparent, etc.) and instructions to complete the task.
When finished, the agent sets the task status (typically DONE),
which ends the agent session.  No human input is required."
  (interactive)
  (let* ((ekg-note (ekg-current-note-or-error-expanded))
         (task-id (ekg-note-id ekg-note))
         (note (ekg-get-note-with-id task-id))
         (hierarchy (ekg-org--get-hierarchy note))
         (hierarchy-text (ekg-org--hierarchy-to-text hierarchy))
         (title (or (ekg-org--note-title note) "(untitled)"))
         (children (ekg-org-get-child-notes-of-id task-id))
         (children-text (when children
                          (concat "\n\nChild tasks:\n"
                                  (mapconcat
                                   (lambda (child)
                                     (format "  - [%s] %s (id: %s)"
                                             (condition-case nil (ekg-org--state child) (error "UNKNOWN"))
                                             (or (ekg-org--note-title child) "(untitled)")
                                             (ekg-note-id child)))
                                   children "\n"))))
         (prompt-notes (ekg-get-notes-cotagged-with-tags
                        (ekg-note-tags note) ekg-llm-prompt-tag))
         (prompt-context (when prompt-notes
                           (concat "\n\nPrompt instructions from co-tagged notes:\n"
                                   (mapconcat (lambda (n)
                                                (string-trim
                                                 (substring-no-properties
                                                  (ekg-display-note-text n nil 'plaintext))))
                                              prompt-notes "\n"))))
         (context (concat
                   "You are executing an org task autonomously. Here is the full task hierarchy, "
                   "from the root task down to the current task you must execute:\n\n"
                   hierarchy-text
                   (or children-text "")
                   (or prompt-context "")
                   "\n\n"
                   (format "The current date and time is %s." (format-time-string "%F %R"))))
         (question (concat
                    (format "Execute the following task: '%s' (note id: %s).\n\n" title task-id)
                    "Complete this task using the tools available to you. "
                    "You should NOT ask the user for input. Work autonomously.\n\n"
                    "When you are done, you MUST call the `set_org_item_status` tool to "
                    "set this task's status (typically to DONE, or WAITING if blocked). "
                    "Calling `set_org_item_status` will end your session.\n\n"
                    "Before setting the status, use `display_result_in_popup` to tell the user what you did.")))
    (ekg-agent--iterate (llm-make-chat-prompt
                         question
                         :context (concat (ekg-agent-instructions-intro) "\n\n" context)
                         :tools (ekg-agent--tools
                                 (list ekg-agent-tool-popup-result
                                       ekg-agent-org-tool-add-task
                                       ekg-agent-org-tool-set-status
                                       ekg-agent-org-tool-list-items))
                         :tool-options (make-llm-tool-options :tool-choice 'any))
                        0
                        (ekg-agent--make-status-callback)
                        '("set_org_item_status")
                        nil)))

(provide 'ekg-agent)

;;; ekg-agent.el ends here
