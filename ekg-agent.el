;;; ekg-agent.el --- Agentic actions for ekg -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Keywords: outlines, hypermedia
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
(require 'llm-vertex)
(require 'ekg-llm)
(require 'ekg-embedding)
(require 'seq)
(require 'json)
(require 'subr-x)
(require 'async)

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

(defcustom ekg-agent-self-instruct-tag "agent/instruct"
  "The tag used to identify notes with instructions for the agent."
  :type 'string
  :group 'ekg-agent)

(add-to-list 'ekg-hidden-tags ekg-agent-self-info-tag)
(add-to-list 'ekg-hidden-tags ekg-agent-self-instruct-tag)

(defcustom ekg-agent-context-func #'ekg-agent-starting-context
  "Function that returns the starting information for an ekg agent.

This is combined with a standard prompt to form the LLM context for the
agent."
  :type 'function
  :group 'ekg-agent)

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

(defcustom ekg-agent-code-command nil
  "Command to run for the coding tool.

This should be a shell-style command string (for example, \"claude -p
--dangerously_skip_permissions\"). The prompt will be provided on stdin;
stdout will be returned."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'ekg-agent)

(defvar-local ekg-agent--prompt nil
  "The prompt for the agent section in the current buffer.")

(defvar-local ekg-agent--end-tools nil
  "The end tools for the current agent run in this buffer.")

(defvar-local ekg-agent--running-p nil
  "Non-nil when the agent is actively running in this buffer.")

(defvar-local ekg-agent--cancelled-p nil
  "Non-nil when the user has requested cancellation of the agent.")

(defvar ekg-agent--daily-timer nil
  "Timer object for the daily agent evaluation.")

(defconst ekg-agent-tool-all-tags
  (make-llm-tool :function (lambda (tags num)
                             (let ((ekg-llm-note-numwords 500)
                                   (notes (ekg-agent--get-notes :tags (append tags nil) :num num)))
                               (mapconcat #'ekg-llm-note-to-text notes "\n\n")))
                 :name "get_notes_with_all_tags"
                 :description "Retrieve notes that have all the specified tags."
                 :args '((:name "tags" :type array :items (:type string) :description "List of tags to filter notes by.  Each tag may have spaces or non-alphanumeric characters.")
                         (:name "num" :type integer :description "Maximum number of notes to retrieve."))))

(defconst ekg-agent-tool-any-tags
  (make-llm-tool :function (lambda (tags num)
                             (let ((ekg-llm-note-numwords 500)
                                   (notes (ekg-agent--get-notes :any-tags (append tags nil) :num num)))
                               (mapconcat #'ekg-llm-note-to-text notes "\n\n")))
                 :name "get_notes_with_any_tags"
                 :description "Retrieve notes that have any of the specified tags."
                 :args '((:name "tags" :type array :items (:type string) :description "List of tags to filter notes by.  Each tag may have spaces or non-alphanumeric characters.")
                         (:name "num" :type integer :description "Maximum number of notes to retrieve."))))

(defconst ekg-agent-tool-get-note-by-id
  (make-llm-tool :function (lambda (id)
                             (let ((notes (ekg-agent--get-notes :note-id id)))
                               (if notes
                                   (ekg-llm-note-to-text (car notes))
                                 (error "Note with ID %s not found" id))))
                 :name "get_note_by_id"
                 :description "Retrieve a note by its unique identifier."
                 :args '((:name "id" :type string :description "The unique identifier of the note."))))

(defconst ekg-agent-tool-search-notes
  (make-llm-tool :function (lambda (query num)
                             (let ((ekg-llm-note-numwords 500)
                                   (notes (ekg-agent--get-notes :semantic-search query :num num)))
                               (mapconcat #'ekg-llm-note-to-text notes "\n\n")))
                 :name "search_notes"
                 :description "Search notes by a query string, retrieving by semantic similarity."
                 :args '((:name "query" :type string :description "The search query string.")
                         (:name "num" :type integer :description "Maximum number of notes to retrieve."))))

(defconst ekg-agent-tool-ask-user
  (make-llm-tool :function (lambda (question)
                             (read-string (format "Question from ekg-agent: %s\nResponse: " question)))
                 :name "ask_user"
                 :description "Ask the user a question and get their response."
                 :args '((:name "question" :type string :description "The question to ask the user."))))

(defconst ekg-agent-tool-list-tags
  (make-llm-tool :function (lambda () (apply #'vector (ekg-tags)))
                 :name "list_all_tags"
                 :description "List all existing tags in the ekg database."
                 :args '()))

(defun ekg-agent--get-note-with-id (id)
  "Retrieve the note with string ID, handling different ID types.

This tries a few different things, since ekg ids can of various types,
but we'll only get strings from the LLM."
  (or (ekg-get-note-with-id id)
      (let ((int-id (string-to-number id)))
        (when (> int-id 0)
          (ekg-get-note-with-id int-id)))
      (ekg-get-note-with-id (intern id))))

(defconst ekg-agent-tool-append-to-note
  (make-llm-tool :function (lambda (id content)
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
                                 (format "Appended content to note ID %s" id))))
                 :name "append_to_note"
                 :description "Append content to an existing note by its ID."
                 :args '((:name "id" :type string :description "The unique identifier of the note.")
                         (:name "content" :type string :description "The content to append to the note."))))

(defconst ekg-agent-tool-replace-note
  (make-llm-tool :function (lambda (id content)
                             (let ((note (ekg-agent--get-note-with-id id)))
                               (unless note
                                 (error "Note with ID %s not found" id))
                               (let* ((enclosure (assoc-default (ekg-note-mode note) ekg-llm-format-output nil '("_BEGIN_" . "_END_")))
                                      (new-text (concat (car enclosure) "\n"
                                                        content "\n"
                                                        (cdr enclosure))))
                                 (setf (ekg-note-text note) new-text)
                                 (ekg-save-note note)
                                 (format "Replaced content of note ID %s" id))))
                 :name "replace_note"
                 :description "Replace the content of an existing note by its ID."
                 :args '((:name "id" :type string :description "The unique identifier of the note.")
                         (:name "content" :type string :description "The new content for the note."))))

(defconst ekg-agent-tool-create-note
  (make-llm-tool :function (lambda (tags content mode)
                             (unless (member mode '("org-mode" "markdown-mode" "text-mode"))
                               (error "Unsupported mode: %s" mode))
                             ;; Use ekg-agent-add-note for consistency (includes auto-tags)
                             (ekg-agent-add-note content (append tags nil) mode))
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
                 :description "Indicate that no further actions need to be taken."
                 :args '()))

(defconst ekg-agent-tool-subagent-end
  (make-llm-tool :function #'identity
                 :name "subagent_end"
                 :description "Indicate that a sub-agent has completed its task and is returning control to the parent agent.  The return value will be passed back to the parent agent as the result of the sub-agent's `run_subagent` call."
                 :args '((:name "result" :type string :description "The result to return to the parent agent."))))

(defun ekg-agent--popup-result-in-buffer (result)
  "Display RESULT in a new buffer and pop up to it."
  (let ((buf (get-buffer-create "*ekg agent result*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert result)
      (when (featurep 'markdown-mode)
        (markdown-mode))
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (format "Popup displayed in buffer %s" (buffer-name))))

(defconst ekg-agent-tool-popup-result
  (make-llm-tool :function #'ekg-agent--popup-result-in-buffer
                 :name "display_result_in_popup"
                 :description "Display the result of a query in a popup buffer.  Use markdown mode for formatting."
                 :args '((:name "result" :type string :description "The result to display."))))

(defun ekg-agent--run-code (prompt)
  "Run the configured `ekg-agent-code-command` with PROMPT on stdin."
  (unless (and (stringp ekg-agent-code-command)
               (string-match-p "\\S-" ekg-agent-code-command))
    (error "Ekg-agent-code-command is not configured"))
  (let* ((args (split-string-and-unquote ekg-agent-code-command))
         (program (car args))
         (program-args (cdr args)))
    (with-temp-buffer
      (insert prompt)
      (let ((exit-code (apply #'call-process-region
                              (point-min) (point-max)
                              program
                              t
                              (list t t)
                              nil
                              program-args)))
        (if (and (integerp exit-code) (zerop exit-code))
            (string-trim-right (buffer-string))
          (error "Command failed (%s): %s"
                 exit-code
                 (string-trim-right (buffer-string))))))))

(defconst ekg-agent-tool-run-elisp
  (make-llm-tool :function (lambda (callback elisp return)
                             (async-start (lambda ()
                                            (let* (e
                                                   (result
                                                    (condition-case err
                                                        (eval (read elisp))
                                                      (error (setq e (format "%S" e))))))
                                              (or e
                                                  (if (equal return "result")
                                                      result
                                                    (buffer-substring-no-properties (point-min) (point-max))))))
                                          callback))
                 :name "run_elisp"
                 :description "Evaluate arbitrary Emacs Lisp and return the printed result of the final form."
                 :args '((:name "elisp" :type string :description "The Emacs Lisp code to evaluate." :required t)
                         (:name "return" :type string :enum ["result" "buffer"]
                                :description "Whether to return the result of the evaluated elisp, or the buffer after the elisp has been evaluated. If there is an error, it will be returned regardless of this value."
                                :required t))
                 :async t))

(defconst ekg-agent-tool-code
  (make-llm-tool :function #'ekg-agent--run-code
                 :name "run_code_tool"
                 :description "Run the configured coding tool (such as 'claude code') command with the prompt and return the result.  If you want to make changes to code, use this tool and ask it to make the changes.  Assume that it can do almost anything, including reading files and webpages."
                 :args '((:name "prompt" :type string :description "The prompt to pass to the tool."))))

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
                              :tools (append
                                      ekg-agent-base-tools
                                      (list ekg-agent-tool-subagent-end)
                                      ekg-agent-extra-tools
                                      (when (llm-google-p (ekg-llm--provider))
                                        (list (make-llm-tool :function #'ignore
                                                             :name "google_search"
                                                             :description "Google Search built-in tool"
                                                             :args nil))))
                              :tool-options (make-llm-tool-options :tool-choice 'any))))
                 (ekg-agent--iterate prompt
                                     1
                                     (lambda (status)
                                       (funcall callback
                                                (format "Subagent result: %s" status)))
                                     '("subagent_end")
                                     (ekg-agent--timeout-deadline))))
   :name "run_subagent"
   :description "Run a sub-agent with the given instructions. The sub-agent has its own conversation with access to all the standard ekg tools, and runs until it calls end. Use this for tasks that require multiple steps or complex tool use."
   :args '((:name "instructions" :type string :description "Detailed instructions for the sub-agent to follow."))
   :async t))

(defun ekg-agent--ensure-log-window ()
  "Ensure the agent log buffer is displayed in a side window."
  (let* ((buf (current-buffer))
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
  "Append LINE to the agent log buffer without a timestamp."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert line "\n"))
  (let ((win (get-buffer-window nil t)))
    (when win
      (set-window-point win (point-max)))))

(defun ekg-agent--log (format-string &rest args)
  "Append a log line built from FORMAT-STRING and ARGS to the agent log buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (format-time-string "%F %T "))
    (insert (apply #'format format-string args))
    (insert "\n"))
  (let ((win (get-buffer-window nil t)))
    (when win
      (set-window-point win (point-max)))))

(defun ekg-agent--log-session-start (label &optional detail)
  "Start a new session in the agent log with LABEL and optional DETAIL."
  (ekg-agent--ensure-log-window)
  (ekg-agent--log-raw (make-string 72 ?-))
  (ekg-agent--log "%s%s" label (if detail (format ": %s" detail) "")))

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
  (format (concat
           "Time limit reached. Before this session ends, immediately create a note with your current state "
           "using `create_note` (tag it with `%s` or `%s`). Also call `summarize_state` with a brief update. "
           "Then finish by calling `end` or the appropriate completion tool.")
          ekg-agent-self-info-tag
          ekg-agent-self-instruct-tag))

(defun ekg-agent--summarize-state (state)
  "Write STATE to the agent log window."
  (ekg-agent--ensure-log-window)
  (ekg-agent--log "State: %s" state)
  "ok")

(defconst ekg-agent-tool-summarize-state
  (make-llm-tool :function #'ekg-agent--summarize-state
                 :name "summarize_state"
                 :description "Summarize the current state in the agent log window.  This should be called often to keep the user up to date on what is happening."
                 :args '((:name "state" :type string :description "Short summary of current progress, plan, or blockers."))))

(defconst ekg-agent-tool-read-agents-md
  (make-llm-tool :function (lambda (dir)
                             (or (ekg-agent--read-agents-md dir)
                                 (format "No AGENTS.md found in %s" dir)))
                 :name "read_agents_md"
                 :description "Read the AGENTS.md file from a specified directory.  AGENTS.md files contain user instructions and preferences for agents."
                 :args '((:name "dir" :type string :description "The directory path to read AGENTS.md from."))))

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
   ekg-agent-tool-read-agents-md)
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

(defun ekg-agent--ask (question context)
  "Ask the ekg agent a QUESTION and display the result.

CONTEXT is what we'll display to the agent as context.  If nil no
additional context is added.

The agent has access to all the defined tools, and can create notes or
display results in a popup buffer, or ask the user a question.  The
agent will decide which is best."
  (let* ((prompt (concat (ekg-agent-instructions-intro)
                         "\n\nYour instructions are to answer the user's question, given below. "
                         "You have access to tools to help you. "
                         "After each tool call you will be given a chance to make more tool calls. "
                         "When you have the answer, you MUST present it to the user by calling either "
                         "`display_result_in_popup` or `create_note`.  Calling one of these tools "
                         "will complete your task.  Do not call any other tools after you have "
                         "presented the answer."
                         (when context
                           "\n\nHere is some additional context to help you:\n")
                         context
                         "\n\n"
                         (format "The current date and time is %s."
                                 (format-time-string "%F %R")))))
    (ekg-agent--iterate (llm-make-chat-prompt
                         question
                         :context prompt
                         :tools (append ekg-agent-base-tools
                                        ekg-agent-extra-tools
                                        (list ekg-agent-tool-popup-result)
                                        (when (llm-google-p (ekg-llm--provider))
                                          (list (make-llm-tool :function #'ignore
                                                               :name "google_search"
                                                               :description "Google Search built-in tool"
                                                               :args nil))))
                         :tool-options (make-llm-tool-options :tool-choice 'any))
                        0
                        (ekg-agent--make-status-callback)
                        '("display_result_in_popup" "create_note")
                        (ekg-agent--timeout-deadline))))

(defun ekg-agent-ask (question)
  "Ask the ekg agent a QUESTION and display the result.
The agent has access to all the ekg tools, and can create notes
or display results in a popup buffer.  The agent will decide
which is best."
  (interactive "sQuestion: ")
  (ekg-agent--ask question
                  (concat
                   "The last 10 notes:\n\n"
                   (mapconcat #'ekg-llm-note-to-text
                              (ekg-get-latest-modified 10) "\n\n"))))

(defun ekg-agent-ask-with-note (question &optional id)
  "Ask the agent QUESTION with the note with ID as context.

If ID is nil, we will use the current buffer's associated note, or the
note at point if in a `ekg-notes-mode` buffer.'"
  (interactive "sQuestion: \n")
  (let* ((note (or (and id (ekg-agent--get-note-with-id id))
                   (and (derived-mode-p 'ekg-notes-mode)
                        (ekg-current-note-or-error))
                   (and (derived-mode-p 'ekg-note-mode)
                        ekg-note)))
         (_ (unless note (error "No current note found")))
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
                     prompt-context))))

(defun ekg-agent-ask-with-buffer (instructions)
  "Issue INSTRUCTIONS to the agent, with the current buffer as context."
  (interactive "sInstructions: ")
  (ekg-agent--ask instructions
                  (concat
                   (format "The current buffer is named %s%s, the major mode is %s.  The content is:\n"
                           (buffer-name)
                           (if buffer-file-name
                               (format " (file: %s)" buffer-file-name)
                             "")
                           major-mode)
                   (buffer-substring-no-properties (point-min) (point-max)))))

(defun ekg-agent-ask-with-region (instructions start end)
  "Issue INSTRUCTIONS to the agent, with the region from START to END as context."
  (interactive "sInstructions: \nr")
  (ekg-agent--ask instructions
                  (concat
                   (format "The current buffer is named %s%s, the major mode is %s.  The content is the selected region:\n"
                           (buffer-name)
                           (if buffer-file-name
                               (format " (file: %s)" buffer-file-name)
                             "")
                           major-mode)
                   (buffer-substring-no-properties start end))))

(defun ekg-agent-starting-context ()
  "Return the context for an agent for new sessions.

This includes the latest 10 notes and the org agenda, and the last 10
self info and self-instruct notes."
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
   "\n\nThe last 10 self-info and self-instruct notes:\n"
   (mapconcat #'ekg-llm-note-to-text
              (seq-take (ekg-get-notes-with-any-tags (list ekg-agent-self-info-tag ekg-agent-self-instruct-tag))
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

(defun ekg-agent-instructions-intro ()
  "Introductory instructions for the ekg agent."
  (let ((timeout-desc (if (and (numberp ekg-agent-timeout-seconds)
                               (> ekg-agent-timeout-seconds 0))
                          (format "%s seconds" ekg-agent-timeout-seconds)
                        "no timeout configured")))
    (concat
     (format
      "You are an agent that works with a user through a note taking system in
Emacs, called ekg.  In ekg, notes have tags and content, and can be
written in markdown, org-mode or plain text.  ekg is backed by a
database, and the way to interact with it is with the tools provided.

In org-mode, you can link to a note with `[[ekg-note:<id>][<link display
text>]]`.  Tags can be linked with `[[ekg-tag:<tag>][<link display
text>]]`.

In markdown mode, there's no way to link directly to notes, but you can
use [[tag]] to link to a tag.

These notes should act as memory for subjects and tasks that both users
and agents can read and write.

To write a note to your future self so that you remember important
information the next time you are run, use the `%s` tag for information
for your future self, so that you can behave more usefully in the future.
Write notes with these tags if you feel you have discovered something
that will make future runs better, and want to record this.

Use the `summarize_state` tool regularly to write brief status updates
to the user-visible agent log window (at least every couple of tool
calls or whenever your plan changes).

The session may end if a total task timeout has been configured. Or,
some error may interrupt the processing. Because your processing can end
unexpected, you MUST occasionally write out your current state to an ekg
note (tag with `%s` or `%s`, and a tag you choose to represent the
task). If you receive a timeout warning, do this immediately and then
finish. When executing a long-running task (more than a couple of tool
calls), start saving your state every few tool calls to a note.

When creating a note, text that you add will automatically have the tags
surrounding it to indicate that it was written by an LLM.  Do not add
these tags manually."
      ekg-agent-self-instruct-tag
      timeout-desc
      ekg-agent-self-info-tag
      ekg-agent-self-instruct-tag)
     (or (ekg-agent--agents-md-context) ""))))

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
                       (ekg-agent-starting-context)
                       :context
                       (concat (ekg-agent-instructions-intro) "\n"
                               (ekg-agent-instructions-evaluate-status))
                       :tools (append
                               ekg-agent-base-tools
                               (list (ekg-agent-tool-end))
                               ekg-agent-extra-tools
                               ;; Use Google Search as well, if possible.
                               (when (llm-google-p (ekg-llm--provider))
                                 (list (make-llm-tool :function #'ignore
                                                      :name "google_search"
                                                      :description "Google Search built-in tool"
                                                      :args nil)))))
                      0
                      (ekg-agent--make-status-callback)
                      '("end")
                      (ekg-agent--timeout-deadline)))

(defun ekg-agent--prompt-id (prompt)
  "From llm PROMPT, call an LLM to get a short identifier."
  (let (id)
    (llm-chat (ekg-llm--provider)
              (llm-make-chat-prompt
               (llm-chat-prompt-interaction-content
                (car (seq-filter (lambda (interaction)
                                   (eq (llm-chat-prompt-interaction-role interaction) 'user))
                                 (llm-chat-prompt-interactions prompt))))
               :context "From user input of the what they are instruction an agent to do, call the tool to report a short name.  "
               :tools (list
                       (make-llm-tool :function (lambda (result) (setq id result))
                                      :name "report_id"
                                      :description "Report on the decided id so that the agent can use this id to name an emacs buffer that will hold the status of the agent.  "
                                      :args '((:name "id" :type string
                                                     :description "This id will be for naming an emacs buffer, so use lowercase and dashes.  This should be about 3-5 words.  Example name: 'check-gnus-email-and-respond'.  "))))
               :tool-options (make-llm-tool-options :tool-choice 'any)))
    id))

(defvar ekg-agent-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ekg-agent-continue)
    (define-key map (kbd "C-c C-k") #'ekg-agent-cancel)
    map)
  "Keymap for `ekg-agent-log-mode'.")

(define-minor-mode ekg-agent-log-mode
  "Minor mode for ekg agent log buffers.
Provides commands to continue or cancel agent work.

\\{ekg-agent-log-mode-map}"
  :lighter (:eval (if ekg-agent--running-p " Agent:run" " Agent:idle"))
  :keymap ekg-agent-log-mode-map)

(defun ekg-agent--set-stopped (log-buf)
  "Mark the agent as no longer running in LOG-BUF."
  (when (and log-buf (buffer-live-p log-buf))
    (with-current-buffer log-buf
      (setq ekg-agent--running-p nil))))

(defun ekg-agent--iterate (prompt iteration-num &optional status-callback end-tools deadline timeout-final)
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

This is to start, and after every tool call to continue the agent
session.  At iteration 0 the log buffer is created and
`ekg-agent-log-mode' is enabled."
  (if (= iteration-num 0)
      ;; Set up everything
      (let* ((id (ekg-agent--prompt-id prompt))
             (buf (get-buffer-create (format ekg-agent-log-buffer-name-format id))))
        (with-current-buffer buf
          (erase-buffer)
          (ekg-agent--log-session-start (buffer-name))

          (insert (format "Agent session for: %s\n\n" id))
          (setq ekg-agent--prompt prompt)
          (setq ekg-agent--end-tools end-tools)
          (setq ekg-agent--running-p t)
          (setq ekg-agent--cancelled-p nil)
          (when (featurep 'markdown-mode)
            (markdown-mode)
            ;; No need for flycheck or flymake to be running on this buffer.
            (when (featurep 'flycheck)
              (flycheck-mode 0))
            (when (featurep 'flymake)
              (flymake-mode 0)))
          (ekg-agent-log-mode 1)
          (goto-char (point-min))
          (ekg-agent--iterate prompt 1
                              status-callback
                              end-tools
                              deadline
                              timeout-final)))
    ;; iteration > 0: run the agent loop
    (let ((log-buf (current-buffer))
          (expired (and deadline (> (float-time) deadline))))
      (when (and expired (not timeout-final))
        (ekg-agent--log "Timeout reached; requesting final state note.")
        (ekg-agent--prompt-append-user-message prompt (ekg-agent--timeout-warning-message))
        (setq timeout-final t))
      (llm-chat-async
       (ekg-llm--provider)
       prompt
       (lambda (result)
         (if (and (buffer-live-p log-buf)
                  (buffer-local-value 'ekg-agent--cancelled-p log-buf))
             (progn
               (ekg-agent--set-stopped log-buf)
               (with-current-buffer log-buf
                 (ekg-agent--log "Agent stopped (cancelled by user)"))
               (when status-callback (funcall status-callback "stopped by user")))
           (let ((result-alist (plist-get result :tool-results))
                 (end-tools (or end-tools '("end"))))
             (let ((tools-ran (mapconcat (lambda (result)
                                           (format "Tool: %s Result: %s"
                                                   (car result) (cdr result)))
                                         result-alist ", ")))
               (if (and log-buf (buffer-live-p log-buf))
                   (with-current-buffer log-buf
                     (ekg-agent--log "Tools: %s" tools-ran))
                 (message "Ran tools: %s" tools-ran)))
             (cond
              (timeout-final
               (ekg-agent--set-stopped log-buf)
               (when status-callback (funcall status-callback "stopped by timeout")))
              ((seq-find (lambda (end-tool) (assoc-default end-tool result-alist)) end-tools)
               (ekg-agent--set-stopped log-buf)
               (when status-callback
                 (funcall
                  status-callback
                  (mapconcat (lambda (end-tool)
                               (assoc-default end-tool result-alist))
                             (seq-filter (lambda (end-tool) (assoc-default end-tool result-alist))
                                         end-tools)
                             ", "))))
              (t
               (ekg-agent--iterate prompt
                                   (+ 1 iteration-num)
                                   status-callback
                                   end-tools
                                   deadline))))))
       (lambda (_ err)
         (ekg-agent--set-stopped log-buf)
         (when status-callback (funcall status-callback 'error))
         (error "%s" err))
       t))))

(defun ekg-agent-continue (message)
  "Continue the agent from where it left off.
With a prefix argument, prompt for a MESSAGE to send to the agent."
  (interactive (list (when current-prefix-arg
                       (read-string "Message to agent: "))))
  (unless ekg-agent--prompt
    (user-error "No agent prompt available to continue"))
  (when ekg-agent--running-p
    (user-error "Agent is already running"))
  (setq ekg-agent--cancelled-p nil)
  (setq ekg-agent--running-p t)
  (ekg-agent--log-session-start "Continuing agent")
  (when (and message (not (string-empty-p message)))
    (ekg-agent--prompt-append-user-message ekg-agent--prompt message)
    (ekg-agent--log "User message: %s" message))
  (ekg-agent--iterate ekg-agent--prompt
                      1
                      (ekg-agent--make-status-callback)
                      ekg-agent--end-tools
                      (ekg-agent--timeout-deadline)))

(defun ekg-agent-cancel ()
  "Cancel the current agent run, preserving the prompt for continuation.
The agent will stop after the current in-flight LLM call completes.
Use \\[ekg-agent-continue] to resume."
  (interactive)
  (unless ekg-agent--running-p
    (user-error "No agent is currently running"))
  (setq ekg-agent--cancelled-p t)
  (ekg-agent--log "Cancellation requested; will stop after current tool call"))

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
                                                  (list ekg-agent-self-info-tag
                                                        ekg-agent-self-instruct-tag))))
                                    10))
           (context-notes-json (let ((ekg-llm-note-numwords 100))
                                 (mapconcat #'ekg-llm-note-to-text context-notes "\n\n")))
           (current-note-json (let ((ekg-llm-note-numwords 10000))
                                (ekg-llm-note-to-text ekg-note)))
           (prompt (concat (ekg-agent-instructions-intro)
                           "\n\nYour instructions:\n"
                           instructions-for-use
                           "\n\nYou have access to tools to help you.
After each tool call you will be given a chance to make more tool calls.
Your work will end after you create a note or rewrite the
note.  Prefer to append to the note by default, unless the user is asking
for a rewritten or new note.\nThe user input will be the note they are
currently editing.\n\n"
                           (format "The current date and time is %s.\n"
                                   (format-time-string "%F %R"))
                           (format "Some notes matching the tags or context: %s\n"
                                   context-notes-json))))
      (let ((overlay (make-overlay (point-max) (point-max) nil t t)))
        (overlay-put overlay 'after-string (propertize " [LLM response computing]" 'face 'shadow))
        (ekg-agent--iterate (llm-make-chat-prompt
                             current-note-json
                             :context prompt
                             :tools (append
                                     ekg-agent-base-tools
                                     ekg-agent-extra-tools
                                     (list
                                      ekg-agent-tool-append-response
                                      ekg-agent-tool-replace-response)
                                     (when (llm-google-p (ekg-llm--provider))
                                       (list (make-llm-tool :function #'ignore
                                                            :name "google_search"
                                                            :description "Google Search built-in tool"
                                                            :args nil))))
                             :tool-options (make-llm-tool-options :tool-choice 'any))
                            0
                            (ekg-agent--make-status-callback
                             (lambda (_status)
                               (delete-overlay overlay)))
                            '("append_to_current_note" "replace_current_note")
                            (ekg-agent--timeout-deadline))))))

;; Redefine the keys, take the binding over from ekg-llm.
(define-key ekg-capture-mode-map (kbd "C-c .") #'ekg-agent-note-response)
(define-key ekg-edit-mode-map (kbd "C-c .") #'ekg-agent-note-response)

(defun ekg-agent-show-internal-notes ()
  "Show notes with agent internal tags."
  (interactive)
  (ekg-show-notes-with-any-tags (list ekg-agent-self-info-tag ekg-agent-self-instruct-tag)))

;;;###autoload
(defun ekg-agent-add-note (text tags mode)
  "Add a note from an agent with TEXT, TAGS, and MODE.

TAGS should be a list of tag strings.  MODE should be a symbol like
'org-mode, 'markdown-mode, or 'text-mode.  Returns the note ID on
success, signals an error on failure.

This function automatically:
- Adds the `ekg-agent-author-tag' to the tags
- Applies all functions from `ekg-capture-auto-tag-funcs' (e.g., date tags)
- Wraps the text in the appropriate LLM output format based on MODE

This is intended to be used from the command-line so agents can easily
add properly formatted notes to ekg."
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
      (modified_time . ,(ekg-note-modified-time note)))))

(defun ekg-agent--notes-to-json (notes &optional max-words)
  "Convert list of NOTES to a JSON array string.
If MAX-WORDS is specified, truncate each note's text to that many words."
  (json-encode (mapcar (lambda (note)
                         (ekg-agent--note-to-alist note max-words))
                       notes)))

(cl-defun ekg-agent--get-notes (&key tags any-tags note-id semantic-search text-search (num 10))
  "Get notes from ekg based on search criteria.

This is a helper function that returns a list of note objects.
Use `ekg-agent-read-notes' for CLI access with JSON output.

This function supports multiple modes of operation:

1. By tags (AND): Provide TAGS as a list of tag strings.
2. By tags (OR): Provide ANY-TAGS as a list of tag strings.
3. By note ID: Provide NOTE-ID as a number or string.
4. By semantic search: Provide SEMANTIC-SEARCH as a query string.
5. By text search: Provide TEXT-SEARCH as a query string.

NUM is the maximum number of notes to return (default 10).

Returns a list of note objects."
  (cond
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
      (mapcar #'ekg-get-note-with-id
              (ekg-embedding-n-most-similar-notes embedding num))))

   ;; Text search (full-text search)
   (text-search
    (seq-take
     (seq-filter #'ekg-note-active-p
                 (mapcar #'ekg-get-note-with-id
                         (triples-fts-query-subject ekg-db text-search ekg-query-pred-abbrevs)))
     num))

   ;; Tag-based search with OR logic
   (any-tags
    (seq-take (ekg-get-notes-with-any-tags any-tags) num))

   ;; Tag-based search (AND logic)
   (tags
    (seq-take (ekg-get-notes-with-tags tags) num))

   ;; No search criteria
   (t
    (error "Must provide tags, any-tags, note-id, semantic-search, or text-search"))))

;;;###autoload
(cl-defun ekg-agent-read-notes (&key tags note-id semantic-search text-search (num 10) (max-words 100))
  "Read notes from ekg and return as JSON.

This function supports multiple modes of operation:

1. By tags (AND): Provide TAGS as a list of tag strings.
2. By note ID: Provide NOTE-ID as a number or string.
3. By semantic search: Provide SEMANTIC-SEARCH as a query string.
4. By text search: Provide TEXT-SEARCH as a query string.

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
                                     :num num)))
    (ekg-agent--notes-to-json notes max-words)))

(provide 'ekg-agent)

;;; ekg-agent.el ends here
