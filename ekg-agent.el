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

(require 'llm)
(require 'llm-vertex)
(require 'ekg-llm)
(require 'ekg-embedding)
(require 'seq)

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

(defcustom ekg-agent-self-instruct-tag "agent/status-instruct"
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

(defvar ekg-agent--daily-timer nil
  "Timer object for the daily agent evaluation.")

(defconst ekg-agent-tool-all-tags
  (make-llm-tool :function (lambda (tags num)
                             (let ((ekg-llm-note-numwords 500))
                               (mapconcat #'ekg-llm-note-to-text
                                          (seq-take (ekg-get-notes-with-tags (append tags nil)) num)
                                          "\n\n")))
                 :name "get_notes_with_all_tags"
                 :description "Retrieve notes that have all the specified tags."
                 :args '((:name "tags" :type array :items (:type string) :description "List of tags to filter notes by.  Each tag may have spaces or non-alphanumeric characters.")
                         (:name "num" :type integer :description "Maximum number of notes to retrieve."))))

(defconst ekg-agent-tool-any-tags
  (make-llm-tool :function (lambda (tags num)
                             (let ((ekg-llm-note-numwords 500))
                               (mapconcat #'ekg-llm-note-to-text
                                          (seq-take (ekg-get-notes-with-any-tags
                                                     (append tags nil)) num)
                                          "\n\n")))
                 :name "get_notes_with_any_tags"
                 :description "Retrieve notes that have any of the specified tags."
                 :args '((:name "tags" :type array :items (:type string) :description "List of tags to filter notes by.  Each tag may have spaces or non-alphanumeric characters.")
                         (:name "num" :type integer :description "Maximum number of notes to retrieve."))))

(defconst ekg-agent-tool-get-note-by-id
  (make-llm-tool :function (lambda (id)
                             (ekg-llm-note-to-text (ekg-get-note-with-id id)))
                 :name "get_note_by_id"
                 :description "Retrieve a note by its unique identifier."
                 :args '((:name "id" :type string :description "The unique identifier of the note."))))

(defconst ekg-agent-tool-search-notes
  (make-llm-tool :function (lambda (query num)
                             (let ((ekg-llm-note-numwords 500))
                               (mapconcat #'ekg-llm-note-to-text
                                          (mapcar #'ekg-get-note-with-id
                                                  (ekg-embedding-n-most-similar-notes
                                                   (llm-embedding ekg-embedding-provider query)
                                                   num))
                                          "\n\n")))
                 :name "search_notes"
                 :description "Search notes by a query string, retrieving by semantic similarity."
                 :args '((:name "query" :type string :description "The search query string.")
                         (:name "num" :type integer :description "Maximum number of notes to retrieve."))))

(defconst ekg-agent-tool-list-tags
  (make-llm-tool :function (lambda () (apply #'vector (ekg-tags)))
                 :name "list_all_tags"
                 :description "List all existing tags in the ekg database."
                 :args '()))

(defconst ekg-agent-tool-create-note
  (make-llm-tool :function (lambda (tags content mode)
                             (unless (member mode '("org-mode" "markdown-mode" "text-mode"))
                               (error "Unsupported mode: %s" mode))
                             (let* ((enclosure (assoc-default (intern mode) ekg-llm-format-output nil '("_BEGIN_" . "_END_")))
                                    (note (ekg-note-create :tags (seq-union (append tags nil)
                                                                            (list ekg-agent-author-tag))
                                                           :text (concat
                                                                  (car enclosure) "\n"
                                                                  content "\n"
                                                                  (cdr enclosure))
                                                           :mode (intern mode))))
                               (ekg-save-note note)))
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

(defun ekg-agent--popup-result-in-buffer (result)
  "Display RESULT in a new buffer and pop up to it."
  (let ((buf (get-buffer-create "*ekg agent result*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert result)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defconst ekg-agent-tool-popup-result
  (make-llm-tool :function #'ekg-agent--popup-result-in-buffer
                 :name "display_result_in_popup"
                 :description "Display the result of a query in a popup buffer."
                 :args '((:name "result" :type string :description "The result to display."))))

(defconst ekg-agent-base-tools
  (list
   ekg-agent-tool-all-tags
   ekg-agent-tool-any-tags
   ekg-agent-tool-get-note-by-id
   ekg-agent-tool-search-notes
   ekg-agent-tool-list-tags
   ekg-agent-tool-create-note
   ekg-agent-tool-end)
  "List of base tools available to the agent.
These tools are necessary for basic agent functionality.")

(defcustom ekg-agent-extra-tools nil
  "List of additional tools available to the agent.
These tools are used in addition to `ekg-agent-base-tools` in
`ekg-agent-evaluate-status` and `ekg-agent-note-response`."
  :type '(repeat (sexp :tag "Tool"))
  :group 'ekg-agent)

(defun ekg-agent-ask (question)
  "Ask the ekg agent a QUESTION and display the result.
The agent has access to all the ekg tools, and can create notes
or display results in a popup buffer.  The agent will decide
which is best."
  (interactive "sQuestion: ")
  (let* ((prompt (concat ekg-agent-instructions-intro
                         "\n\nYour instructions are to answer the user's question, given below. "
                         "You have access to tools to help you. "
                         "After each tool call you will be given a chance to make more tool calls. "
                         "When you have the answer, you MUST present it to the user by calling either "
                         "`display_result_in_popup` or `create_note`.  Calling one of these tools "
                         "will complete your task.  Do not call any other tools after you have "
                         "presented the answer."
                         "\n\n"
                         (format "The current date and time is %s."
                                 (format-time-string "%F %R"))))
         (context-notes (mapconcat #'ekg-llm-note-to-text
                                   (ekg-get-latest-modified 10) "\n\n"))
         (question-with-context (concat question "\n\n"
                                        "Here are the last 10 notes for context:\n"
                                        context-notes)))
    (ekg-agent--iterate (llm-make-chat-prompt
                         question-with-context
                         :context prompt
                         :tools (append (seq-remove (lambda (tool) (equal (llm-tool-name tool) "end"))
                                                    ekg-agent-base-tools)
                                        ekg-agent-extra-tools
                                        (list ekg-agent-tool-popup-result)
                                        (when (llm-google-p (ekg-llm--provider))
                                          (list (make-llm-tool :function #'ignore
                                                               :name "google_search"
                                                               :description "Google Search built-in tool"
                                                               :args nil))))
                         :tool-options (make-llm-tool-options :tool-choice 'any))
                        0
                        nil
                        '("display_result_in_popup" "create_note"))))

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

(defconst ekg-agent-instructions-intro
  "You are an agent that works with a user through a note taking system in
Emacs, called ekg.  In ekg, notes have tags and content, and can be
written in markdown, org-mode or plain text.  ekg is backed by a
database, and the way to interact with it is with the tools provided.

In org-mode, you can link to a note with `[[ekg-note:<id>][<link display
text>]]`.  Tags can be linked with `[[ekg-tag:<tag>][<link display
text>]]`.

In markdown mode, there's no way to link directly to notes, but you can
use [[tag]] to link to a tag.")

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

To write a note to your future self so that you remember important
information the next time you are run, use the `%s` tag for information
for your future self.  The `%s` is for instructions for your future
self (which you may want to use when you see how the user is interacting
with your text), so that you can behave more usefully in the future.
Write notes with these tags if you feel you have discovered something
that will make future runs better, and want to record this.

When creating a note, text that you add will automatically have the tags
surrounding it to indicate that it was written by an LLM.  Do not add
these tags manually.

The date and time is %s.
"
   ekg-agent-self-info-tag ekg-agent-self-instruct-tag
   (format-time-string "%F %R")))

(defun ekg-agent-evaluate-status ()
  "Run the ekg agent to evaluate status and help the user.
The agent will review recent notes and TODO items, then decide whether
to create new notes or perform other actions to help the user."
  (interactive)
  (ekg-agent--iterate (llm-make-chat-prompt
                       (ekg-agent-starting-context)
                       :context
                       (concat ekg-agent-instructions-intro "\n"
                               (ekg-agent-instructions-evaluate-status))
                       :tools (append
                               ekg-agent-base-tools
                               ekg-agent-extra-tools
                               ;; Use Google Search as well, if possible.
                               (when (llm-google-p (ekg-llm--provider))
                                 (list (make-llm-tool :function #'ignore
                                                      :name "google_search"
                                                      :description "Google Search built-in tool"
                                                      :args nil)))))
                      0))

(defun ekg-agent--iterate (prompt iteration-num &optional status-callback end-tools)
  "Run an iteration of the ekg agent with PROMPT and ITERATION-NUM.
PROMPT is the chat prompt for the LLM.
ITERATION-NUM is the current iteration count.
STATUS-CALLBACK is a function that takes a string argument, which is the
status of the agent.  If the status is \\='done or \\='error, the agent has
finished.
END-TOOLS is a list of tool names that will end the iteration.

This is to start, and after every tool call to continue the agent
session."
  (if (> iteration-num 6)
      (progn
        (message "ekg agent exceeded maximum number of iterations")
        (when status-callback (funcall status-callback 'done)))
    (llm-chat-async
     (ekg-llm--provider) prompt
     (lambda (result)
       (let ((result-alist (plist-get result :tool-results))
             (end-tools (or end-tools '("end"))))
         (if (seq-find (lambda (end-tool) (assoc-default end-tool result-alist)) end-tools)
             (when status-callback (funcall status-callback 'done))
           (let ((tools-ran (mapconcat #'car result-alist ", ")))
             (if status-callback
                 (funcall status-callback tools-ran)
               (message "Iteration %d: Ran tools: %s" (+ 1 iteration-num) tools-ran))
             (ekg-agent--iterate prompt (+ 1 iteration-num) status-callback end-tools)))))
     (lambda (_ err)
       (when status-callback (funcall status-callback 'error))
       (error "%s" err))
     t)))

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
  "Schedule the daily agent evaluation to run at `ekg-agent-daily-time`.
If already scheduled, cancels the existing timer and creates a new one."
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
           (prompt (concat ekg-agent-instructions-intro
                           "\n\nYour instructions:\n"
                           instructions-for-use
                           "\n\nYou have access to tools to help you.
After each tool call you will be given a chance to make more tool calls.
When you are done, you can call the end tool to indicate that you are
finished.  Try to make no more than 4 tool calls before calling the end
tool to finish your work.  Although you can create a note or rewrite the
note, prefer to append to the note by default, unless the user is asking
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
                            (lambda (status)
                              (if (memq status '(done error))
                                  (delete-overlay overlay)
                                (overlay-put overlay 'after-string
                                             (propertize (format " [LLM response: %s]" status)
                                                         'face 'shadow)))))))))

;; Redefine the keys, take the binding over from ekg-llm.
(define-key ekg-capture-mode-map (kbd "C-c .") #'ekg-agent-note-response)
(define-key ekg-edit-mode-map (kbd "C-c .") #'ekg-agent-note-response)

(defun ekg-agent-show-internal-notes ()
  "Show notes with agent internal tags."
  (interactive)
  (ekg-show-notes-with-any-tags (list ekg-agent-self-info-tag ekg-agent-self-instruct-tag)))

(provide 'ekg-agent)

;;; ekg-agent.el ends here
