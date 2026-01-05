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
  (make-llm-tool :function (lambda () (ekg-tags))
                 :name "list_all_tags"
                 :description "List all existing tags in the ekg database."
                 :args '()))

(defconst ekg-agent-tool-create-note
  (make-llm-tool :function (lambda (tags content mode)
                             (unless (member mode '("org-mode" "markdown-mode" "text-mode"))
                               (error "Unsupported mode: %s" mode))
                             (let* ((enclosure (assoc-default major-mode ekg-llm-format-output nil '("_BEGIN_" . "_END_")))
                                    (note (ekg-note-create :tags (seq-union (append tags nil)
                                                                            (list ekg-agent-author-tag))
                                                           :text (concat
                                                                  (car enclosure)
                                                                  content
                                                                  (cdr enclosure))
                                                           :mode (intern mode))))
                               (ekg-save-note note)))
                 :name "create_note"
                 :description "Create a new note with specified tags and content."
                 :args '((:name "tags" :type array :items (:type string)
                                :description "List of tags to assign to the new note.  The tags should preferably be preexisting tags, but new tags can be created as well.")
                         (:name "content" :type string :description "The content of the new note.")
                         (:name "mode" :type string :enum ["markdown-mode" "org-mode" "text-mode"]
                                :description "The emacs mode of the note content (e.g., 'org-mode', 'markdown-mode', 'text-mode')."))))

(defconst ekg-agent-tool-end
  (make-llm-tool :function (lambda () 'done)
                 :name "end"
                 :description "Indicate that no further actions need to be taken."
                 :args '()))

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
"
   ekg-agent-self-info-tag ekg-agent-self-instruct-tag))

(defun ekg-agent-evaluate-status ()
  "Run the ekg agent to evaluate status and help the user.
The agent will review recent notes and TODO items, then decide whether
to create new notes or perform other actions to help the user."
  (interactive)
  (ekg-agent--iterate (llm-make-chat-prompt
                       (concat ekg-agent-instructions-intro (ekg-agent-instructions-evaluate-status)
                               (ekg-agent-starting-context))
                       :tools (append
                               (list
                                ekg-agent-tool-all-tags
                                ekg-agent-tool-any-tags
                                ekg-agent-tool-get-note-by-id
                                ekg-agent-tool-search-notes
                                ekg-agent-tool-list-tags
                                ekg-agent-tool-create-note
                                ekg-agent-tool-end)
                               ;; Use Google Search as well, if possible.
                               (when (llm-google-p (ekg-llm--provider))
                                 (list (make-llm-tool :function #'ignore
                                                      :name "google_search"
                                                      :description "Google Search built-in tool"
                                                      :args nil)))))
                      0))

(defun ekg-agent--iterate (prompt iteration-num)
  "Run an iteration of the ekg agent with PROMPT and ITERATION-NUM.
PROMPT is the chat prompt for the LLM.
ITERATION-NUM is the current iteration count.

This is to start, and after every tool call to continue the agent
session."
  (if (> iteration-num 6)
      (message "ekg agent exceeded maximum number of iterations")
    (llm-chat-async
     (ekg-llm--provider) prompt
     (lambda (result)
       (let ((result-alist (plist-get result :tool-results)))
         (unless (assoc-default "end" result-alist)
           (message "Iteration %d: Ran tools: %s" (+ 1 iteration-num)
                    (mapcar #'car result-alist))
           (ekg-agent--iterate prompt (+ 1 iteration-num)))))
     (lambda (_ err) (error "%s" err))
     t)))

(defun ekg-agent-evaluate-status-daily ()
  "Run the ekg agent to evaluate status as a daily routine."
  (interactive)
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

(provide 'ekg-agent)

;;; ekg-agent.el ends here
