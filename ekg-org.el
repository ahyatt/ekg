;;; ekg-org.el --- Letting ekg act as a source of data for org  -*- lexical-binding: t -*-

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
;; This module allows ekg to act as a source of tasks for org-mode.

;;; Code:

(require 'org-element)
(require 'seq)
(require 'ekg)
(require 'llm)
(require 'ekg-agent)

;; Forward declarations for variables defined later in this file.
(defvar ekg-org-tool-add-task)
(defvar ekg-org-tool-set-status)
(defvar ekg-org-tool-list-items)

(defconst ekg-org-state-tag-prefix "org/state/"
  "Prefix for EKG tags representing Org TODO states.")

(defconst ekg-org-task-tag "org/task"
  "Tag used to identify EKG notes that should be treated as Org tasks.")

(defconst ekg-org-archive-tag "org/archive"
  "Tag used to identify EKG notes that should be treated as archived Org tasks.")

(defun ekg-org--note-title (note)
  "Return the title string of NOTE, or nil if it has no title."
  (car (plist-get (ekg-note-properties note) :titled/title)))

(defun ekg-org-add-schema ()
  "Add schema for integration with `org-mode'."
  (triples-add-schema ekg-db 'org
                      '(deadline :base/type integer :base/unique t)
                      '(scheduled :base/type integer :base/unique t)
                      ;; We assume here that all org notes have the standard int ids.
                      '(parent :base/type integer :base/unique t)
                      '(children :base/virtual-reversed org/parent))
  ;; Mark 'org' as an ekg note type so it's managed with notes
  (triples-set-type ekg-db 'org 'ekg-note-type))

(add-hook 'ekg-add-schema-hook #'ekg-org-add-schema)

(defun ekg-org-get-tasks (&optional archive)
  "Fetch top-level tasks from ekg, as ekg-note structs.

If ARCHIVE is non-nil, fetch archived tasks instead.  If nil, fetch
active, unarchived, tasks."
  (seq-filter
   (lambda (note)
     (and (ekg-note-active-p note)
          (ekg-note-is-content-p note)
          (let ((props (ekg-note-properties note)))
            (and
             (plist-get props :titled/title)
             ;; Only top-level tasks
             (not (plist-get props :org/parent))
             (let ((tags (ekg-note-tags note)))
               (if archive
                   (member ekg-org-archive-tag tags)
                 (not (member ekg-org-archive-tag tags))))))))
   (ekg-get-notes-with-parent-tag ekg-org-task-tag)))

(defun ekg-org-get-child-notes-of-id (id)
  "Fetch child notes of a given note ID."
  (seq-filter #'ekg-note-active-p
              (mapcar (lambda (row) (ekg-get-note-with-id (car row)))
                      (triples-db-select ekg-db nil 'org/parent id))))

(defun ekg-org--format-timestamp (timestamp)
  "Parse TIMESTAMP integer into an Org timestamp string."
  (format-time-string "<%Y-%m-%d %a %H:%M>" (seconds-to-time timestamp)))

(defun ekg-org--to-timestamp (ts-string)
  "Convert TS-STRING to a unix timestamp integer."
  (time-convert (date-to-time ts-string) 'integer))

(defun ekg-org-task-to-element (note parent)
  "Convert an EKG NOTE to an org-element node.

PARENT is the parent org-element node."
  (let* ((props (ekg-note-properties note))
         (title (plist-get props :titled/title))
         (id (format "%s" (ekg-note-id note)))
         (state (ekg-org--state note))
         (deadline (let ((d (plist-get props :org/deadline)))
                     (when d (org-timestamp-from-time (time-convert d t)))))
         (scheduled (let ((s (plist-get props :org/scheduled)))
                      (when s (org-timestamp-from-time (time-convert s t))))))
    (let ((element (org-element-create
                    'headline
                    `(:level ,(+ 1 (or (org-element-property :level parent) 0))
                             :title ,title
                             :tags ,(seq-filter
                                     (lambda (tag) (and (not (string-prefix-p ekg-org-state-tag-prefix tag))
                                                        (not (string-equal tag ekg-org-task-tag))
                                                        (not (string-equal tag ekg-org-archive-tag))))
                                     (ekg-note-tags note))
                             :todo-keyword ,state
                             ,@(when deadline `(:deadline ,deadline))
                             ,@(when scheduled `(:scheduled ,scheduled))))))
      (org-element-set-contents
       element
       (append
        (list
         (org-element-create 'property-drawer nil
                             (org-element-create 'node-property `(:key "EKG_ID" :value ,id)))
         (org-element-create 'paragraph `(:post-blank 1)
                             (format "EKG Entry: [[ekg-note:%s][View in EKG]]" id)))
        (let ((text (ekg-display-note-text note)))
          (when (and text (not (string-empty-p text)))
            (with-temp-buffer
              (insert text)
              (org-element-contents (org-element-parse-buffer)))))
        (mapcar (lambda (child-note) (ekg-org-task-to-element child-note element))
                (ekg-org-get-child-notes-of-id (ekg-note-id note)))))
      element)))

(defun ekg-org-task-to-string (note)
  "Turn NOTE and all its children into an `org-mode' string."
  (org-element-interpret-data
   (ekg-org-task-to-element note nil)))

(defun ekg-org-generate-org-content (&optional archive filter)
  "Generate Org formatted content from EKG tasks.

If ARCHIVE is nil, use active tasks, if non-nil, use archived tasks.

Apply FILTER to filter tasks; it should be a function that takes
an ekg-note and returns nil to exclude it."
  (with-temp-buffer
    (let ((tasks
           (seq-filter
            (or filter #'identity)
            (ekg-org-get-tasks archive))))
      (dolist (task tasks)
        (insert (ekg-org-task-to-string task)
                "\n"))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ekg-org--org-note-p (note)
  "Return t if NOTE is an org task note."
  (or
   (triples-get-type ekg-db (ekg-note-id note) 'org)
   (member ekg-org-task-tag (ekg-note-tags note))))

(defun ekg-org-change-state (new-state)
  "Change the state of the current org task to NEW-STATE.

NEW-STATE is one of the standard org states."
  (interactive (list (completing-read "New state: " org-todo-keywords-1)))
  (let ((ekg-note (ekg-current-note-or-error-expanded)))
    (setf (ekg-note-tags ekg-note)
          (cons
           (concat ekg-org-state-tag-prefix (downcase new-state))
           (seq-remove
            (lambda (tag) (string-prefix-p ekg-org-state-tag-prefix tag))
            (ekg-note-tags ekg-note))))
    ;; We save unless we're currently editing the note.
    (unless (member 'ekg-edit-mode local-minor-modes)
      (ekg-save-note ekg-note)
      (when (member 'ekg-notes-mode local-minor-modes)
        (ekg-notes-refresh)))))

(defun ekg-org-capture (title)
  "Capture a new org task with TITLE into EKG."
  (interactive "sTask title: ")
  (ekg-capture :mode 'org-mode
               :properties (list
                            :titled/title (list title))
               :tags (list ekg-org-task-tag
                           (format "%s%s" ekg-org-state-tag-prefix "todo"))))

(defun ekg-org--get-hierarchy (note &optional max-depth)
  "Return the org hierarchy for NOTE as a list, from root to NOTE.
Walk up the parent chain, collecting notes.  MAX-DEPTH limits the
traversal depth to avoid infinite loops; defaults to 10."
  (let ((hierarchy (list note))
        (depth (or max-depth 10))
        (current note))
    (while (and (> depth 0)
                (let ((parent-id (plist-get (ekg-note-properties current) :org/parent)))
                  (when parent-id
                    (let ((parent (ekg-get-note-with-id parent-id)))
                      (when parent
                        (push parent hierarchy)
                        (setq current parent)
                        t)))))
      (cl-decf depth))
    hierarchy))

(defun ekg-org--hierarchy-to-text (hierarchy)
  "Format HIERARCHY (a list of notes from root to leaf) as context text.
Each level is indented to show the nesting structure."
  (let ((depth 0))
    (mapconcat
     (lambda (note)
       (let* ((title (or (ekg-org--note-title note) "(untitled)"))
              (state (condition-case nil (ekg-org--state note) (error "UNKNOWN")))
              (text (string-trim (or (ekg-note-text note) "")))
              (indent (make-string (* 2 depth) ?\s))
              (result (concat
                       (format "%s[%s] %s (id: %s)\n" indent state title (ekg-note-id note))
                       (when (and text (not (string-empty-p text)))
                         (format "%s  %s\n" indent
                                 (replace-regexp-in-string
                                  "\n" (concat "\n" indent "  ") text))))))
         (cl-incf depth)
         result))
     hierarchy "")))

(defun ekg-org-agent-plan-task ()
  "Plan the current task and add the plan as child tasks using the agent."
  (let* ((ekg-note (ekg-current-note-or-error-expanded))
         (parent-id (ekg-note-id ekg-note))
         (parent-note (ekg-get-note-with-id parent-id))
         (question (format "Given the task '%s', create a plan to accomplish it by creating subtasks using the tool to add ekg org tasks or add information to existing ekg note tasks. The parent ekg note id is '%s'."
                           (ekg-org--note-title parent-note)
                           parent-id)))
    (ekg-agent-ask-with-note question parent-id (list ekg-org-tool-add-task))))

(defun ekg-org-agent-run-task ()
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
                                       ekg-org-tool-add-task
                                       ekg-org-tool-set-status
                                       ekg-org-tool-list-items))
                         :tool-options (make-llm-tool-options :tool-choice 'any))
                        0
                        (ekg-agent--make-status-callback)
                        '("set_org_item_status")
                        (ekg-agent--timeout-deadline))))

(defun ekg-org-fs-handler (operation &rest args)
  "Fake our ekg data as a file.

OPERATION is the file operation being performed.
ARGS are additional arguments to the operation."
  (let ((filename (car args)))
    (cond
     ;; 1. Emacs asks: "Does this file exist?" -> YES
     ((eq operation 'file-exists-p) t)

     ;; 2. Emacs asks: "Is it readable?" -> YES
     ((eq operation 'file-readable-p) t)

     ;; 3. Emacs asks: "What is the real path?" -> Just return the input
     ((eq operation 'file-truename) filename)

     ;; 4. Emacs asks: "What are the attributes?" (Size, ModTime, etc) Org
     ;; Agenda checks this. We use the db's file, so when it updates, the agenda
     ;; will see that it has updated.
     ((eq operation 'file-attributes)
      ;; (t/nil nlinks uid gid atime mtime ctime size modes ...)
      (let ((num-notes (length
                        (ekg-org-get-tasks (string-match ".*archive" filename))))
            (db-attributes (file-attributes ekg-db-file)))
        (list
         nil ;; is-directory
         nil ;; nlinks
         (nth 2 db-attributes) ;; uid
         (nth 3 db-attributes) ;; gid
         (nth 4 db-attributes) ;; atime
         (nth 5 db-attributes) ;; mtime
         (nth 6 db-attributes) ;; ctime
         (* 1000 num-notes)    ;; size (estimate)
         (nth 8 db-attributes) ;; modes
         (nth 9 db-attributes) ;; inode
         (nth 10 db-attributes) ;; device
         )))

     ;; 5. Emacs asks: "Read the file!" -> WE GENERATE IT
     ((eq operation 'insert-file-contents)
      (let ((content (ekg-org-generate-org-content
                      (string-match ".*archive" filename))))
        (setq-local buffer-file-name filename)
        (insert content)
        ;; Return value must be (filename size)
        (list filename (length content))))

     ;; 6. Emacs asks: "Expand this path" (resolve ./ or ~/)
     ;; We just ensure the prefix stays intact.
     ((eq operation 'expand-file-name)
      (if (file-name-absolute-p filename)
          filename
        (concat "/ekg:" filename)))

     ;; 7. Catch-all: If we didn't handle it, fail gracefully or pass through.
     (t (let ((inhibit-file-name-handlers
               (cons 'ekg-org-fs-handler
                     (and (eq inhibit-file-name-operation operation)
                          inhibit-file-name-handlers)))
              (inhibit-file-name-operation operation))
          (apply operation args))))))

(add-to-list 'file-name-handler-alist '("\\`/ekg:" . ekg-org-fs-handler))

(defun ekg-org-revert-buffers (note)
  "Revert any buffers visiting the fake ekg org files.
Optionally check NOTE to only revert when org tasks change."
  (when (or (null note) (ekg-org--org-note-p note))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name
                   (string-match "\\`/ekg:" buffer-file-name)
                   (buffer-modified-p buf))
          (revert-buffer t t t))))))

;; When we save an org note, any org buffers showing our fake files should
;; update to reflect the changes.
(add-hook 'ekg-note-save-hook #'ekg-org-revert-buffers)

;; Add the fake file to Org Agenda
(add-to-list 'org-agenda-files "/ekg:tasks.org")

;; We need archive to open up as org, and it doesn't by default, which is odd.
;; But without this, we get an error.
(add-to-list 'auto-mode-alist '("\\.org_archive" . org-mode))

(defun ekg-org--agent-tool-add-item (title content tags parent-id status deadline scheduled)
  "Add a new org task item to EKG.

TITLE is the task title.
CONTENT is the task content/description.
TAGS is a list of additional tags.
PARENT-ID is the parent task ID (nil for no parent)
STATUS is the task status (will be converted to uppercase).
DEADLINE is the deadline timestamp string (ignored if empty).
SCHEDULED is the scheduled timestamp string (ignored if empty)."
  (ekg-agent--with-error-as-text
    (let* ((note (ekg-note-create :text content
                                  :tags (append (list ekg-org-task-tag) tags
                                                (when (and status (not (string-empty-p status)))
                                                  (list (concat ekg-org-state-tag-prefix (downcase status)))))
                                  :properties (list :titled/title (list title)))))
      ;; Handle parent ID
      (when (and parent-id (not (equal parent-id 0)) (not (string-empty-p (format "%s" parent-id))))
        (setf (ekg-note-properties note) (plist-put (ekg-note-properties note) :org/parent parent-id)))
      ;; Handle deadline
      (when (and deadline (not (string-empty-p deadline)))
        (setf (ekg-note-properties note) (plist-put (ekg-note-properties note) :org/deadline (ekg-org--to-timestamp deadline))))
      ;; Handle scheduled
      (when (and scheduled (not (string-empty-p scheduled)))
        (setf (ekg-note-properties note) (plist-put (ekg-note-properties note) :org/scheduled (ekg-org--to-timestamp scheduled))))
      ;; Save the note
      (ekg-save-note note)
      (format "Added note with ID %s" (ekg-note-id note)))))

(defun ekg-org--agent-tool-set-status (id status)
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

(defun ekg-org--state (note)
  "Get the org state of NOTE."
  (let ((tags (ekg-note-tags note)))
    (let ((tag (car (seq-filter
                     (lambda (tag) (string-prefix-p ekg-org-state-tag-prefix tag))
                     tags))))
      (if tag
          (upcase (string-replace ekg-org-state-tag-prefix "" tag))
        (error "No org state tag found for note ID %s" (ekg-note-id note))))))

(defun ekg-org--agent-tool-list-items (&optional state)
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

(defconst ekg-org-tool-add-task
  (llm-make-tool
   :function #'ekg-org--agent-tool-add-item
   :name "add_org_item"
   :description "Add a new org-mode task item"
   :args
   '((:name "title" :type string :description "The title/headline of the task" :require t)
     (:name "content" :type string :description "The content/description of the task" :required t)
     (:name "tags" :type array :items (:type string)
            :description "Additional tags for the task (org tags and agent tags will be added automatically)")
     (:name "parent_id" :type integer :description "The parent task ID if exists")
     (:name "status" :type string :description "The task status (TODO, DONE, etc.), will default to TODO if not set")
     (:name "deadline" :type string :description "The deadline timestamp in ISO 8601 format")
     (:name "scheduled" :type string :description "The scheduled timestamp in ISO 8601 format"))))

(defconst ekg-org-tool-set-status
  (llm-make-tool
   :function #'ekg-org--agent-tool-set-status
   :name "set_org_item_status"
   :description "Set the status of an org-mode task item.  Use this when you want to change the status of an existing task, for example setting it to DONE when completing it, or marking it WAITING if it's on hold."
   :args
   '((:name "id" :type integer :description "The ID of the task item" :required t)
     (:name "status" :type string :description "The new status of the task (TODO, DONE, etc.)" :required t))))

(defconst ekg-org-tool-list-items
  (llm-make-tool
   :function #'ekg-org--agent-tool-list-items
   :name "list_org_items"
   :description "Return all matching org-mode task items as an Org formatted string."
   :args
   '((:name "state" :type string
            :description "Filter tasks by state (TODO, DONE, etc.)"))))

(defun ekg-org-initialize ()
  "Initialize the ekg-org integration."
  (ekg-org-add-schema)
  (when (featurep 'ekg-agent)
    (add-to-list 'ekg-agent-extra-tools ekg-org-tool-add-task)
    (add-to-list 'ekg-agent-extra-tools ekg-org-tool-set-status)
    (add-to-list 'ekg-agent-extra-tools ekg-org-tool-list-items)))

(provide 'ekg-org)
;;; ekg-org.el ends here
