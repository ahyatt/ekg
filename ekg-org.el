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

(defconst ekg-org-state-tag-prefix "org/state/"
  "Prefix for EKG tags representing Org TODO states.")

(defconst ekg-org-task-tag "org/task"
  "Tag used to identify EKG notes that should be treated as Org tasks.")

(defconst ekg-org-archive-tag "org/archive"
  "Tag used to identify EKG notes that should be treated as archived Org tasks.")

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
  (mapcar (lambda (row) (ekg-get-note-with-id (car row)))
          (triples-db-select ekg-db nil 'org/parent-id id)))

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
                             :tags ,(ekg-note-tags note)
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
  (unless (and (member 'ekg-edit-mode local-minor-modes)
               (ekg-org--org-note-p ekg-note))
    (error "Not in an EKG org task buffer"))
  (setf (ekg-note-tags ekg-note)
        (cons
         (concat ekg-org-state-tag-prefix (downcase new-state))
         (seq-remove
          (lambda (tag) (string-prefix-p ekg-org-state-tag-prefix tag))
          (ekg-note-tags ekg-note)))))

(defun ekg-org-capture (title)
  "Capture a new org task with TITLE into EKG."
  (interactive "sTask title: ")
  (ekg-capture :mode 'org-mode
               :properties (list
                            :titled/title (list title))
               :tags (list ekg-org-task-tag
                           (format "%s%s" ekg-org-state-tag-prefix "todo"))))

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
    (format "Added note with ID %s" (ekg-note-id note))))

(defun ekg-org--agent-tool-set-status (id status)
  "Set the status of an org task item.

ID is the note ID.
STATUS is the new status (will be converted to uppercase)."
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
    (format "Set status of note ID %s to %s" id status)))

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
INCLUDE-ARCHIVED if non-nil, include archived tasks.
Returns text in Org format, as if they were in an Org file."
  (ekg-org-generate-org-content nil (when state
                                      (lambda (note)
                                        (string-equal
                                         (downcase state)
                                         (downcase (ekg-org--state note)))))))

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
   :description "Set the status of an org-mode task item"
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
