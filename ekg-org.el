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

;; Forward declarations for variables defined in ekg-agent.el.
(defvar ekg-agent-extra-tools)
(defvar ekg-agent-org-tool-add-task)
(defvar ekg-agent-org-tool-create-plan)
(defvar ekg-agent-org-tool-set-status)
(defvar ekg-agent-org-tool-list-items)

(defvar ekg-org--inhibit-view-refresh nil
  "When non-nil, defer `ekg-org-view' refreshes from save hooks.
Bind this around batch operations that save multiple notes; call
`ekg-org-view--refresh-all' once afterward to refresh synchronously.")

(defconst ekg-org-state-tag-prefix "org/state/"
  "Prefix for EKG tags representing Org TODO states.")

(defconst ekg-org-task-tag "org/task"
  "Tag used to identify EKG notes that should be treated as Org tasks.")

(add-to-list 'ekg-hidden-tags ekg-org-task-tag)

(defconst ekg-org-archive-tag "org/archive"
  "Tag used to identify EKG notes that should be treated as archived Org tasks.")

(defconst ekg-org-dependency-types '(sequential parallel)
  "Supported dependency modes for the children of an Org task.")

(defun ekg-org--response-inheritable-tag-p (tag _note)
  "Return non-nil when TAG is not Org task control data."
  (and (not (string-prefix-p ekg-org-state-tag-prefix tag))
       (not (member tag (list ekg-org-task-tag ekg-org-archive-tag)))))

(add-hook 'ekg-response-tag-filter-functions
          #'ekg-org--response-inheritable-tag-p)

(defun ekg-org--note-title (note)
  "Return the title string of NOTE, or nil if it has no title."
  (let ((title (plist-get (ekg-note-properties note) :titled/title)))
    (if (listp title) (car title) title)))

(defun ekg-org-add-schema ()
  "Add schema for integration with `org-mode'."
  (triples-db-delete ekg-db 'org/depends-on 'base/type)
  (triples-add-schema ekg-db 'org
                      '(deadline :base/type integer :base/unique t)
                      '(scheduled :base/type integer :base/unique t)
                      '(sort-order :base/type integer :base/unique t)
                      'depends-on
                      ;; Generic org properties, stored as readable cons
                      ;; cells "(KEY . VALUE)" following org property
                      ;; syntax: keys are uppercase strings,
                      ;; case-insensitive, values are strings.
                      '(property :base/type string))
  ;; Mark 'org' as an ekg note type so it's managed with notes
  (triples-set-type ekg-db 'org 'ekg-note-type))

(add-hook 'ekg-add-schema-hook #'ekg-org-add-schema)

;; These are set at load time rather than in the schema hook, which
;; only runs on database version changes.
(add-to-list 'ekg-header-hidden-properties :org/sort-order)
(add-to-list 'ekg-header-hidden-properties :org/property)

(defun ekg-org-properties-alist (note)
  "Return the org properties of NOTE as an alist of (KEY . VALUE) pairs.
Keys are uppercase strings, values are strings, following org property
syntax.  See Info node `(org) Property Syntax'."
  (mapcar #'read (or (plist-get (ekg-note-properties note) :org/property) nil)))

(defun ekg-org-get-property (note key)
  "Return the value of org property KEY on NOTE, or nil.
KEY is case-insensitive."
  (let ((ukey (upcase key)))
    (cdr (assoc ukey (ekg-org-properties-alist note)))))

(defun ekg-org-set-property (note key value)
  "Set org property KEY to VALUE on NOTE.
KEY is case-insensitive and stored uppercase.  VALUE is a string.
The note is not saved; call `ekg-save-note' afterward."
  (let* ((ukey (upcase key))
         (entries (or (plist-get (ekg-note-properties note) :org/property) nil))
         (new-entry (prin1-to-string (cons ukey value)))
         (filtered (seq-remove
                    (lambda (entry)
                      (string= (car (read entry)) ukey))
                    entries))
         ;; Rebuild props without any :org/property keys to avoid
         ;; duplicates (ekg-get-note-with-id can produce duplicate
         ;; plist keys via extra-props).
         (clean-props (ekg--plist-without-key
                       (ekg-note-properties note) :org/property)))
    (setf (ekg-note-properties note)
          (plist-put clean-props :org/property (cons new-entry filtered)))))

(defun ekg-org-remove-property (note key)
  "Remove org property KEY from NOTE.
KEY is case-insensitive.  The note is not saved."
  (let* ((ukey (upcase key))
         (entries (or (plist-get (ekg-note-properties note) :org/property) nil))
         (filtered (seq-remove
                    (lambda (entry)
                      (string= (car (read entry)) ukey))
                    entries))
         (clean-props (ekg--plist-without-key
                       (ekg-note-properties note) :org/property)))
    (setf (ekg-note-properties note)
          (plist-put clean-props :org/property filtered))))

(defun ekg-org-working-directory (note)
  "Return NOTE's working directory, inheriting the nearest DIR property.
Return nil when neither NOTE nor its ancestors has DIR set.  Relative
paths are resolved against `default-directory'.  Signal a user error
if the configured directory is empty, missing, or inaccessible."
  (let ((current note) seen directory)
    (while (and current (not directory))
      (when (member (ekg-note-id current) seen)
        (user-error "Cycle in task parents at %s" (ekg-note-id current)))
      (push (ekg-note-id current) seen)
      (setq directory (ekg-org-get-property current "DIR"))
      (unless directory
        (setq current (when-let* ((parent (ekg-note-parent-id current)))
                        (ekg-get-note-with-id parent)))))
    (when directory
      (when (string-empty-p (string-trim directory))
        (user-error "Task DIR property must not be empty"))
      (setq directory (file-name-as-directory (expand-file-name directory)))
      (unless (and (file-directory-p directory)
                   (file-accessible-directory-p directory))
        (user-error "Task directory is missing or inaccessible: %s" directory))
      directory)))

(defun ekg-org-dependency-type (note)
  "Return the dependency mode for NOTE's children.
The return value is `parallel' when NOTE has an explicitly false
ORDERED Org property and `sequential' otherwise."
  (let ((ordered (ekg-org-get-property note "ORDERED")))
    (if (and ordered
             (member (downcase ordered) '("" "nil" "no" "false")))
        'parallel
      'sequential)))

(defun ekg-org-set-dependency-type (note dependency-type)
  "Set how NOTE's children depend on one another.
DEPENDENCY-TYPE must be `sequential' or `parallel'.  Sequential
children use Org's ORDERED property.  Parallel children store an
explicit false value so they remain distinguishable from the
sequential default.  The note is not saved."
  (unless (memq dependency-type ekg-org-dependency-types)
    (error "Invalid Org dependency type: %s" dependency-type))
  (if (eq dependency-type 'sequential)
      (ekg-org-set-property note "ORDERED" "t")
    (ekg-org-set-property note "ORDERED" "nil"))
  note)

(defun ekg-org--property-drawer (note)
  "Return an Org property drawer element for NOTE."
  (let* ((properties
          ;; EKG_ID is generated from the actual note ID.  ORDERED is
          ;; normalized below from the dependency mode.
          (seq-remove (lambda (property)
                        (member (car property) '("EKG_ID" "ORDERED")))
                      (ekg-org-properties-alist note)))
         (properties
          (if (and (ekg-org--org-note-p note)
                   (eq (ekg-org-dependency-type note) 'sequential))
              (cons '("ORDERED" . "t") properties)
            properties)))
    (apply
     #'org-element-create 'property-drawer nil
     (org-element-create
      'node-property
      `(:key "EKG_ID" :value ,(format "%s" (ekg-note-id note))))
     (mapcar
      (lambda (property)
        (org-element-create
         'node-property
         `(:key ,(car property) :value ,(cdr property))))
      properties))))

(defun ekg-org--note-active-p (note)
  "Return whether NOTE is visible to the Org integration.
Allow the task tag, while respecting other hidden tags."
  (let ((ekg-hidden-tags (remove ekg-org-task-tag ekg-hidden-tags)))
    (ekg-note-active-p note)))

(defun ekg-org-get-tasks (&optional archive)
  "Fetch top-level tasks from ekg, as ekg-note structs.

If ARCHIVE is non-nil, fetch archived tasks instead.  If nil, fetch
active, unarchived, tasks."
  (ekg-connect)
  (let* ((ekg-hidden-tags (remove ekg-org-task-tag ekg-hidden-tags))
         (all-ids (plist-get (triples-get-type ekg-db ekg-org-task-tag 'tag)
                             :tagged))
         ;; Pre-filter: exclude IDs that have a parent, since we only
         ;; want top-level tasks.  This avoids loading child notes from
         ;; the database just to discard them.
         (child-id-set (make-hash-table :test 'equal))
         (_ (dolist (row (triples-db-select ekg-db nil
                                            'hierarchy/parent nil))
              (puthash (car row) t child-id-set)))
         (top-ids (seq-remove (lambda (id) (gethash id child-id-set))
                              all-ids)))
    (seq-filter
     (lambda (note)
       (and (ekg-note-active-p note)
            (ekg-note-is-content-p note)
            (let ((tags (ekg-note-tags note)))
              (if archive
                  (member ekg-org-archive-tag tags)
                (not (member ekg-org-archive-tag tags))))))
     (delq nil (mapcar #'ekg-get-note-with-id top-ids)))))

(defun ekg-org-get-child-notes-of-id (id)
  "Fetch child notes of a given note ID."
  (let ((ekg-hidden-tags (remove ekg-org-task-tag ekg-hidden-tags)))
    (ekg-note-child-notes id t)))

(defun ekg-org-get-child-tasks-of-id (id)
  "Fetch active Org task children of ID."
  (seq-filter #'ekg-org--org-note-p
              (ekg-org-get-child-notes-of-id id)))

(defun ekg-org--format-timestamp (timestamp)
  "Parse TIMESTAMP integer into an Org timestamp string."
  (format-time-string "<%Y-%m-%d %a %H:%M>" (seconds-to-time timestamp)))

(defun ekg-org--to-timestamp (ts-string)
  "Convert TS-STRING to a unix timestamp integer."
  (time-convert (date-to-time ts-string) 'integer))

(defun ekg-org--timestamp-from-epoch (epoch)
  "Return an Org timestamp for EPOCH, including a non-midnight time."
  (let* ((time (seconds-to-time epoch))
         (decoded (decode-time time)))
    (org-timestamp-from-time
     time
     (or (not (zerop (decoded-time-hour decoded)))
         (not (zerop (decoded-time-minute decoded)))))))

(defun ekg-org-task-to-element (note parent)
  "Convert an EKG NOTE to an org-element node.

PARENT is the parent org-element node."
  (let* ((props (ekg-note-properties note))
         (task-p (ekg-org--org-note-p note))
         (title (or (ekg-org--note-title note)
                    (unless task-p "Response")))
         (id (format "%s" (ekg-note-id note)))
         (state (when task-p (ekg-org--state note)))
         (deadline (let ((d (plist-get props :org/deadline)))
                     (when d (ekg-org--timestamp-from-epoch d))))
         (scheduled (let ((s (plist-get props :org/scheduled)))
                      (when s (ekg-org--timestamp-from-epoch s)))))
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
        (append
         (when (or deadline scheduled)
           (list
            (org-element-create
             'planning
             `(,@(when deadline `(:deadline ,deadline))
               ,@(when scheduled `(:scheduled ,scheduled))))))
         (list
          (ekg-org--property-drawer note)
          (org-element-create 'paragraph `(:post-blank 1)
                              (format "EKG Entry: [[ekg-note:%s][View in EKG]]" id))))
        (let ((text (ekg-display-note-text note)))
          (when (and text (not (string-empty-p text)))
            (with-temp-buffer
              (insert text)
              (org-element-contents (org-element-parse-buffer)))))
        (let* ((children (ekg-org-get-child-notes-of-id (ekg-note-id note)))
               (tasks (seq-filter #'ekg-org--org-note-p children))
               (responses (seq-remove #'ekg-org--org-note-p children))
               (ordered (append (sort tasks #'ekg-org-view--sort-predicate)
                                responses)))
          (mapcar (lambda (child-note) (ekg-org-task-to-element child-note element))
                  ordered))))
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
  (member ekg-org-task-tag (ekg-note-tags note)))

(defun ekg-org-change-state (new-state)
  "Change the current task to NEW-STATE from `org-todo-keywords'.
While editing, change the note in memory; otherwise persist just its tags."
  (interactive
   (list (completing-read "New state: " (ekg-org--todo-keywords) nil t)))
  (unless (member new-state (ekg-org--todo-keywords))
    (user-error "Unknown TODO state: %s" new-state))
  (let ((note (ekg-current-note-or-error-expanded)))
    (unless (ekg-org--org-note-p note)
      (user-error "This action requires an Org task"))
    (setf (ekg-note-tags note)
          (cons (concat ekg-org-state-tag-prefix (downcase new-state))
                (seq-remove
                 (lambda (tag)
                   (string-prefix-p ekg-org-state-tag-prefix tag))
                 (ekg-note-tags note))))
    (unless (or ekg-edit-mode ekg-capture-mode)
      (ekg-org-view--save-tags note)
      ;; Task views refresh and preserve selection through the save hook.
      (when (and (derived-mode-p 'ekg-notes-mode)
                 (not (derived-mode-p 'ekg-org-view-mode)))
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
                (let ((parent-id (ekg-note-parent-id current)))
                  (when parent-id
                    (let ((parent (ekg-get-note-with-id parent-id)))
                      (when (and parent (ekg-org--note-active-p parent))
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
                      (string-match ".*archive" filename)))
            (replace (nth 4 args)))
        (if replace
            (let ((temp-file (make-temp-file "ekg-org-")))
              (write-region content nil temp-file nil 'silent)
              (let ((inhibit-file-name-handlers
                     (cons 'ekg-org-fs-handler inhibit-file-name-handlers))
                    (inhibit-file-name-operation 'insert-file-contents))
                (insert-file-contents temp-file nil nil nil t))
              (delete-file temp-file))
          (insert content))
        (setq-local buffer-file-name filename)
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
Optionally check NOTE to only revert when org tasks change.
Does nothing when `ekg-org--inhibit-view-refresh' is non-nil."
  (unless ekg-org--inhibit-view-refresh
    (when (or (null note) (ekg-org--org-note-p note))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and buffer-file-name
                     (string-match "\\`/ekg:" buffer-file-name)
                     (not (buffer-modified-p buf)))
            (revert-buffer t t t)))))))

;; When we save an org note, any org buffers showing our fake files should
;; update to reflect the changes.
(add-hook 'ekg-note-save-hook #'ekg-org-revert-buffers)

;; Add the fake file to Org Agenda
(add-to-list 'org-agenda-files "/ekg:tasks.org")

;; We need archive to open up as org, and it doesn't by default, which is odd.
;; But without this, we get an error.
(add-to-list 'auto-mode-alist '("\\.org_archive" . org-mode))

(defun ekg-org--todo-sequences ()
  "Return global `org-todo-keywords' in sequence form."
  (let ((keywords (default-value 'org-todo-keywords)))
    (if (seq-every-p #'stringp keywords)
        (list (cons 'sequence keywords))
      keywords)))

(defun ekg-org--todo-keywords (&optional sequences)
  "Return a flat list of TODO keyword strings from `org-todo-keywords'.
This reads directly from the global defcustom rather than the
buffer-local `org-todo-keywords-1', which is only populated inside
Org-mode buffers.  SEQUENCES optionally restricts the keyword sequences."
  (let (result)
    (dolist (entry (or sequences (ekg-org--todo-sequences)))
      (dolist (kw (cdr entry))
        (unless (equal kw "|")
          ;; Strip fast-selection key / logging specs, e.g. "WAIT(w@/!)".
          (when (string-match "^\\([^(]+\\)" kw)
            (push (match-string 1 kw) result)))))
    (nreverse result)))

(defun ekg-org--state (note)
  "Get the org state of NOTE, signaling an error when it is absent."
  (or (ekg-org-task-state note)
      (error "No org state tag found for note ID %s" (ekg-note-id note))))

;;; ekg-org-view — Org actions in the normal notes UI

(defvar-local ekg-org-view--root-id nil
  "When non-nil, show only this task and its children.")

(defvar-local ekg-org-view--archive nil
  "When non-nil, show archived tasks instead of active ones.")

(defun ekg-org-view--save-tags (note)
  "Persist the tags of NOTE without triggering full save hooks.
Only updates the tag triples and runs `ekg-note-save-hook' (not
`ekg-note-pre-save-hook').  Suitable for state-only changes where
the text content has not changed and embedding regeneration is
unnecessary."
  (let ((id (ekg-note-id note))
        (tags (mapcar #'ekg--normalize-tag (ekg-note-tags note))))
    (setf (ekg-note-tags note) tags)
    (triples-with-transaction
      ekg-db
      (triples-set-type ekg-db id 'tagged :tag tags)
      (mapc (lambda (tag) (triples-set-type ekg-db tag 'tag)) tags)
      (let ((modified-time (time-convert (current-time) 'integer)))
        (triples-set-type ekg-db id 'time-tracked
                          :creation-time (ekg-note-creation-time note)
                          :modified-time modified-time)
        (setf (ekg-note-modified-time note) modified-time))
      (run-hook-with-args 'ekg-note-save-hook note))))

(defun ekg-org-view--sort-order (note)
  "Return the sort-order of NOTE, defaulting to creation time."
  (or (plist-get (ekg-note-properties note) :org/sort-order)
      (ekg-note-creation-time note)
      0))

(defun ekg-org-view--id-less-p (a b)
  "Return non-nil if note ID A should sort before note ID B."
  (if (and (numberp a) (numberp b))
      (< a b)
    (string< (format "%s" a) (format "%s" b))))

(defun ekg-org-view--sort-predicate (a b)
  "Return non-nil if note A should sort before note B.
Uses sort-order as primary key and note ID as tiebreaker for
stability."
  (let ((oa (ekg-org-view--sort-order a))
        (ob (ekg-org-view--sort-order b)))
    (or (< oa ob)
        (and (= oa ob)
             (ekg-org-view--id-less-p (ekg-note-id a) (ekg-note-id b))))))

(defun ekg-org-view--sorted-children (id)
  "Return non-archived task children of ID, sorted by sort-order."
  (sort (seq-remove
         (lambda (child)
           (member ekg-org-archive-tag (ekg-note-tags child)))
         (ekg-org-get-child-tasks-of-id id))
        #'ekg-org-view--sort-predicate))

(defun ekg-org-view--display-children (id &optional archive)
  "Return children of ID for display, with task ordering preserved.
When ARCHIVE is non-nil, show archived descendants."
  (let* ((children
          (seq-filter
           (lambda (note)
             (let ((task-p (ekg-org--org-note-p note))
                   (archived
                    (member ekg-org-archive-tag (ekg-note-tags note))))
               (if archive
                   (or archived (not task-p))
                 (not archived))))
           (ekg-org-get-child-notes-of-id id)))
         (tasks (seq-filter #'ekg-org--org-note-p children)))
    (append (sort tasks #'ekg-org-view--sort-predicate)
            (seq-remove #'ekg-org--org-note-p children))))

(defun ekg-org-view--sorted-top-level (&optional archive)
  "Return top-level tasks, sorted by sort-order.
If ARCHIVE is non-nil, return archived tasks instead."
  (sort (ekg-org-get-tasks archive)
        #'ekg-org-view--sort-predicate))

(defun ekg-org-view--set-sort-order (id order)
  "Set the sort-order of note ID to ORDER without replacing other org properties."
  (triples-db-delete ekg-db id 'org/sort-order)
  (triples-db-insert ekg-db id 'org/sort-order order))

(defun ekg-org-view--assign-order-after (siblings current-id)
  "Return a sort-order value that places a new item after CURRENT-ID in SIBLINGS.
Also renumbers all SIBLINGS with gaps to ensure consistent spacing.
Updates sort-order directly via triples to avoid triggering full
note save hooks (embedding generation, view refreshes) for each
sibling."
  (let ((order 0)
        (insert-after nil)
        (found nil))
    (triples-with-transaction
      ekg-db
      (dolist (sib siblings)
        (when (and found (not insert-after))
          (setq insert-after order)
          (cl-incf order))
        (ekg-org-view--set-sort-order (ekg-note-id sib) order)
        (when (equal (ekg-note-id sib) current-id)
          (setq found t))
        (cl-incf order)))
    (or insert-after order)))

(defun ekg-org-task-state (note)
  "Return NOTE's TODO keyword, or nil if it has no task state."
  (when-let* ((tag (seq-find
                    (lambda (tag)
                      (string-prefix-p ekg-org-state-tag-prefix tag))
                    (ekg-note-tags note))))
    (upcase (substring tag (length ekg-org-state-tag-prefix)))))

(defun ekg-org--done-keywords (&optional sequences)
  "Return completed keywords from the global `org-todo-keywords'.
SEQUENCES optionally restricts the keyword sequences."
  (let (result)
    (dolist (sequence (or sequences (ekg-org--todo-sequences)))
      (let* ((keywords (cdr sequence))
             (separator (member "|" keywords))
             (done (if separator (cdr separator) (last keywords))))
        (dolist (keyword done)
          (push (car (split-string keyword "(")) result))))
    (nreverse result)))

(defun ekg-org-task-done-p (note)
  "Return non-nil if NOTE has a completed TODO keyword.
Use the configured `org-todo-keywords', including custom done states."
  (member (ekg-org-task-state note) (ekg-org--done-keywords)))

(defun ekg-org-task-summary (note)
  "Return a plist describing NOTE's task state and immediate task children.
Keys are :state, :done, :scheduled, :deadline, :children, :completed,
:remaining and :dependencies.  Children share NOTE's archive scope."
  (let* ((children
          (seq-filter
           #'ekg-org--org-note-p
           (ekg-org-view--display-children
            (ekg-note-id note)
            (member ekg-org-archive-tag (ekg-note-tags note)))))
         (completed (seq-count #'ekg-org-task-done-p children))
         (props (ekg-note-properties note)))
    (list :state (ekg-org-task-state note)
          :done (and (ekg-org-task-done-p note) t)
          :scheduled (plist-get props :org/scheduled)
          :deadline (plist-get props :org/deadline)
          :children (length children) :completed completed
          :remaining (- (length children) completed)
          :dependencies (ekg-org-dependency-type note))))

(defun ekg-org-view-state ()
  "Return the task view's current selection, scope and folding state.
Keys are :current-id, :root-id, :archive, :visible-ids, :folded-ids
and :display-states."
  (unless (derived-mode-p 'ekg-org-view-mode)
    (user-error "This command requires an Org task view"))
  (list :current-id (get-text-property (point) :ekg-note-id)
        :root-id ekg-org-view--root-id :archive ekg-org-view--archive
        :visible-ids (ekg-notes-visible-ids)
        :folded-ids (mapcar #'car (seq-filter
                                   (lambda (entry) (eq (cdr entry) 'folded))
                                   ekg-notes-display-states))
        :display-states (copy-tree ekg-notes-display-states)))

(defun ekg-org-view--visible-tags (note)
  "Return NOTE's tags excluding task control, trash and draft tags."
  (seq-filter (lambda (tag)
                (and (ekg-org--response-inheritable-tag-p tag note)
                     (not (member tag (list ekg-trash-tag ekg-draft-tag)))))
              (ekg-note-tags note)))

(defun ekg-display-note-org-tags (note &optional numwords &rest format)
  "Display NOTE's user tags, with NUMWORDS and FORMAT template options."
  (let ((note (copy-ekg-note note)))
    (setf (ekg-note-tags note)
          (ekg-org-view--visible-tags note))
    (apply #'ekg-display-note-tagged note numwords format)))

(defun ekg-org--format-task-date (epoch)
  "Format EPOCH with its time of day when it is not midnight."
  (let ((time (seconds-to-time epoch)))
    (format-time-string
     (if (equal (format-time-string "%H:%M:%S" time) "00:00:00")
         "%Y-%m-%d"
       "%Y-%m-%d %H:%M")
     time)))

(defun ekg-display-note-org-status (note &optional numwords &rest format)
  "Display NOTE's task status, with NUMWORDS and FORMAT template options.
With `state-only', omit dates and child completion counts."
  (when (ekg-org--org-note-p note)
    (let* ((summary (if (memq 'state-only format)
                        (list :state (ekg-org-task-state note)
                              :done (ekg-org-task-done-p note))
                      (ekg-org-task-summary note)))
           (state (or (plist-get summary :state) "No state"))
           (parts (list (propertize state 'face
                                    (if (plist-get summary :done)
                                        'org-done 'org-todo)))))
      (dolist (field '((:scheduled . "Scheduled") (:deadline . "Due")))
        (when-let* ((date (plist-get summary (car field))))
          (setq parts (append parts
                              (list (format "%s %s" (cdr field)
                                            (ekg-org--format-task-date
                                             date)))))))
      (when (> (or (plist-get summary :children) 0) 0)
        (setq parts (append parts
                            (list (format "%d/%d done · %s"
                                          (plist-get summary :completed)
                                          (plist-get summary :children)
                                          (plist-get summary :dependencies))))))
      (when (memq 'folded format)
        (setq parts (append parts '("folded"))))
      (ekg-display--format (mapconcat #'identity parts " · ")
                           numwords format))))

(defun ekg-org-view--children (note)
  "Return NOTE's children for the active or archived task view."
  (ekg-org-view--display-children (ekg-note-id note) ekg-org-view--archive))

(defun ekg-org-view--fetch-notes ()
  "Return the root notes of the current task view."
  (if ekg-org-view--root-id
      (when-let* ((note (ekg-get-note-with-id ekg-org-view--root-id)))
        (when (and (ekg-org--note-active-p note)
                   (eq (and (member ekg-org-archive-tag (ekg-note-tags note)) t)
                       (and ekg-org-view--archive t)))
          (list note)))
    (ekg-org-view--sorted-top-level ekg-org-view--archive)))

;; Navigation and task commands use the normal notes UI's IDs and depth.

(defun ekg-org-view--note-at-point ()
  "Return the note ID at point, or nil."
  (get-text-property (point) :ekg-note-id))

(defun ekg-org-view--require-task-at-point ()
  "Return the task at point, or signal a user-facing error."
  (let* ((id (ekg-org-view--note-at-point))
         (note (and id (ekg-get-note-with-id id))))
    (unless (and note (ekg-org--org-note-p note))
      (user-error "This action requires an Org task"))
    note))

(defun ekg-org-view--level-at-point ()
  "Return the task hierarchy level at point, or nil."
  (when-let* ((id (ekg-org-view--note-at-point))
              (start (text-property-any (point-min) (point-max)
                                        :ekg-note-id id))
              (depth (get-text-property start :ekg-note-depth)))
    (1+ depth)))

(defun ekg-org-view--on-heading-p ()
  "Return non-nil at the beginning of a displayed note."
  (and (get-text-property (point) 'ekg-note-id)
       (or (bobp)
           (not (equal (get-text-property (point) :ekg-note-id)
                       (get-text-property (1- (point)) :ekg-note-id))))))

(defun ekg-org-view--ensure-on-heading ()
  "Move to the beginning of the current note."
  (when-let* ((id (ekg-org-view--note-at-point)))
    (ekg-notes--goto-id id)))

(defun ekg-org-view--highlight ()
  "Update the current note's normal selection indicator."
  (ekg--note-highlight))

(defun ekg-org-view--goto-heading (direction &optional same-level)
  "Move to the next note in DIRECTION, optionally at the SAME-LEVEL."
  (let ((level (ekg-org-view--level-at-point))
        (id (ekg-org-view--note-at-point))
        (step (if (eq direction :forward) 1 -1)))
    (save-excursion
      (let (found)
        (while (and (not found)
                    (if (= step 1) (not (eobp)) (not (bobp))))
          (forward-line step)
          (when (and (ekg-org-view--on-heading-p)
                     (not (equal id (ekg-org-view--note-at-point)))
                     (or (not same-level)
                         (equal level (ekg-org-view--level-at-point))))
            (setq found (point))))
        (when found (setq id (ekg-org-view--note-at-point)))))
    (ekg-notes--goto-id id)))

;; Interactive navigation commands

(defun ekg-org-view-next-heading ()
  "Move to the next heading."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--goto-heading :forward)
  (ekg-org-view--highlight))

(defun ekg-org-view-previous-heading ()
  "Move to the previous heading."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--goto-heading :backward)
  (ekg-org-view--highlight))

(defun ekg-org-view-next-sibling ()
  "Move to the next heading at the same level."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--goto-heading :forward t)
  (ekg-org-view--highlight))

(defun ekg-org-view-previous-sibling ()
  "Move to the previous heading at the same level."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--goto-heading :backward t)
  (ekg-org-view--highlight))

(defun ekg-org-view-up-heading ()
  "Move to the parent heading."
  (interactive nil ekg-org-view-mode)
  (let ((current-level (ekg-org-view--level-at-point))
        (current-id (ekg-org-view--note-at-point))
        (found nil))
    (when (and current-level (> current-level 1))
      (save-excursion
        (while (and (not found) (not (bobp)))
          (forward-line -1)
          (let ((id (get-text-property (point) :ekg-note-id)))
            (when (and (ekg-org-view--on-heading-p)
                       (not (equal id current-id)))
              (let ((level (ekg-org-view--level-at-point)))
                (when (and level (< level current-level))
                  (setq found (point)))))))))
    (when found (goto-char found))
    (ekg-org-view--highlight)))

;; Interactive action commands

(defun ekg-org-view--goto-note-id (id)
  "Move to note ID in the current buffer, returning non-nil if found."
  (ekg-notes--goto-id id))

(defun ekg-org-view--refresh (&optional target-id)
  "Re-render the view in place without switching windows.
If TARGET-ID is non-nil, move point to that note's heading after
re-rendering, unfolding its ancestors to reveal it.  Without TARGET-ID,
preserve the note at point, or its nearest visible ancestor if folded."
  (let* ((restore-id (or target-id (ekg-org-view--note-at-point)))
         (ancestors (when-let* ((note (and restore-id
                                           (ekg-get-note-with-id restore-id))))
                      (mapcar #'ekg-note-id (ekg-note-ancestors note)))))
    ;; Actions that explicitly select a task must reveal it.  Background
    ;; refreshes retain folds and select the nearest visible ancestor.
    (when target-id
      (ekg-notes--unfold-ids ancestors))
    (ekg--show-notes
     (if ekg-org-view--archive "Archived tasks" "Org tasks")
     #'ekg-org-view--fetch-notes
     (append (list ekg-org-task-tag)
             (when ekg-org-view--archive (list ekg-org-archive-tag))))
    (when (or (null target-id)
              (seq-some #'ekg-org-view--goto-note-id
                        (cons restore-id (reverse ancestors))))
      (let ((windows (get-buffer-window-list (current-buffer) nil t)))
        (dolist (win windows)
          (when (eq win (selected-window))
            (set-window-point win (point))))
        ;; When the buffer is not visible, update the saved point in
        ;; each window's prev-buffer history so that `quit-window'
        ;; restores point to the right heading.
        (unless windows
          (dolist (win (window-list nil 'no-mini))
            (when-let* ((entry (assq (current-buffer)
                                     (window-prev-buffers win))))
              (setcar (cddr entry) (point-marker))))))))
  (ekg-org-view--highlight))

(define-obsolete-function-alias 'ekg-org-view-toggle-collapse
  #'ekg-notes-toggle-collapse "0.10.0")

(defun ekg-org-view-toggle-all ()
  "Fold all root tasks, or expand all tasks when any are folded."
  (interactive nil ekg-org-view-mode)
  (setq ekg-notes-display-states
        (unless (seq-some (lambda (entry) (eq (cdr entry) 'folded))
                          ekg-notes-display-states)
          (mapcar (lambda (note) (cons (ekg-note-id note) 'folded))
                  (ekg-org-view--fetch-notes))))
  (let ((id (ekg-org-view--note-at-point)))
    (ekg-notes--refresh-preserving-view #'ekg-org-view--refresh id)))

(defun ekg-org-view-cycle-state ()
  "Choose the TODO state of the task at point."
  (interactive nil ekg-org-view-mode)
  (let* ((note (ekg-org-view--require-task-at-point))
         (state (completing-read "State: " (ekg-org--todo-keywords) nil t
                                 nil nil (ekg-org-task-state note))))
    (ekg-org-change-state state)))

(defun ekg-org-view-toggle-done ()
  "Mark the current task done, or reopen it in its TODO keyword sequence."
  (interactive nil ekg-org-view-mode)
  (let* ((note (ekg-org-view--require-task-at-point))
         (current (ekg-org-task-state note))
         (sequence (seq-find
                    (lambda (sequence)
                      (member current (ekg-org--todo-keywords (list sequence))))
                    (ekg-org--todo-sequences))))
    (unless sequence
      (user-error "No TODO keyword sequence contains %s" current))
    (let* ((done (ekg-org--done-keywords (list sequence)))
           (state (if (member current done)
                      (car (seq-difference
                            (ekg-org--todo-keywords (list sequence)) done))
                    (car done))))
      (unless state (user-error "No suitable TODO keyword is configured"))
      (ekg-org-change-state state))))

(defun ekg-org-view-add-child (title)
  "Create a child task of the task at point with TITLE."
  (interactive "sChild task: " ekg-org-view-mode)
  (when ekg-org-view--archive
    (user-error "Create tasks in the active task view"))
  (let ((note (ekg-org-view--require-task-at-point)))
    (unless (string-empty-p (string-trim title))
      (let ((child (ekg-note-create
                    :text "" :mode 'org-mode
                    :tags (append
                           (list ekg-org-task-tag
                                 (concat ekg-org-state-tag-prefix
                                         (downcase (ekg-org--initial-state))))
                           (ekg-org-view--visible-tags note))
                    :properties
                    (list :titled/title (list title)
                          :hierarchy/parent (ekg-note-id note)
                          :org/sort-order
                          (ekg-org--next-sort-order (ekg-note-id note)))))
            (ekg-org--inhibit-view-refresh t))
        (ekg-save-note child)
        (ekg-org-view--refresh (ekg-note-id child))))))

(defun ekg-org--initial-state ()
  "Return the first configured unfinished TODO keyword, or signal an error."
  (or (car (seq-difference (ekg-org--todo-keywords)
                           (ekg-org--done-keywords)))
      (user-error "No unfinished TODO keyword is configured")))

(defun ekg-org--next-sort-order (&optional parent-id exclude-id)
  "Return an append order for children of PARENT-ID, or for root tasks.
Use the highest stored order rather than the count: orders can have gaps.
Exclude EXCLUDE-ID when assigning an order to an existing unordered note."
  (let ((siblings (if parent-id
                      (ekg-org-view--sorted-children parent-id)
                    (ekg-org-view--sorted-top-level))))
    (1+ (apply #'max -1
               (mapcar #'ekg-org-view--sort-order
                       (seq-remove (lambda (note)
                                     (equal exclude-id (ekg-note-id note)))
                                   siblings))))))

(defun ekg-org--ensure-task-state (note)
  "Supply an initial state for task NOTE when no state is present."
  (unless (ekg-org-task-state note)
    (push (concat ekg-org-state-tag-prefix
                  (downcase (ekg-org--initial-state)))
          (ekg-note-tags note))))

(defun ekg-org--save-task-defaults (note)
  "Assign defaults using NOTE's final task tags, just before saving.
Preserve explicit order; ordinary response notes receive no Org properties."
  (when (ekg-org--org-note-p note)
    (ekg-org--ensure-task-state note)
    (unless (plist-member (ekg-note-properties note) :org/sort-order)
      (setf (ekg-note-properties note)
            (plist-put (ekg-note-properties note) :org/sort-order
                       (ekg-org--next-sort-order (ekg-note-parent-id note)
                                                 (ekg-note-id note)))))))

(add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)

(defun ekg-org--capture-task-defaults ()
  "Initialize task captures through `ekg-capture-mode-hook'.
Only notes tagged `org/task' receive an initial state.
Ordering is deferred until save, after the user has finished editing tags."
  (when (and ekg-capture-mode ekg-note
             (member ekg-org-task-tag (ekg-note-tags ekg-note)))
    (let* ((parent-id (ekg-note-parent-id ekg-note))
           (parent (and parent-id (ekg-get-note-with-id parent-id))))
      ;; Reject archived contexts before changing any defaults.
      (when (or (member ekg-org-archive-tag (ekg-note-tags ekg-note))
                (and parent
                     (member ekg-org-archive-tag (ekg-note-tags parent))))
        (user-error "Create tasks in the active task view"))
      (ekg-org--ensure-task-state ekg-note))))

(add-hook 'ekg-capture-mode-hook #'ekg-org--capture-task-defaults)

(defun ekg-org-view-set-dependency-type (dependency-type)
  "Set the dependency mode of the task's children.
DEPENDENCY-TYPE is either `sequential' or `parallel'.  Sequential
children must be completed in sibling order when interpreted by Org;
parallel children have no ordering dependency."
  (interactive
   (let* ((note (ekg-org-view--require-task-at-point))
          (current (ekg-org-dependency-type note)))
     (list
      (intern
       (completing-read "Child dependencies: "
                        (mapcar #'symbol-name ekg-org-dependency-types)
                        nil t nil nil (symbol-name current)))))
   ekg-org-view-mode)
  (ekg-org-view--require-task-at-point)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (let ((ekg-org--inhibit-view-refresh t))
      (ekg-org-set-dependency-type note dependency-type)
      (ekg-save-note note))
    (ekg-org-view--refresh id)
    (message "Child tasks are now %s" dependency-type)))

(defun ekg-org-view-set-property (property value &optional remove)
  "Set PROPERTY to VALUE on the task at point and save it.
With prefix argument REMOVE, delete PROPERTY instead.  Property names
are case-insensitive.  Prompt with directory completion for DIR."
  (interactive
   (let* ((note (ekg-org-view--require-task-at-point))
          (remove current-prefix-arg)
          (properties (mapcar #'car (ekg-org-properties-alist note)))
          (property
           (upcase (completing-read
                    (if remove "Remove property: " "Property: ")
                    (if remove properties
                      (delete-dups (append '("DIR" "ORDERED") properties)))
                    nil (and remove t))))
          (current (ekg-org-get-property note property)))
     (list property
           (unless remove
             (if (string= property "DIR")
                 (read-directory-name "Directory: " nil current t)
               (read-string (format "%s value: " property) current)))
           remove))
   ekg-org-view-mode)
  (unless (and (string-match-p "\\`[^[:space:]:]+\\'" property)
               (not (string-equal (upcase property) "EKG_ID")))
    (user-error "Invalid or reserved task property: %s" property))
  (let* ((note (ekg-org-view--require-task-at-point))
         (id (ekg-note-id note))
         (ekg-org--inhibit-view-refresh t))
    (if remove
        (ekg-org-remove-property note property)
      (ekg-org-set-property note property value)
      (when (string-equal (upcase property) "DIR")
        (ekg-org-set-property note property (ekg-org-working-directory note))))
    (ekg-save-note note)
    (ekg-org-view--refresh id)
    (message "%s property %s" (if remove "Removed" "Set") (upcase property))))

(defun ekg-org-view--archive-note (note)
  "Archive NOTE by adding the archive tag if not already present."
  (unless (member ekg-org-archive-tag (ekg-note-tags note))
    (setf (ekg-note-tags note)
          (cons ekg-org-archive-tag (ekg-note-tags note)))
    (ekg-org-view--save-tags note))
  (dolist (child (ekg-org-get-child-notes-of-id (ekg-note-id note)))
    (ekg-org-view--archive-note child)))

(defun ekg-org-view-archive ()
  "Archive the task at point and all its descendants."
  (interactive nil ekg-org-view-mode)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (when (y-or-n-p (format "Archive \"%s\" and all children? "
                            (or (ekg-org--note-title note) "Untitled")))
      (let ((ekg-org--inhibit-view-refresh t))
        (ekg-org-view--archive-note note))
      (ekg-org-view--refresh))))

(defun ekg-org-view--trash-note (note)
  "Trash NOTE and all its descendants."
  (ekg-note-trash note))

(defun ekg-org-view-delete ()
  "Trash the task at point and all its descendants.
Notes are moved to trash, which hides them from view.  If already
trashed, they are permanently deleted."
  (interactive nil ekg-org-view-mode)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (when (y-or-n-p (format "Delete \"%s\" and all children? "
                            (or (ekg-org--note-title note) "Untitled")))
      (let ((ekg-org--inhibit-view-refresh t))
        (ekg-org-view--trash-note note))
      (ekg-org-view--refresh))))

(defun ekg-org-view-open-note ()
  "Open the ekg note at point for editing."
  (interactive nil ekg-org-view-mode)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (ekg-edit note)))

(defun ekg-org-view-respond ()
  "Capture a Markdown response to the note at point."
  (interactive nil ekg-org-view-mode)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (ekg-respond-to-note note)))

(defun ekg-org-view-promote ()
  "Promote the task at point, making it a sibling of its current parent.
The promoted task is placed immediately after its former parent
among the new siblings."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--require-task-at-point)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id))
              (props (ekg-note-properties note))
              (parent-id (ekg-note-parent-id note)))
    (let* ((parent-note (ekg-get-note-with-id parent-id))
           (grandparent-id (when parent-note
                             (ekg-note-parent-id parent-note)))
           (ekg-org--inhibit-view-refresh t))
      (triples-with-transaction
        ekg-db
        (if grandparent-id
            (ekg-note-set-parent note grandparent-id)
          ;; Promoting to top-level: remove the parent property and the
          ;; hierarchy/parent triple.
          (ekg-note-set-parent note nil))
        ;; Place right after the former parent among new siblings.
        (let* ((new-siblings (if grandparent-id
                                 (ekg-org-view--sorted-children grandparent-id)
                               (ekg-org-view--sorted-top-level)))
               (sort-order (ekg-org-view--assign-order-after
                            new-siblings parent-id)))
          (ekg-org-view--set-sort-order id sort-order))
        (ekg-save-note note)))
    (ekg-org-view--refresh id)))

(defun ekg-org-view--descendant-ids (id)
  "Return a list of all descendant note IDs of ID."
  (let ((result nil))
    (dolist (child (ekg-org-get-child-notes-of-id id))
      (let ((child-id (ekg-note-id child)))
        (push child-id result)
        (setq result (nconc result (ekg-org-view--descendant-ids child-id)))))
    result))

(defun ekg-org-view--all-tasks-for-refile (exclude-id)
  "Return an alist of (DISPLAY-STRING . ID) for refile targets.
EXCLUDE-ID and its descendants are excluded to prevent cycles.
A \"Top level\" entry is included to allow refiling to the root.
Each display string shows the full hierarchy path, e.g.
\"Parent/Child/Target\", so the list remains meaningful even when a
completion framework reorders candidates."
  (let ((exclude-set (cons exclude-id
                           (ekg-org-view--descendant-ids exclude-id)))
        (result (list (cons "Top level" nil))))
    (letrec
        ((collect
          (lambda (notes path)
            (dolist (note notes)
              (let ((id (ekg-note-id note)))
                (unless (member id exclude-set)
                  (let* ((title (or (ekg-org--note-title note) "Untitled"))
                         (display (if path
                                      (concat path "/" title)
                                    title)))
                    (push (cons display id) result)
                    (funcall collect
                             (ekg-org-view--sorted-children id)
                             display))))))))
      (funcall collect (ekg-org-view--sorted-top-level) nil))
    (nreverse result)))

(defun ekg-org-view-refile ()
  "Refile the task at point to a new location in the hierarchy.
Prompts for a target task; the refiled task becomes the last child
of the target.  Selecting \"Top level\" makes it a top-level task."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--require-task-at-point)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (let* ((choices (ekg-org-view--all-tasks-for-refile id))
           (selection (completing-read
                       (format "Refile \"%s\" to: "
                               (or (ekg-org--note-title note) "Untitled"))
                       choices nil t))
           (target-id (cdr (assoc selection choices)))
           (ekg-org--inhibit-view-refresh t))
      (triples-with-transaction ekg-db
                                (if target-id
                                    (ekg-note-set-parent note target-id)
                                  ;; Moving to top level: remove parent.
                                  (ekg-note-set-parent note nil))
                                ;; Place at the end of the new siblings.
                                (let* ((new-siblings (if target-id
                                                         (ekg-org-view--sorted-children target-id)
                                                       (ekg-org-view--sorted-top-level)))
                                       ;; Exclude self from siblings when computing order.
                                       (new-siblings (seq-remove
                                                      (lambda (s) (equal (ekg-note-id s) id))
                                                      new-siblings))
                                       (last-id (when new-siblings
                                                  (ekg-note-id (car (last new-siblings)))))
                                       (sort-order (if new-siblings
                                                       (ekg-org-view--assign-order-after
                                                        new-siblings last-id)
                                                     0)))
                                  (ekg-org-view--set-sort-order id sort-order))
                                (ekg-save-note note)))
    (ekg-org-view--refresh id)))

(defun ekg-org-view-move-up ()
  "Move the task at point up, swapping it with its previous sibling."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--require-task-at-point)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (let* ((parent-id (ekg-note-parent-id note))
           (siblings (if parent-id
                         (ekg-org-view--sorted-children parent-id)
                       (ekg-org-view--sorted-top-level)))
           (pos (cl-position id siblings
                             :test #'equal
                             :key #'ekg-note-id)))
      (when (and pos (> pos 0))
        (let* ((prev (nth (1- pos) siblings))
               (prev-id (ekg-note-id prev))
               (order-cur (ekg-org-view--sort-order note))
               (order-prev (ekg-org-view--sort-order prev))
               (ekg-org--inhibit-view-refresh t))
          (triples-with-transaction
            ekg-db
            (ekg-org-view--set-sort-order id order-prev)
            (ekg-org-view--set-sort-order prev-id order-cur))
          (ekg-org-view--refresh id))))))

(defun ekg-org-view-move-down ()
  "Move the task at point down, swapping it with its next sibling."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--require-task-at-point)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (let* ((parent-id (ekg-note-parent-id note))
           (siblings (if parent-id
                         (ekg-org-view--sorted-children parent-id)
                       (ekg-org-view--sorted-top-level)))
           (pos (cl-position id siblings
                             :test #'equal
                             :key #'ekg-note-id)))
      (when (and pos (< pos (1- (length siblings))))
        (let* ((next (nth (1+ pos) siblings))
               (next-id (ekg-note-id next))
               (order-cur (ekg-org-view--sort-order note))
               (order-next (ekg-org-view--sort-order next))
               (ekg-org--inhibit-view-refresh t))
          (triples-with-transaction
            ekg-db
            (ekg-org-view--set-sort-order id order-next)
            (ekg-org-view--set-sort-order next-id order-cur))
          (ekg-org-view--refresh id))))))

(defun ekg-org-view-demote ()
  "Demote the task at point, making it a child of the previous sibling."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--require-task-at-point)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id))
              (props (ekg-note-properties note)))
    (let* ((parent-id (ekg-note-parent-id note))
           (siblings (if parent-id
                         (ekg-org-view--sorted-children parent-id)
                       (ekg-org-view--sorted-top-level)))
           (prev-sibling (let ((prev nil))
                           (cl-dolist (sib siblings)
                             (when (equal (ekg-note-id sib) id)
                               (cl-return prev))
                             (setq prev sib)))))
      (when prev-sibling
        (let ((ekg-org--inhibit-view-refresh t))
          (ekg-note-set-parent note (ekg-note-id prev-sibling))
          (ekg-save-note note))
        (ekg-org-view--refresh id)))))

(defun ekg-org--set-date-property (property prompt)
  "Set PROPERTY to a date chosen by the user with PROMPT.
Works in `ekg-org-view-mode' (operates on note at point),
`ekg-capture-mode', and `ekg-edit-mode' (operates on `ekg-note')."
  (let* ((in-view (derived-mode-p 'ekg-org-view-mode))
         (_ (when in-view (ekg-org-view--require-task-at-point)))
         (id (if in-view (ekg-org-view--note-at-point) (ekg-note-id ekg-note)))
         (note (if in-view (ekg-get-note-with-id id) ekg-note)))
    (when note
      (let* ((current (plist-get (ekg-note-properties note) property))
             (current-time (when current (seconds-to-time current)))
             (new-ts (org-read-date nil t nil prompt current-time))
             (ekg-org--inhibit-view-refresh t))
        (setf (ekg-note-properties note)
              (plist-put (ekg-note-properties note)
                         property
                         (time-convert new-ts 'integer)))
        (ekg-save-note note))
      (when in-view
        (ekg-org-view--refresh id)))))

(defun ekg-org-set-schedule ()
  "Set the scheduled date of the current task.
Prompts with `org-read-date' and stores the result as a Unix
timestamp in the note's `:org/scheduled' property."
  (interactive nil ekg-org-view-mode ekg-capture-mode ekg-edit-mode)
  (ekg-org--set-date-property :org/scheduled "Schedule: "))

(defun ekg-org-set-deadline ()
  "Set the deadline of the current task.
Prompts with `org-read-date' and stores the result as a Unix
timestamp in the note's `:org/deadline' property."
  (interactive nil ekg-org-view-mode ekg-capture-mode ekg-edit-mode)
  (ekg-org--set-date-property :org/deadline "Deadline: "))

;; Major mode

(defun ekg-org-view--refresh-all (&rest _args)
  "Refresh all live `ekg-org-view-mode' buffers.
During inhibited batches, let VUI coalesce updates to each mounted
view.  Other saves refresh synchronously.  Accept and ignore arguments
so this can run on `ekg-note-save-hook' and `ekg-note-delete-hook'."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (with-current-buffer buf
                 (derived-mode-p 'ekg-org-view-mode)))
      (with-current-buffer buf
        (cond
         ((and ekg-org--inhibit-view-refresh ekg-notes--request-refresh)
          ;; Even when the user disables VUI's delay, wait until the
          ;; current batch has finished before showing its intermediate state.
          (setq ekg-notes--hidden-ids nil)
          (let ((vui-render-delay (or vui-render-delay 0)))
            (funcall ekg-notes--request-refresh)))
         (t (ekg-org-view--refresh)))))))

(define-derived-mode ekg-org-view-mode ekg-notes-mode "EKG-Org"
  "Show a task query using the normal notes UI and keybindings."
  (setq-local ekg-notes-children-function #'ekg-org-view--children
              ekg-notes-include-descendants t
              ekg-notes-show-child-count nil
              ekg-display-note-template
              (concat "%n(titled)%n(org-status)%n(org-tags nil separated)"
                      "%n(id)\n%n(text 500)")
              ekg-display-note-summary-template
              (concat "%n(org-status nil state-only oneline)"
                      "%n(titled 12 text-fallback oneline)"))
  (add-hook 'ekg-note-save-hook #'ekg-org-view--refresh-all)
  (add-hook 'ekg-note-delete-hook #'ekg-org-view--refresh-all)
  (add-hook 'kill-buffer-hook
            (lambda ()
              ;; Remove the hooks when no view buffers remain.
              (unless (cl-some (lambda (buf)
                                 (and (not (eq buf (current-buffer)))
                                      (buffer-live-p buf)
                                      (with-current-buffer buf
                                        (derived-mode-p 'ekg-org-view-mode))))
                               (buffer-list))
                (remove-hook 'ekg-note-save-hook #'ekg-org-view--refresh-all)
                (remove-hook 'ekg-note-delete-hook #'ekg-org-view--refresh-all)))
            nil t))

(defun ekg-org-view-refresh ()
  "Refresh the task view."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--refresh))

(defun ekg-org-view--mount (&optional archive root-id)
  "Show a normal notes view of Org tasks.
ARCHIVE selects archived tasks.  ROOT-ID focuses on a single task."
  (ekg-connect)
  (let* ((buf-name (cond (root-id (format "*ekg-org-task-%s*" root-id))
                         (archive "*ekg-org-archive*")
                         (t "*ekg-org-tasks*")))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ekg-org-view-mode)
        (ekg-org-view-mode))
      (setq-local ekg-org-view--root-id root-id
                  ekg-org-view--archive archive)
      (ekg-org-view--refresh))
    (switch-to-buffer buf)
    (buffer-local-value 'ekg-notes--instance buf)))

;;;###autoload
(defun ekg-org-view ()
  "Show all ekg org tasks in a hierarchical view."
  (interactive)
  (ekg-org-view--mount))

;;;###autoload
(defun ekg-org-archive-view ()
  "Show archived ekg org tasks in a hierarchical view."
  (interactive)
  (ekg-org-view--mount t))

;;;###autoload
(defun ekg-org-view-task (id)
  "Show the task with ID and its children in a hierarchical view."
  (interactive
   (list (let* ((tasks (ekg-org-get-tasks))
                (choices (mapcar
                          (lambda (n)
                            (cons (or (ekg-org--note-title n) "Untitled")
                                  (ekg-note-id n)))
                          tasks))
                (title (completing-read "Task: " choices nil t)))
           (cdr (assoc title choices)))))
  (ekg-org-view--mount nil id))

(defun ekg-org--format-epoch-as-date (epoch)
  "Format EPOCH seconds as a readable date string."
  (format-time-string "%Y-%m-%d %a" (seconds-to-time epoch)))

(defun ekg-org-initialize ()
  "Initialize the ekg-org integration.

This adds the necessary schema and, if `ekg-agent' is available,
registers tools for interacting with org tasks."
  (ekg-connect)
  (ekg-org-add-schema)
  (setf (alist-get :org/scheduled ekg-property-format-functions)
        #'ekg-org--format-epoch-as-date)
  (setf (alist-get :org/deadline ekg-property-format-functions)
        #'ekg-org--format-epoch-as-date)
  (define-key ekg-capture-mode-map "\C-c\C-s" #'ekg-org-set-schedule)
  (define-key ekg-capture-mode-map "\C-c\C-d" #'ekg-org-set-deadline)
  (define-key ekg-edit-mode-map "\C-c\C-s" #'ekg-org-set-schedule)
  (define-key ekg-edit-mode-map "\C-c\C-d" #'ekg-org-set-deadline)
  (when (featurep 'ekg-agent)
    (add-to-list 'ekg-agent-extra-tools ekg-agent-org-tool-add-task)
    (add-to-list 'ekg-agent-extra-tools ekg-agent-org-tool-create-plan)
    (add-to-list 'ekg-agent-extra-tools ekg-agent-org-tool-set-status)
    (add-to-list 'ekg-agent-extra-tools ekg-agent-org-tool-list-items)))

(provide 'ekg-org)
;;; ekg-org.el ends here
