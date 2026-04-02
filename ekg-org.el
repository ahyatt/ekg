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
(require 'vui)
(declare-function vui-rerender "vui")

;; Forward declarations for variables defined in ekg-agent.el.
(defvar ekg-agent-extra-tools)
(defvar ekg-agent-org-tool-add-task)
(defvar ekg-agent-org-tool-set-status)
(defvar ekg-agent-org-tool-list-items)

(defvar ekg-org--inhibit-view-refresh nil
  "When non-nil, suppress `ekg-org-view' refreshes from save hooks.
Bind this around batch operations that save multiple notes; call
`ekg-org-view--refresh' once afterward.")

(defconst ekg-org-state-tag-prefix "org/state/"
  "Prefix for EKG tags representing Org TODO states.")

(defconst ekg-org-task-tag "org/task"
  "Tag used to identify EKG notes that should be treated as Org tasks.")

(defconst ekg-org-archive-tag "org/archive"
  "Tag used to identify EKG notes that should be treated as archived Org tasks.")

(defface ekg-org-view-body
  '((t :inherit shadow))
  "Face used for task body text in `ekg-org-view-mode'."
  :group 'ekg)

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
                      '(children :base/virtual-reversed org/parent)
                      '(sort-order :base/type integer :base/unique t)
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
(add-to-list 'ekg-header-hidden-properties :org/parent)
(add-to-list 'ekg-header-hidden-properties :org/children)
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

(defun ekg-org-get-tasks (&optional archive)
  "Fetch top-level tasks from ekg, as ekg-note structs.

If ARCHIVE is non-nil, fetch archived tasks instead.  If nil, fetch
active, unarchived, tasks."
  (ekg-connect)
  (let* ((all-ids (plist-get (triples-get-type ekg-db ekg-org-task-tag 'tag)
                             :tagged))
         ;; Pre-filter: exclude IDs that have a parent, since we only
         ;; want top-level tasks.  This avoids loading child notes from
         ;; the database just to discard them.
         (child-id-set (make-hash-table :test 'equal))
         (_ (dolist (row (triples-db-select ekg-db nil 'org/parent nil))
              (puthash (car row) t child-id-set)))
         (top-ids (seq-remove (lambda (id) (gethash id child-id-set))
                              all-ids)))
    (seq-filter
     (lambda (note)
       (and (ekg-note-active-p note)
            (ekg-note-is-content-p note)
            (let ((props (ekg-note-properties note)))
              (and
               (plist-get props :titled/title)
               (let ((tags (ekg-note-tags note)))
                 (if archive
                     (member ekg-org-archive-tag tags)
                   (not (member ekg-org-archive-tag tags))))))))
     (delq nil (mapcar #'ekg-get-note-with-id top-ids)))))

(defun ekg-org-get-child-notes-of-id (id)
  "Fetch child notes of a given note ID."
  (seq-filter #'ekg-note-active-p
              (delq nil (mapcar (lambda (row) (ekg-get-note-with-id (car row)))
                                (triples-db-select ekg-db nil 'org/parent id)))))

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
  (interactive (list (completing-read "New state: " (ekg-org--todo-keywords))))
  (let ((ekg-note (ekg-current-note-or-error-expanded)))
    (setf (ekg-note-tags ekg-note)
          (cons
           (concat ekg-org-state-tag-prefix (downcase new-state))
           (seq-remove
            (lambda (tag) (string-prefix-p ekg-org-state-tag-prefix tag))
            (ekg-note-tags ekg-note))))
    ;; We save unless we're currently editing the note.  Use the
    ;; lightweight tag-only save since only the state tag changed.
    (unless (member 'ekg-edit-mode local-minor-modes)
      (ekg-org-view--save-tags ekg-note)
      (when (derived-mode-p 'ekg-notes-mode)
        (ekg-notes-refresh)))))
;; ekg-org-view-mode buffers are refreshed via ekg-note-save-hook.

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
            (let ((temp-buf (generate-new-buffer " *ekg-temp*")))
              (unwind-protect
                  (progn
                    (with-current-buffer temp-buf
                      (insert content))
                    (replace-buffer-contents temp-buf))
                (kill-buffer temp-buf)))
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

(defun ekg-org--todo-keywords ()
  "Return a flat list of TODO keyword strings from `org-todo-keywords'.
This reads directly from the global defcustom rather than the
buffer-local `org-todo-keywords-1', which is only populated inside
Org-mode buffers."
  (let (result)
    (dolist (entry (default-value 'org-todo-keywords))
      (if (stringp entry)
          ;; Old-style plain keyword list.
          (push entry result)
        ;; New-style (sequence/type "KW1" "|" "KW2" ...) entry.
        (dolist (kw (cdr entry))
          (unless (equal kw "|")
            ;; Strip fast-selection key / logging specs, e.g. "WAIT(w@/!)".
            (when (string-match "^\\([^(]+\\)" kw)
              (push (match-string 1 kw) result))))))
    (nreverse result)))

(defun ekg-org--state (note)
  "Get the org state of NOTE."
  (let ((tags (ekg-note-tags note)))
    (let ((tag (car (seq-filter
                     (lambda (tag)
                       (string-prefix-p ekg-org-state-tag-prefix tag))
                     tags))))
      (if tag
          (upcase (string-replace ekg-org-state-tag-prefix "" tag))
        (error "No org state tag found for note ID %s" (ekg-note-id note))))))

;;; ekg-org-view — vui-based hierarchical task view

(defvar-local ekg-org-view--hl nil
  "Overlay for highlighting the current heading in `ekg-org-view-mode'.")

(defvar-local ekg-org-view--refresh-pending nil
  "When non-nil, a refresh was deferred because insert mode was active.")

(defvar-local ekg-org-view--instance nil
  "The vui root instance for the current ekg-org-view buffer.")

(defvar-local ekg-org-view--root-id nil
  "When non-nil, show only this task and its children.")

(defvar-local ekg-org-view--archive nil
  "When non-nil, show archived tasks instead of active ones.")

(defvar-local ekg-org-view--insert-slots nil
  "List of available insertion slots during insert mode.")

(defvar-local ekg-org-view--insert-index nil
  "Current slot index during insert mode.")

(defvar-local ekg-org-view--insert-overlay nil
  "Overlay showing the insertion placeholder.")

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

(defun ekg-org-view--sort-predicate (a b)
  "Return non-nil if note A should sort before note B.
Uses sort-order as primary key and note ID as tiebreaker for
stability."
  (let ((oa (ekg-org-view--sort-order a))
        (ob (ekg-org-view--sort-order b)))
    (or (< oa ob)
        (and (= oa ob)
             (< (ekg-note-id a) (ekg-note-id b))))))

(defun ekg-org-view--sorted-children (id)
  "Return non-archived children of ID, sorted by sort-order."
  (sort (seq-remove
         (lambda (child)
           (member ekg-org-archive-tag (ekg-note-tags child)))
         (ekg-org-get-child-notes-of-id id))
        #'ekg-org-view--sort-predicate))

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

(defun ekg-org-view--visible-tags (note)
  "Return the user-visible tags of NOTE, excluding internal org tags."
  (seq-filter (lambda (tag)
                (and (not (string-prefix-p ekg-org-state-tag-prefix tag))
                     (not (string-equal tag ekg-org-task-tag))
                     (not (string-equal tag ekg-org-archive-tag))))
              (ekg-note-tags note)))

(defun ekg-org-view--indent-text (text prefix)
  "Indent each line of TEXT with PREFIX."
  (mapconcat (lambda (line) (concat prefix line))
             (split-string text "\n")
             "\n"))

(defun ekg-org-view--fontify-org (text)
  "Return TEXT with `org-mode' font-lock properties applied.
Reuses a hidden buffer to avoid repeated `org-mode' initialization."
  (if (or (null text) (string-empty-p text))
      ""
    (with-current-buffer (get-buffer-create " *ekg-org-fontify*")
      (unless (derived-mode-p 'org-mode)
        (delay-mode-hooks (org-mode)))
      (let ((inhibit-modification-hooks t))
        (erase-buffer)
        (insert text)
        (font-lock-ensure)
        (buffer-string)))))

(defun ekg-org-view--heading-face (level)
  "Return the org heading face for LEVEL."
  (intern (format "org-level-%d" (min level 8))))

(defun ekg-org-view--render-heading (note level)
  "Return a propertized heading string for NOTE at LEVEL."
  (let* ((state (condition-case nil (ekg-org--state note) (error "UNKNOWN")))
         (title (or (ekg-org--note-title note) "Untitled"))
         (tags (ekg-org-view--visible-tags note))
         (stars (make-string level ?*))
         (state-face (if (string-equal state "DONE") 'org-done 'org-todo))
         (tag-str (if tags (concat " :" (mapconcat #'identity tags ":") ":") "")))
    (concat (propertize stars 'face (ekg-org-view--heading-face level))
            " "
            (propertize state 'face state-face)
            " "
            (propertize title 'face (ekg-org-view--heading-face level))
            (propertize tag-str 'face 'org-tag))))



(defun ekg-org-view--render-task (note level collapsed-ids)
  "Return a vui vnode tree for NOTE at LEVEL with COLLAPSED-IDS."
  (let* ((id (ekg-note-id note))
         (collapsed (member id collapsed-ids))
         (heading (ekg-org-view--render-heading note level))
         (children (ekg-org-view--sorted-children id))
         (text (ekg-note-text note))
         (body-nodes nil))
    (unless collapsed
      (when (and text (not (string-empty-p (string-trim text))))
        (let* ((fontified (string-trim-right (ekg-org-view--fontify-org text)))
               (indent (make-string (1+ level) ?\s))
               (body (ekg-org-view--indent-text fontified indent)))
          (push (vui-text body
                  :key (intern (format "body-%s" id))
                  :ekg-org-note-id id
                  :ekg-org-level level)
                body-nodes)))
      (dolist (child children)
        (push (ekg-org-view--render-task child (1+ level) collapsed-ids)
              body-nodes))
      (setq body-nodes (nreverse body-nodes)))
    (apply #'vui-vstack
           :key (intern (format "task-%s" id))
           (vui-text heading
             :key (intern (format "heading-%s" id))
             :ekg-org-note-id id
             :ekg-org-level level
             :ekg-org-heading t)
           body-nodes)))

(vui-defcomponent ekg-org-view-root (root-id archive)
  "Root component for the ekg org task view."
  :state ((collapsed-ids nil))
  :render
  (let* ((tasks (if root-id
                    (let ((note (ekg-get-note-with-id root-id)))
                      (when note (list note)))
                  (ekg-org-view--sorted-top-level archive)))
         (start-level (if root-id 1 1)))
    (if (null tasks)
        (vui-text (if archive
                      "(No archived org tasks found)"
                    "(No org tasks found)")
          :face 'font-lock-comment-face)
      (apply #'vui-vstack
             :spacing 1
             (mapcar (lambda (task)
                       (ekg-org-view--render-task task start-level collapsed-ids))
                     tasks)))))

;; Navigation helpers

(defun ekg-org-view--note-at-point ()
  "Return the note ID at point, or nil."
  (or (get-text-property (line-beginning-position) :ekg-org-note-id)
      (save-excursion
        (let ((found nil))
          (while (and (not found) (not (bobp)))
            (forward-line -1)
            (setq found (get-text-property (line-beginning-position) :ekg-org-note-id)))
          found))))

(defun ekg-org-view--level-at-point ()
  "Return the heading level at point, or nil."
  (get-text-property (line-beginning-position) :ekg-org-level))

(defun ekg-org-view--on-heading-p ()
  "Return non-nil if point is on a heading line."
  (get-text-property (line-beginning-position) :ekg-org-heading))

(defun ekg-org-view--ensure-on-heading ()
  "Move point to the nearest heading line if not already on one.
Tries forward first, then backward.  Does nothing if the buffer
has no headings."
  (unless (ekg-org-view--on-heading-p)
    (let ((fwd nil) (bwd nil))
      (save-excursion
        (while (and (not (eobp)) (not (ekg-org-view--on-heading-p)))
          (forward-line 1))
        (when (ekg-org-view--on-heading-p)
          (setq fwd (point))))
      (save-excursion
        (while (and (not (bobp)) (not (ekg-org-view--on-heading-p)))
          (forward-line -1))
        (when (ekg-org-view--on-heading-p)
          (setq bwd (point))))
      (cond
       ((and fwd bwd)
        (goto-char (if (<= (- fwd (point)) (- (point) bwd)) fwd bwd)))
       (fwd (goto-char fwd))
       (bwd (goto-char bwd))))))

(defun ekg-org-view--highlight ()
  "Highlight the current heading in the ekg-org-view buffer."
  (when ekg-org-view--hl
    (let* ((current-id (ekg-org-view--note-at-point))
           (beg (line-beginning-position))
           (end (save-excursion
                  (forward-line 1)
                  (while (and (not (eobp))
                              (let ((id (get-text-property (point) :ekg-org-note-id)))
                                (or (equal id current-id)
                                    (not (get-text-property (point) :ekg-org-heading)))))
                    (forward-line 1))
                  (if (eobp) (point) (line-beginning-position)))))
      (move-overlay ekg-org-view--hl beg end))))

(defun ekg-org-view--goto-heading (direction &optional same-level)
  "Move to the next heading in DIRECTION (:forward or :backward).
If SAME-LEVEL, only stop at headings with the same level as current.
Continuation lines (SCHEDULED/DEADLINE) sharing the same note ID
as the starting heading are skipped."
  (let ((current-level (ekg-org-view--level-at-point))
        (current-id (ekg-org-view--note-at-point))
        (step (if (eq direction :forward) 1 -1))
        (found nil))
    (save-excursion
      (forward-line step)
      (while (not found)
        (let ((id (get-text-property (point) :ekg-org-note-id)))
          (when (and (get-text-property (point) :ekg-org-heading)
                     (not (equal id current-id)))
            (if same-level
                (when (equal (get-text-property (point) :ekg-org-level)
                             current-level)
                  (setq found (point)))
              (setq found (point)))))
        (when (or found (if (= step 1) (eobp) (bobp)))
          (setq found (or found 'stop)))
        (unless found (forward-line step))))
    (when (integerp found) (goto-char found))))

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
          (let ((id (get-text-property (point) :ekg-org-note-id)))
            (when (and (get-text-property (point) :ekg-org-heading)
                       (not (equal id current-id)))
              (let ((level (get-text-property (point) :ekg-org-level)))
                (when (and level (< level current-level))
                  (setq found (point)))))))))
    (when found (goto-char found))
    (ekg-org-view--highlight)))

;; Interactive action commands

(defun ekg-org-view--goto-note-id (id)
  "Move point to the heading for note ID, if present in the buffer.
Returns non-nil if found."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (when (and (equal (get-text-property (point) :ekg-org-note-id) id)
                   (get-text-property (point) :ekg-org-heading))
          (setq found (point)))
        (forward-line 1)))
    (when found
      (goto-char found)
      t)))

(defun ekg-org-view--refresh (&optional target-id)
  "Re-render the view in place without switching windows.
If TARGET-ID is non-nil, move point to that note's heading after
re-rendering and ensure it is visible.  When insert mode is active,
the refresh is deferred until insert mode ends."
  (if ekg-org-view--insert-overlay
      (setq ekg-org-view--refresh-pending t)
    (vui-rerender ekg-org-view--instance)
    (when (and target-id (ekg-org-view--goto-note-id target-id))
      (let ((windows (get-buffer-window-list (current-buffer) nil t)))
        (dolist (win windows)
          (set-window-point win (point))
          (unless (pos-visible-in-window-p (point) win)
            (with-selected-window win (recenter))))
        ;; When the buffer is not visible, update the saved point in
        ;; each window's prev-buffer history so that `quit-window'
        ;; restores point to the right heading.
        (unless windows
          (dolist (win (window-list nil 'no-mini))
            (when-let* ((entry (assq (current-buffer)
                                     (window-prev-buffers win))))
              (setcar (cddr entry) (point-marker)))))))
    (ekg-org-view--highlight)))

(defun ekg-org-view-toggle-collapse ()
  "Toggle collapse/expand of the task at point."
  (interactive nil ekg-org-view-mode)
  (when-let* ((id (ekg-org-view--note-at-point))
              (instance ekg-org-view--instance))
    (let* ((state (vui-instance-state instance))
           (collapsed (plist-get state :collapsed-ids)))
      (if (member id collapsed)
          (setf (vui-instance-state instance)
                (plist-put state :collapsed-ids (remove id collapsed)))
        (setf (vui-instance-state instance)
              (plist-put state :collapsed-ids (cons id collapsed)))))
    (vui-rerender ekg-org-view--instance)))

(defun ekg-org-view-cycle-state ()
  "Change the TODO state of the task at point."
  (interactive nil ekg-org-view-mode)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (let* ((states (or (ekg-org--todo-keywords) '("TODO" "DONE")))
           (new-state (completing-read "State: " states nil t))
           (ekg-org--inhibit-view-refresh t))
      (setf (ekg-note-tags note)
            (cons (concat ekg-org-state-tag-prefix (downcase new-state))
                  (seq-remove
                   (lambda (tag) (string-prefix-p ekg-org-state-tag-prefix tag))
                   (ekg-note-tags note))))
      (ekg-org-view--save-tags note))
    (ekg-org-view--refresh)))

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
  (dolist (child (ekg-org-get-child-notes-of-id (ekg-note-id note)))
    (ekg-org-view--trash-note child))
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

(defun ekg-org-view--plist-delete (plist key)
  "Return a copy of PLIST with KEY and its value removed."
  (let ((result nil))
    (while plist
      (unless (eq (car plist) key)
        (setq result (cons (cadr plist) (cons (car plist) result))))
      (setq plist (cddr plist)))
    (nreverse result)))

(defun ekg-org-view-promote ()
  "Promote the task at point, making it a sibling of its current parent.
The promoted task is placed immediately after its former parent
among the new siblings."
  (interactive nil ekg-org-view-mode)
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id))
              (props (ekg-note-properties note))
              (parent-id (plist-get props :org/parent)))
    (let* ((parent-note (ekg-get-note-with-id parent-id))
           (grandparent-id (when parent-note
                             (plist-get (ekg-note-properties parent-note)
                                        :org/parent)))
           (ekg-org--inhibit-view-refresh t))
      (triples-with-transaction
        ekg-db
        (if grandparent-id
            (setf (ekg-note-properties note)
                  (plist-put props :org/parent grandparent-id))
          ;; Promoting to top-level: remove the parent property and the
          ;; org/parent triple directly.
          (setf (ekg-note-properties note)
                (ekg-org-view--plist-delete props :org/parent))
          (triples-db-delete ekg-db id 'org/parent))
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
                                    (setf (ekg-note-properties note)
                                          (plist-put (ekg-note-properties note)
                                                     :org/parent target-id))
                                  ;; Moving to top level: remove parent.
                                  (setf (ekg-note-properties note)
                                        (ekg-org-view--plist-delete (ekg-note-properties note)
                                                                    :org/parent))
                                  (triples-db-delete ekg-db id 'org/parent))
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
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (let* ((parent-id (plist-get (ekg-note-properties note) :org/parent))
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
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id)))
    (let* ((parent-id (plist-get (ekg-note-properties note) :org/parent))
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
  (when-let* ((id (ekg-org-view--note-at-point))
              (note (ekg-get-note-with-id id))
              (props (ekg-note-properties note)))
    (let* ((parent-id (plist-get props :org/parent))
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
          (setf (ekg-note-properties note)
                (plist-put props :org/parent (ekg-note-id prev-sibling)))
          (ekg-save-note note))
        (ekg-org-view--refresh)))))

;;; Task insertion mode

(defun ekg-org-view--collect-headings ()
  "Return a list of (BUFFER-POS ID LEVEL PARENT-ID) for every heading.
Only the first line of each heading is collected; continuation
lines (e.g. SCHEDULED/DEADLINE) sharing the same note ID are
skipped."
  (let ((headings nil)
        (seen-ids nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) :ekg-org-heading)
          (let ((id (get-text-property (point) :ekg-org-note-id))
                (level (get-text-property (point) :ekg-org-level)))
            (unless (member id seen-ids)
              (push id seen-ids)
              (push (list (point) id level
                          (when-let* ((note (and id (ekg-get-note-with-id id))))
                            (plist-get (ekg-note-properties note)
                                       :org/parent)))
                    headings))))
        (forward-line 1)))
    (nreverse headings)))

(defun ekg-org-view--build-insert-slots ()
  "Build a list of insertion slots from the current buffer headings.
Each slot is a plist with :buffer-pos, :level, :parent-id, :after-id.
:buffer-pos is where to display the placeholder (the line before
which the new task would visually appear).
:after-id is the sibling after which to insert for sort-order, or
nil to insert as first child/first top-level."
  (let* ((headings (ekg-org-view--collect-headings))
         (slots nil))
    ;; Slot before the first heading (top-level, first position).
    (when headings
      (push (list :buffer-pos (nth 0 (car headings))
                  :level 1 :parent-id nil :after-id nil)
            slots))
    (let ((len (length headings)))
      (dotimes (i len)
        (let* ((h (nth i headings))
               (h-id (nth 1 h))
               (h-level (nth 2 h))
               (next (when (< (1+ i) len) (nth (1+ i) headings)))
               (next-pos (when next (nth 0 next)))
               (next-level (when next (nth 2 next)))
               ;; Find where this heading's subtree ends: the next
               ;; heading at the same level or shallower.
               (tree-end-pos
                (or (cl-loop for j from (1+ i) below len
                             for candidate = (nth j headings)
                             when (<= (nth 2 candidate) h-level)
                             return (nth 0 candidate))
                    (point-max))))
          ;; Slot: sibling after this heading (same level, same parent).
          (push (list :buffer-pos tree-end-pos
                      :level h-level
                      :parent-id (nth 3 h)
                      :after-id h-id)
                slots)
          ;; Slot: first child of this heading (one level deeper).
          ;; When children exist, place before the first child.
          ;; Otherwise, place at the subtree end.
          (let ((child-pos (if (and next-level (> next-level h-level))
                               next-pos
                             tree-end-pos)))
            (push (list :buffer-pos child-pos
                        :level (1+ h-level)
                        :parent-id h-id
                        :after-id nil)
                  slots)))))
    ;; Remove duplicate slots (same buffer-pos + level + parent-id).
    (let ((seen (make-hash-table :test #'equal))
          (result nil))
      (dolist (slot (nreverse slots))
        (let ((key (list (plist-get slot :buffer-pos)
                         (plist-get slot :level)
                         (plist-get slot :parent-id))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push slot result))))
      (nreverse result))))

(defface ekg-org-view-insert-placeholder
  '((t :inherit highlight :extend t))
  "Face for the task insertion placeholder line."
  :group 'ekg)

(defun ekg-org-view--insert-placeholder-string (level &optional title)
  "Return the placeholder string for an insertion slot at LEVEL.
If TITLE is non-nil and non-empty, show it; otherwise show hint text."
  (let* ((stars (make-string level ?*))
         (title-part (if (and title (not (string-empty-p title)))
                         title
                       (propertize "← type task title" 'face 'shadow)))
         (text (concat (propertize stars 'face (ekg-org-view--heading-face level))
                       " "
                       (propertize "TODO" 'face 'org-todo)
                       " "
                       title-part)))
    (propertize text 'face 'ekg-org-view-insert-placeholder)))

(defvar ekg-org-view-insert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'ekg-org-view-insert-next)
    (define-key map (kbd "p") #'ekg-org-view-insert-prev)
    (define-key map (kbd "R") #'ekg-org-view-insert-demote)
    (define-key map (kbd "L") #'ekg-org-view-insert-promote)
    (define-key map (kbd "RET") #'ekg-org-view-insert-confirm)
    (define-key map (kbd "C-g") #'ekg-org-view-insert-cancel)
    map)
  "Keymap active during task insertion mode.")

(defun ekg-org-view--insert-update-overlay (ov pos level spacing &optional title)
  "Update overlay OV at POS to show insertion placeholder at LEVEL.
SPACING is the number of blank lines between items at this level,
matching the vui-vstack spacing of the containing layout (1 for
top-level items, 0 for children).
If TITLE is given, display it in the placeholder."
  (let ((placeholder (ekg-org-view--insert-placeholder-string level title))
        (at-end (>= pos (point-max))))
    (move-overlay ov pos pos)
    ;; At end of buffer, we need a preceding newline since there's no
    ;; following line to attach before-string to.  Use after-string
    ;; instead.
    (if at-end
        (progn
          (overlay-put ov 'before-string nil)
          (overlay-put ov 'after-string
                       (concat "\n" (make-string spacing ?\n) placeholder)))
      (overlay-put ov 'after-string nil)
      (overlay-put ov 'before-string
                   (concat placeholder "\n" (make-string spacing ?\n))))))

(defun ekg-org-view--insert-slot-spacing (slot)
  "Return the inter-item spacing for SLOT's context.
Top-level items (no parent) use spacing 1 to match the outer
vui-vstack.  Child items use spacing 0."
  (if (plist-get slot :parent-id) 0 1))

(defun ekg-org-view--insert-show ()
  "Display the placeholder overlay at the current insertion slot."
  (when ekg-org-view--insert-overlay
    (let* ((slot (nth ekg-org-view--insert-index ekg-org-view--insert-slots))
           (level (plist-get slot :level))
           (pos (plist-get slot :buffer-pos))
           (spacing (ekg-org-view--insert-slot-spacing slot)))
      (ekg-org-view--insert-update-overlay
       ekg-org-view--insert-overlay pos level spacing)
      (goto-char (min pos (point-max))))))

(defun ekg-org-view--insert-find-nearest-index (pos &optional prefer-parent-id)
  "Return the slot index closest to buffer position POS.
When PREFER-PARENT-ID is non-nil, prefer the first-child slot of
that note (the slot with :parent-id = PREFER-PARENT-ID and
:after-id = nil) when it exists."
  (or (when prefer-parent-id
        (cl-loop for slot in ekg-org-view--insert-slots
                 for i from 0
                 when (and (equal (plist-get slot :parent-id) prefer-parent-id)
                           (null (plist-get slot :after-id)))
                 return i))
      ;; Fallback: nearest by buffer position.
      (let ((best 0)
            (best-dist most-positive-fixnum))
        (cl-loop for slot in ekg-org-view--insert-slots
                 for i from 0
                 do (let ((dist (abs (- (plist-get slot :buffer-pos) pos))))
                      (when (< dist best-dist)
                        (setq best i best-dist dist))))
        best)))

(defun ekg-org-view-insert-next ()
  "Move to the next insertion slot."
  (interactive nil ekg-org-view-mode)
  (when (< ekg-org-view--insert-index
           (1- (length ekg-org-view--insert-slots)))
    (cl-incf ekg-org-view--insert-index)
    (ekg-org-view--insert-show)))

(defun ekg-org-view-insert-prev ()
  "Move to the previous insertion slot."
  (interactive nil ekg-org-view-mode)
  (when (> ekg-org-view--insert-index 0)
    (cl-decf ekg-org-view--insert-index)
    (ekg-org-view--insert-show)))

(defun ekg-org-view-insert-demote ()
  "Make the insertion slot one level deeper (child of current parent)."
  (interactive nil ekg-org-view-mode)
  (let* ((slot (nth ekg-org-view--insert-index ekg-org-view--insert-slots))
         (parent-id (plist-get slot :parent-id))
         (after-id (plist-get slot :after-id))
         (level (plist-get slot :level))
         ;; Demote: the new parent is the after-id sibling (or if
         ;; after-id is nil, there's nothing to demote into).
         (new-parent-id (or after-id parent-id)))
    (when new-parent-id
      (let ((new-slot (list :buffer-pos (plist-get slot :buffer-pos)
                            :level (1+ level)
                            :parent-id new-parent-id
                            :after-id nil)))
        ;; Replace the current slot in-place.
        (setf (nth ekg-org-view--insert-index ekg-org-view--insert-slots)
              new-slot)
        (ekg-org-view--insert-show)))))

(defun ekg-org-view-insert-promote ()
  "Make the insertion slot one level shallower (sibling of current parent)."
  (interactive nil ekg-org-view-mode)
  (let* ((slot (nth ekg-org-view--insert-index ekg-org-view--insert-slots))
         (parent-id (plist-get slot :parent-id))
         (level (plist-get slot :level)))
    (when (and parent-id (> level 1))
      (let* ((parent-note (ekg-get-note-with-id parent-id))
             (grandparent-id (when parent-note
                               (plist-get (ekg-note-properties parent-note)
                                          :org/parent)))
             (new-slot (list :buffer-pos (plist-get slot :buffer-pos)
                             :level (1- level)
                             :parent-id grandparent-id
                             :after-id parent-id)))
        (setf (nth ekg-org-view--insert-index ekg-org-view--insert-slots)
              new-slot)
        (ekg-org-view--insert-show)))))

(defun ekg-org-view--insert-cleanup ()
  "Clean up insertion mode state."
  (when ekg-org-view--insert-overlay
    (delete-overlay ekg-org-view--insert-overlay)
    (setq ekg-org-view--insert-overlay nil))
  (setq ekg-org-view--insert-slots nil
        ekg-org-view--insert-index nil)
  (setq overriding-local-map nil)
  (when ekg-org-view--refresh-pending
    (setq ekg-org-view--refresh-pending nil)
    (ekg-org-view--refresh)))

(defun ekg-org-view--insert-create-task (slot title)
  "Create a new task from SLOT data with TITLE."
  (let* ((parent-id (plist-get slot :parent-id))
         (after-id (plist-get slot :after-id))
         (ekg-org--inhibit-view-refresh t)
         (siblings (if parent-id
                       (ekg-org-view--sorted-children parent-id)
                     (ekg-org-view--sorted-top-level)))
         (sort-order (if after-id
                         (ekg-org-view--assign-order-after siblings after-id)
                       ;; Inserting as first: renumber from 1 and take 0.
                       (when siblings
                         (triples-with-transaction
                           ekg-db
                           (let ((order 1))
                             (dolist (sib siblings)
                               (ekg-org-view--set-sort-order
                                (ekg-note-id sib) order)
                               (cl-incf order)))))
                       0))
         (note (ekg-note-create
                :text ""
                :mode 'org-mode
                :tags (list ekg-org-task-tag
                            (concat ekg-org-state-tag-prefix "todo"))
                :properties (append
                             (list :titled/title (list title)
                                   :org/sort-order sort-order)
                             (when parent-id
                               (list :org/parent parent-id))))))
    (ekg-save-note note)
    (ekg-org-view--refresh (ekg-note-id note))))

(defun ekg-org-view-insert-confirm ()
  "Confirm the insertion position and prompt for the task title.
The placeholder remains visible and updates live as you type."
  (interactive nil ekg-org-view-mode)
  (let* ((slot (nth ekg-org-view--insert-index ekg-org-view--insert-slots))
         (level (plist-get slot :level))
         (pos (plist-get slot :buffer-pos))
         (spacing (ekg-org-view--insert-slot-spacing slot))
         (ov ekg-org-view--insert-overlay)
         (buf (current-buffer)))
    ;; Release the positioning keymap so the minibuffer works normally.
    (setq overriding-local-map nil)
    (unwind-protect
        (let ((title
               (minibuffer-with-setup-hook
                   (lambda ()
                     (add-hook
                      'post-command-hook
                      (lambda ()
                        (when (buffer-live-p buf)
                          (let ((input (minibuffer-contents)))
                            (with-current-buffer buf
                              (ekg-org-view--insert-update-overlay
                               ov pos level spacing input)))))
                      nil t))
                 (read-string "Task title: "))))
          (ekg-org-view--insert-cleanup)
          (when (not (string-empty-p title))
            (ekg-org-view--insert-create-task slot title)))
      ;; Ensure cleanup happens even on C-g.
      (ekg-org-view--insert-cleanup))))

(defun ekg-org-view-insert-cancel ()
  "Cancel task insertion mode."
  (interactive nil ekg-org-view-mode)
  (ekg-org-view--insert-cleanup)
  (message "Cancelled."))

(defun ekg-org-view-create ()
  "Enter insertion mode to create a new task.
A placeholder shows where the new task will be inserted.  Use
\\`n'/\\`p' to move between positions, \\`R'/\\`L' to demote/promote,
\\`RET' to confirm, and \\`C-g' to cancel."
  (interactive nil ekg-org-view-mode)
  (let ((slots (ekg-org-view--build-insert-slots)))
    (if (null slots)
        ;; Empty view — just create a top-level task directly.
        (let ((title (read-string "Task title: ")))
          (when (not (string-empty-p title))
            (ekg-org-view--insert-create-task
             (list :parent-id nil :after-id nil) title)))
      (setq ekg-org-view--insert-slots slots
            ekg-org-view--insert-index (ekg-org-view--insert-find-nearest-index
                                        (point)
                                        (ekg-org-view--note-at-point))
            ekg-org-view--insert-overlay (make-overlay 1 1))
      (overlay-put ekg-org-view--insert-overlay 'priority 100)
      (setq overriding-local-map ekg-org-view-insert-mode-map)
      (ekg-org-view--insert-show)
      (message "Insert mode: n/p move, R/L demote/promote, RET confirm, C-g cancel"))))

(defun ekg-org--set-date-property (property prompt)
  "Set PROPERTY to a date chosen by the user with PROMPT.
Works in `ekg-org-view-mode' (operates on note at point),
`ekg-capture-mode', and `ekg-edit-mode' (operates on `ekg-note')."
  (let* ((in-view (derived-mode-p 'ekg-org-view-mode))
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

(defvar ekg-org-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'ekg-org-view-next-heading)
    (define-key map (kbd "p") #'ekg-org-view-previous-heading)
    (define-key map (kbd "f") #'ekg-org-view-next-sibling)
    (define-key map (kbd "b") #'ekg-org-view-previous-sibling)
    (define-key map (kbd "u") #'ekg-org-view-up-heading)
    (define-key map (kbd "t") #'ekg-org-view-cycle-state)
    (define-key map (kbd "a") #'ekg-org-view-archive)
    (define-key map (kbd "d") #'ekg-org-view-delete)
    (define-key map (kbd "c") #'ekg-org-view-create)
    (define-key map (kbd "TAB") #'ekg-org-view-toggle-collapse)
    (define-key map (kbd "RET") #'ekg-org-view-open-note)
    (define-key map (kbd "L") #'ekg-org-view-promote)
    (define-key map (kbd "l") #'ekg-org-view-promote)
    (define-key map (kbd "R") #'ekg-org-view-demote)
    (define-key map (kbd "r") #'ekg-org-view-demote)
    (define-key map (kbd "U") #'ekg-org-view-move-up)
    (define-key map (kbd "D") #'ekg-org-view-move-down)
    (define-key map (kbd "w") #'ekg-org-view-refile)
    (define-key map (kbd "g") #'ekg-org-view-refresh)
    (define-key map (kbd "C-c C-s") #'ekg-org-set-schedule)
    (define-key map (kbd "C-c C-d") #'ekg-org-set-deadline)
    map)
  "Keymap for `ekg-org-view-mode'.")

(defun ekg-org-view--refresh-all (&rest _args)
  "Refresh all live `ekg-org-view-mode' buffers.
Does nothing when `ekg-org--inhibit-view-refresh' is non-nil.
Accepts and ignores arguments so it can be used directly on
`ekg-note-save-hook' and `ekg-note-delete-hook'."
  (unless ekg-org--inhibit-view-refresh
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (derived-mode-p 'ekg-org-view-mode)))
        (with-current-buffer buf
          (ekg-org-view--refresh))))))

(define-derived-mode ekg-org-view-mode vui-mode "EKG-Org"
  "Major mode for viewing ekg org tasks in a hierarchical view.

\\{ekg-org-view-mode-map}"
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

(defun ekg-org-view--mount (&optional archive)
  "Mount the org task view buffer.
If ARCHIVE is non-nil, show archived tasks."
  (ekg-connect)
  (let* ((buf-name (if archive "*ekg-org-archive*" "*ekg-org-tasks*"))
         (buf (get-buffer-create buf-name))
         (vnode (vui-component 'ekg-org-view-root
                  :root-id nil :archive archive))
         (instance (vui--create-instance vnode nil))
         (vui--pending-effects nil))
    (setf (vui-instance-buffer instance) buf)
    (with-current-buffer buf
      (ekg-org-view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local vui--root-instance instance)
        (setq-local ekg-org-view--instance instance)
        (setq-local ekg-org-view--root-id nil)
        (setq-local ekg-org-view--archive archive)
        (let ((vui--root-instance instance)
              (vui--rendering-p t))
          (unwind-protect
              (vui--render-instance instance)
            (setq vui--rendering-p nil)))
        (widget-setup)
        (vui--run-pending-effects)
        (setq-local ekg-org-view--hl (make-overlay 1 1))
        (overlay-put ekg-org-view--hl 'face hl-line-face)
        (goto-char (point-min))
        (ekg-org-view--ensure-on-heading)
        (ekg-org-view--highlight)))
    (switch-to-buffer buf)
    instance))

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
  (ekg-connect)
  (let* ((buf (get-buffer-create (format "*ekg-org-task-%s*" id)))
         (vnode (vui-component 'ekg-org-view-root :root-id id))
         (instance (vui--create-instance vnode nil))
         (vui--pending-effects nil))
    (setf (vui-instance-buffer instance) buf)
    (with-current-buffer buf
      (ekg-org-view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local vui--root-instance instance)
        (setq-local ekg-org-view--instance instance)
        (setq-local ekg-org-view--root-id id)
        (let ((vui--root-instance instance)
              (vui--rendering-p t))
          (unwind-protect
              (vui--render-instance instance)
            (setq vui--rendering-p nil)))
        (widget-setup)
        (vui--run-pending-effects)
        (setq-local ekg-org-view--hl (make-overlay 1 1))
        (overlay-put ekg-org-view--hl 'face hl-line-face)
        (goto-char (point-min))
        (ekg-org-view--ensure-on-heading)
        (ekg-org-view--highlight)))
    (switch-to-buffer buf)
    instance))

(defun ekg-org--format-epoch-as-date (epoch)
  "Format EPOCH seconds as a readable date string."
  (format-time-string "%Y-%m-%d %a" (seconds-to-time epoch)))

(defun ekg-org-initialize ()
  "Initialize the ekg-org integration.

This adds the necessary schema and, if `ekg-agent' is available,
registers tools for interacting with org tasks."
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
    (add-to-list 'ekg-agent-extra-tools ekg-agent-org-tool-set-status)
    (add-to-list 'ekg-agent-extra-tools ekg-agent-org-tool-list-items)))

(provide 'ekg-org)
;;; ekg-org.el ends here
