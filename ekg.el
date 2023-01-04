;;; ekg.el --- A system for recording and linking information in emacs. -*- lexical-binding: t -*-

;; Copyright (c) 2022  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Package-Requires: ((triples "1.0"))
;; Keywords: knowledge graph, pkms
;; Version: 0.0
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
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
;; EKG is a note-taking and information storing application, centered around
;; tags, but with the ability to have other note metadata.

(require 'triples)
(require 'triples-backups)
(require 'seq)
(require 'ewoc)
(require 'cl-lib)
(require 'map)

;;; Code:

(defgroup ekg nil
  "The Emacs knowledge graph, an app for notes and structured data."
  :group 'applications)

(defcustom ekg-capture-default-mode 'org-mode
  "The default mode for all new notes."
  :type 'symbol
  :group 'ekg)

(defcustom ekg-capture-acceptable-modes '(org-mode markdown-mode text-mode)
  "Modes that make sense to use as note types."
  :type '(set symbol)
  :group 'ekg)

(defcustom ekg-capture-auto-tag-funcs '(ekg-date-tag)
  "Functions to run to create tags automatically.
The functions are run in the note buffer before saving it.
Return a list of tags to add."
  :type '(set function)
  :group 'ekg)

(defcustom ekg-format-funcs '()
  "Functions to run to format what we display to the user.
What we display on the UI is different than what we store
internally, which is the document itself. Each function here will
be run in sequence on a temporary buffer, and is responsible for
making the changes in the current buffer to affect the formatting
however it wants. It is the responsibility of each function to
check for the mode of the buffer."
  :type '(set function)
  :group 'ekg)

(defcustom ekg-db-file "~/.emacs.d/ekg.db"
  "Location of DB file used by EKG."
  :type 'file
  :group 'ekg)

(defconst ekg-default-num-backups 5
  "The number of backups to set when first using the database.
This can be overwritten by other database users, and will not be
set again. If you want to change the number of backups in your
database after it has been created, run `triples-backups-setup'.")

(defconst ekg-default-backups-strategy 'daily
  "The default database backup strategy when first setting up the
database. This can be overwritten by other database users, and
will not be set again. If you want to change the number of
backups in your database after it has been created, run
`triples-backups-setup'.")

(defface ekg-notes-mode-title
  '((((type graphic)) :height 2.0 :box t :inherit hl-line)
    (((type tty))) :underline t :inherit hl-line)
  "Face shown for the titles of EKG notes mode.")

(defface ekg-tag
  '((((type graphic)) :height 1.0 :box t :inherit default)
    (((type tty))) :underline t :inherit hl-line)
  "Face shown for EKG tags.")

(defface ekg-resource
  '((((type graphic)) :inherit fixed-pitch)
    (((type tty))) :bold t :inherit hl-line)
  "Face shown for EKG resource.")

(defface ekg-metadata
  '((default :background "grey" :inherit default))
  "Face shown for the metadata section of notes.")

(defvar ekg-db nil
  "Live sqlite database connection.")

(defvar ekg-metadata-parsers '(("Tags" . ekg--metadata-update-tag)
                               ("Resource" . ekg--metadata-update-resource)
                               ("Title" . ekg--metadata-update-title))
  "Metadata fields to functions for updating data based on buffer test.
Each function updates the buffer's `ekg-note' with the results of the field.
The function takes one argument, the field metadata property value.")

(defvar ekg-metadata-labels '((:titled/title . "Title"))
  "Alist of properties that can be on the note and their labels.
The label needs to match the keys in the `ekg-metadata-parsers' alist.")

(cl-defstruct ekg-note
  id text mode tags creation-time modified-time properties)

(defun ekg--connect ()
  "Ensure EKG-DB is connected."
  (unless ekg-db
    (setq ekg-db (triples-connect ekg-db-file))
    (ekg-add-schema)
    (unless (triples-backups-configuration ekg-db)
      (triples-backups-setup ekg-db ekg-default-num-backups
                             ekg-default-backups-strategy))))

(defun ekg--close ()
  "Close the EKG-DB connection."
  (setq ekg-db nil))

(defun ekg-add-schema ()
  "Add schema necessary for EKG to function."
  (triples-add-schema ekg-db 'tagged '(tag :base/type string))
  (triples-add-schema ekg-db 'text
                      '(text :base/unique t :base/type string)
                      '(mode :base/unique t :base/type symbol))
  (triples-add-schema ekg-db 'time-tracked
                      '(creation-time :base/unique t :base/type integer)
                      '(modified-time :base/unique t :base/type integer))
  (triples-add-schema ekg-db 'tag
                      '(tagged :base/virtual-reversed tagged/tag))
  (triples-add-schema ekg-db 'named 'name)
  (triples-add-schema ekg-db 'email 'address)
  ;; Person is just a marker
  (triples-add-schema ekg-db 'person)
  ;; A URL can be a subject too, and has data, including the title. The title is
  ;; something that can be used to select the subject via completion.
  (triples-add-schema ekg-db 'titled '(title :base/type string)))

(defun ekg--normalize-note (note)
  "Make sure NOTE adheres to ekg-wide constraints before saving.
This
  1) makes sure all tags are lowercase and trimmed.
  2) removes commas in tags, since those are used to separate tags.
  3) trim the text of any trailing or leading whitespace."
  (setf (ekg-note-tags note)
        (mapcar (lambda (tag)
                  (string-trim (downcase (string-replace "," "" tag)))) (ekg-note-tags note)))
  (setf (ekg-note-text note)
        (string-trim (ekg-note-text note))))

(defun ekg-save-note (note)
  "Save NOTE in database, replacing note information there."
  (ekg--connect)
  (ekg--normalize-note note)
  (triples-with-transaction ekg-db
    (triples-set-type ekg-db (ekg-note-id note) 'tagged :tag (ekg-note-tags note))
    (triples-set-type ekg-db (ekg-note-id note) 'text :text (ekg-note-text note)
                      :mode (ekg-note-mode note))
    ;; Note that we recalculate modified time here, since we are modifying the
    ;; entity.
    (let ((modified-time (time-convert (current-time) 'integer)))
      (triples-set-type ekg-db (ekg-note-id note) 'time-tracked
                        :creation-time (ekg-note-creation-time note)
                        :modified-time modified-time)
      (setf (ekg-note-modified-time note) modified-time))
    (mapc (lambda (tag) (triples-set-type ekg-db tag 'tag)) (ekg-note-tags note))
    (apply #'triples-set-types ekg-db (ekg-note-id note) (ekg-note-properties note)))
  (triples-backups-maybe-backup ekg-db ekg-db-file))

(defun ekg-get-notes-with-tags (tags)
  "Get all notes with TAGS, returning a list of `ekg-note' structs."
  (ekg--connect)
  (cl-loop for tag in tags
           with results = nil
           do
           (when-let (tag-ids (plist-get (triples-get-type ekg-db tag 'tag) :tagged))
             (setq results
                   (if results (seq-intersection tag-ids results)
                     tag-ids)))
           finally return
           (cl-loop for id in results
                    collect
                    (ekg-get-note-with-id id))))

(defun ekg-get-notes-with-tag (tag)
  "Get all notes with TAG, returning a list of `ekg-note' structs."
  (ekg-get-notes-with-tags (list tag)))

(defun ekg-get-note-with-id (id)
  "Get the specific note with ID."
  (let ((v (triples-get-subject ekg-db id)))
    (make-ekg-note :id id
                   :text (plist-get v :text/text)
                   :mode (plist-get v :text/mode)
                   :tags (plist-get v :tagged/tag)
                   :creation-time (plist-get v :time-tracked/creation-time)
                   :modified-time (plist-get v :time-tracked/modified-time)
                   :properties (map-into
                                (map-filter
                                (lambda (plist-key _)
                                  (not (member plist-key
                                               '(:text/text
                                                 :text/mode
                                                 :tagged/tag
                                                 :time-tracked/creation-time
                                                 :time-tracked/modified-time)))) v)
                                'plist))))

(defun ekg-note-delete (note)
  "Delete NOTE from the database.
This doesn't actually delete, but rather prepends all tags with
 `trash/'. This can be garbage collected at a later time.
If all tags are trash tags, then the note is really deleted."
  (triples-with-transaction 
    (if (seq-every-p #'ekg-tag-trash-p (ekg-note-tags note))
        (triples-delete-subject ekg-db (ekg-note-id note))
      (setf (ekg-note-tags note)
            (mapcar (lambda (tag) (unless (ekg-tag-trash-p tag)
                                    (ekg-mark-trashed tag)))
                    (ekg-note-tags note)))
      (ekg-save-note note)))
  (triples-backups-maybe-backup ekg-db ekg-db-file))

(defun ekg-has-live-tags-p (sub)
  "Return non-nil if SUB represents an undeleted note."
  (seq-filter (lambda (tag) (not (ekg-tag-trash-p tag))) (plist-get (triples-get-type ekg-db sub 'tagged) :tag)))

(defun ekg-displayable-note-text (note)
  "Return text, with mode-specific properties, of NOTE.
A text property `ekg-note-id' is added with the id of the note."
  (with-temp-buffer
    (when (ekg-note-text note)
      (insert (ekg-note-text note)))
    (when (ekg-note-mode note)
      (let ((mode-func (intern (format "%s-mode" (ekg-note-mode note)))))
        (if (fboundp mode-func) (funcall mode-func)
          (funcall (ekg-note-mode note)))))
    (mapc #'funcall ekg-format-funcs)
    (font-lock-ensure)
    (put-text-property (point-min) (point-max) 'ekg-note-id (ekg-note-id note))
    (buffer-string)))

(defvar ekg-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'ekg-capture-finalize)
    map)
  "Key map for `ekg-capture-mode', a minor mode.
This is used when capturing new notes.")

(define-minor-mode ekg-capture-mode
  "Minor mode for simple finish/cancel keybindings."
  :init-value nil
  :lighter "EKG CAP"
  :interactive nil
  (when ekg-capture-mode
    (setq-local completion-at-point-functions
                (cons #'ekg--capf completion-at-point-functions)
                header-line-format
                (substitute-command-keys
                 "\\<ekg-capture-mode-map>Capture buffer.  Finish \
`\\[ekg-capture-finalize]'."))))

(defvar ekg-capture-mode-hook nil
  "Hook for `ekg-capture-mode'.")

(defvar ekg-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'ekg-edit-finalize)
    map)
  "Key map for `ekg-edit-mode', a minor mode.
This is used when editing existing blocks.")

(define-minor-mode ekg-edit-mode
  "Minor mode for simple finish/cancel keybindings."
  :init-value nil
  :lighter "EKG ED"
  :interactive nil)

(defvar ekg-edit-mode-hook nil
  "Hook for `ekg-edit-mode'.")

(defvar-local ekg-note nil
  "Holds the note information for buffers adding or changing notes.")

(defvar-local ekg-note-orig-id nil
  "Holds the original ID (subject) for this note.
This is needed to identify references to refresh when the subject is changed." )

(defun ekg-note-create (text mode tags)
  "Create a new `ekg-note' with TEXT, MODE and TAGS."
  (let* ((time (time-convert (current-time) 'integer))
         (subject (sxhash (cons time (random 100)))))
    (make-ekg-note :id subject
                   :text text
                   :mode mode
                   :tags tags
                   :creation-time time
                   :modified-time time)))

(defun ekg--metadata-string-to-tag (s)
  "Return string S as a tag."
  (replace-regexp-in-string (rx ?\") "" s))

(defun ekg--metadata-string (property value)
  "Return a representation of PROPERTY with VALUE for the metadata.
This will be displayed at the top of the note buffer."
  (format "%s: %s\n" (propertize property 'face 'bold 'read-only t)
          value))

(defun ekg--should-show-id-p (id)
  "Return non-nil if the note ID should be shown to the user.
The ID can represent a browseable resource, which is meaningful to the user."
  (ffap-url-p id))

(defun ekg--replace-metadata ()
  "Replace the metadata in a buffer."
  (let ((note ekg-note))
    (with-temp-buffer
      (when (ekg--should-show-id-p (ekg-note-id note))
        (insert (ekg--metadata-string "Resource" (ekg-note-id note))))
      (insert
       (ekg--metadata-string "Tags"
                             (mapconcat (lambda (tag) (format "%s" tag))
                                        (ekg-note-tags note) ", ")))
      (map-apply (lambda (k v)
                   (when-let (label (cdr (assoc k ekg-metadata-labels)))
                     (insert (ekg--metadata-string
                              label
                              (if (listp v)
                                  (mapconcat (lambda (v) (format "%s" v))
                                             v ", ")
                                (format "%s" v))))))
                 (ekg-note-properties note))
      (buffer-string))))

(defun ekg--metadata-overlay ()
  "Return the overlay used for metadata."
  (or (car (seq-filter
            (lambda (o) (eq 'ekg-metadata (overlay-get o 'category)))
            (overlays-in (point-min) (point-max))))
      (make-overlay (point-min) (point-max) nil nil t)))

(defun ekg--metadata-modification (overlay after begin end &optional _)
  "Make sure that metadata region doesn't interfere with editing.
This function is called on modification within the metadata.
We want to make sure of a few things:
  1) The user isn't adding more than one empty line.
  2) There is at least one non-metadata line in the buffer."
  (when after
    (save-excursion
      (forward-line -1)
      (while (looking-at (rx (seq line-start (zero-or-more space) line-end)))
        (kill-line)
        (forward-line -1))
      (when (= (overlay-end overlay)
               (buffer-end 1))
        (goto-char (buffer-end 1))
        (insert "\n")))))

(defun ekg-edit-display-metadata ()
  "Create or edit the overlay to show metadata."
  (let ((o (ekg--metadata-overlay))
        (inhibit-read-only t))
    (buffer-disable-undo)
    (replace-region-contents (overlay-start o) (overlay-end o)
                             #'ekg--replace-metadata)
    (goto-char (overlay-end o))
    (insert "\n")
    (move-overlay o (point-min) (- (overlay-end o) 1))
    (overlay-put o 'before-string (propertize "Note properties\n" 'face '(underline ekg-metadata)
                                              'read-only t))
    (overlay-put o 'category 'ekg-metadata)
    (overlay-put o 'modification-hooks '(ekg--metadata-modification))
    (overlay-put o 'face 'ekg-metadata)
    (buffer-enable-undo)
    ;; If org-mode is on, the metadata messes up the org-element-cache, so let's disable it.
    (when (eq major-mode 'org-mode)
      (setq-local org-element-use-cache nil))))

(defun ekg-capture (&optional tags properties subject)
  "Capture a new note, with TAGS and other PROPERTIES.
If SUBJECT is given, force the triple subject to be that value."
  (interactive)
  (let ((buf (get-buffer-create "*EKG Capture*")))
    (set-buffer buf)
    (funcall ekg-capture-default-mode)
    (ekg-capture-mode 1)
    (setq ekg-note
            (ekg-note-create nil ekg-capture-default-mode
                             (seq-uniq (append
                                        tags
                                        (mapcan (lambda (f) (funcall f)) ekg-capture-auto-tag-funcs)))))
    (when subject
      (setf (ekg-note-id ekg-note) subject))
    (setf (ekg-note-properties ekg-note) properties)
    (ekg-edit-display-metadata)
    (goto-char (point-max))
    (switch-to-buffer-other-window buf)))

(defun ekg-capture-url (&optional url title)
  "Capture a new note given a URL and its TITLE."
  (interactive "MURL: \nMTitle: \n")
  (let ((cleaned-title (string-replace "," "" title)))
    (ekg-capture (list (concat "doc/" (downcase cleaned-title)))
                 ;; Remove commas from the value.
                 `(:titled/title ,cleaned-title) url)))

(defun ekg-capture-change-mode (mode)
  "Change the mode to MODE of the current note."
  (interactive (list
                (completing-read "Mode: " ekg-capture-acceptable-modes)) ekg-capture-mode)
  (let ((note ekg-note))
    (funcall (intern mode))
    (ekg-capture-mode 1)
    (setq-local header-line-format 
                (substitute-command-keys
                 "\\<ekg-capture-mode-map>Capture buffer.  Finish \
`\\[ekg-capture-finalize]'."))
    (setq ekg-note note)))

(defun ekg-edit (note)
  "Edit an existing NOTE."
  (interactive nil ekg-notes-mode)
  (let ((buf (get-buffer-create (format "*EKG Edit: %s*" (ekg-note-id note)))))
    (set-buffer buf)
    (when (= 0 (buffer-size))
      (when (ekg-note-mode note)
        (funcall (ekg-note-mode note)))
      (ekg-edit-mode 1)
      (setq-local completion-at-point-functions
                  (cons #'ekg--capf completion-at-point-functions)
                  header-line-format
                  (substitute-command-keys
                   "\\<ekg-edit-mode-map>Capture buffer.  Finish \
`\\[ekg-edit-finalize]'.")
                  ekg-note (copy-ekg-note note)
                  ekg-note-orig-id (ekg-note-id note))
      (ekg-edit-display-metadata)
      (insert (ekg-note-text note))
      (goto-char (+ 1 (overlay-end (ekg--metadata-overlay)))))
    (switch-to-buffer-other-window buf)))

(defun ekg--save-note-in-buffer ()
  "Save the current note.
Return the latest `ekg-note' object."
  (ekg--connect)
  (widen)
  (setf (ekg-note-text ekg-note)
        (buffer-substring (overlay-end (ekg--metadata-overlay))
                          (point-max))
        (ekg-note-mode ekg-note) major-mode
        (ekg-note-tags ekg-note) (seq-uniq (ekg-note-tags ekg-note)))
  (ekg-save-note ekg-note)
  ekg-note)

(defun ekg--in-metadata-p ()
  "If the point is in the metadata section."
  (< (point) (overlay-end (ekg--metadata-overlay))))

(defun ekg--metadata-current-field ()
  "Return the label and value of the current metadata property.
If none can be found, return NIL."
  (when (ekg--in-metadata-p)
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (when (string-match
              (rx (seq (group (seq (one-or-more (not ?\:))))
                       ?\: (one-or-more whitespace)
                       (group (zero-or-more anychar)))) line)
        (cons (substring-no-properties (match-string 1 line))
              (substring-no-properties (match-string 2 line)))))))

(defvar ekg-capf-field-complete-funcs
  '(("Tags" . ekg--tags-complete))
  "Functions to complete values in various metadata fields.
The function is expected to behave as normal for a function in
`completion-at-point-functions'.")

(defun ekg--capf ()
  "Completion function for all metadata at `completion-at-point-functions'.
If no completion function is found for the field type, don't
attempt the completion."
  (if-let (field (ekg--metadata-current-field))
    (when-let (completion-func (assoc (car field) ekg-capf-field-complete-funcs
                                      #'equal))
      (funcall (cdr completion-func)))
    ;; There's no current field, but we're in the metadata, so let's complete
    ;; the possible fields.
    (when (ekg--in-metadata-p)
      (ekg--field-name-complete))))

(defun ekg--field-name-complete ()
  "Completion function for metadata field names."
  (list (save-excursion (beginning-of-line) (point))
        (save-excursion (skip-chars-forward "^:\s\t\n") (point))
        (completion-table-dynamic
         (lambda (_)
           (seq-difference (mapcar #'car ekg-metadata-parsers)
                           (mapcar #'car (ekg--metadata-fields)))))))

(defun ekg--tags-cap-exit (completion finished)
  "Cleanup after completion at point happened in a tag.
The cleanup now is just to always have a space after every comma."
  (when finished
    (save-excursion
      (when (search-backward (format ",%s" completion) (line-beginning-position) t)
        (replace-match (format ", %s" completion))))))

(defun ekg--tags-complete ()
  "Completion function for tags, CAPF-style."
  (let ((end (save-excursion
               (skip-chars-forward "^,\t\n")
               (point)))
	    (start (save-excursion
                 (skip-chars-backward "^,\t\n")
                 (point))))
    (list start end (completion-table-dynamic
                     (lambda (_) (ekg-tags)))
          :exclusive t :exit-function #'ekg--tags-cap-exit)))

(defun ekg-edit-finalize ()
  "Save the edited note and refresh where it appears."
  (interactive nil ekg-edit-mode)
  (ekg--update-from-metadata)
  (let ((note (ekg--save-note-in-buffer))
        (orig-id ekg-note-orig-id))
    (kill-buffer)
    (cl-loop for b being the buffers do
           (with-current-buffer b
               (when (and (eq major-mode 'ekg-notes-mode)
                          (seq-intersection (ekg-note-tags note)
                                            ekg-notes-tags))
                 (let ((n (ewoc-nth ekg-notes-ewoc 0)))
                   (while n
                     (when (or (equal (ekg-note-id (ewoc-data n))
                                      (ekg-note-id note))
                               (and orig-id
                                    (equal orig-id (ekg-note-id (ewoc-data n)))))
                       (ewoc-set-data n note))
                     (setq n (ewoc-next ekg-notes-ewoc n))))
                 (ewoc-refresh ekg-notes-ewoc))))))

(defun ekg--split-metadata-string (val)
  "Split a multi-valued metadata field into the component values.
The metadata fields are comma separated."
  (split-string val (rx (seq ?\, (zero-or-more space))) t (rx (1+ space))))

(defun ekg--metadata-update-tag (val)
  "Update the tag field from the metadata VAL."
  (setf (ekg-note-tags ekg-note) (ekg--split-metadata-string val)))

(defun ekg--metadata-update-title (val)
  "Update the title field from the metadata VAL."
  (setf (ekg-note-properties ekg-note)
        (plist-put (ekg-note-properties ekg-note) :titled/title
                   (ekg--split-metadata-string val))))

(defun ekg--metadata-update-resource (val)
  "Update the resource to the metadata VAL."
  (when (and (not (string= val (ekg-note-id ekg-note)))
             (or (not (ekg-note-id ekg-note))
                 (y-or-n-p "Changing the resource of this note will also change all references to it.  Are you sure?")))
    (triples-with-transaction ekg-db
      (when (ekg-note-id ekg-note)
        (let* ((old-id (ekg-note-id ekg-note))
               (existing-types (triples-get-types ekg-db old-id))
               (conflicting-types (seq-union existing-types '(text 'tag 'titled))))
          (when (and conflicting-types
                     (y-or-n-p "Existing data exists on this resource, replace?"))
            (mapc (lambda (type) (triples-remove-type ekg-db old-id type)) conflicting-types))
          (triples-move-subject ekg-db old-id val))))
    (setf (ekg-note-id ekg-note) val)))

(defun ekg--metadata-fields ()
  "Return all metadata fields as a cons of labels and values."
  (save-excursion
    (let ((mo (ekg--metadata-overlay))
          (fields))
      (goto-char (overlay-start mo))
      (while (< (point) (overlay-end mo))
        (if-let (field (ekg--metadata-current-field))
            (push field fields)
          (warn "EKG: No field could be parsed from metadata line at point %s" (point)))
        (forward-line))
      fields)))

(defun ekg--update-from-metadata ()
  "Update the `ekg-note' object from the metadata."
  (cl-loop for field in (ekg--metadata-fields)
           do
           (if-let (func (assoc (car field) ekg-metadata-parsers))
                (funcall (cdr func) (cdr field))
              (warn "EKG: No function found for field %s" (car field)))))

(defun ekg-capture-finalize ()
  "Save the current note."
  (interactive nil ekg-capture-mode)
  (ekg--update-from-metadata)
  (ekg--save-note-in-buffer)
  (let ((note ekg-note))
    (kill-buffer)
    (cl-loop for b being the buffers do
           (with-current-buffer b
               (when (and (eq major-mode 'ekg-notes-mode)
                          (seq-intersection (ekg-note-tags note)
                                            ekg-notes-tags))
                 (ewoc-enter-last ekg-notes-ewoc note))))))

(defun ekg-tag-trash-p (tag)
  "Return non-nil if TAG is part of the trash."
  ;; All tags should be strings, but better to ignore violations here.
  (and (stringp tag)
       (string-match-p (rx (seq string-start "trash/")) tag)))

(defun ekg-mark-trashed (tag)
  "Return TAG transformed to mark it as trash."
  (format "trash/%s" tag))

(defun ekg-rename-tag (from-tag to-tag)
  "Rename FROM-TAG to TO-TAG.
This can be done whether or not TO-TAG exists or not."
  (interactive (list (completing-read "From tag: " (ekg-tags))
                     (completing-read "To tag: " (ekg-tags))))
  (triples-with-transaction
    ekg-db
    (pcase triples-sqlite-interface
      ('builtin (sqlite-execute
                 ekg-db
                 "UPDATE triples SET object = ? WHERE object = ? AND predicate = 'tagged/tag'"
                 (list (triples-standardize-val to-tag) (triples-standardize-val from-tag))))
      ('emacsql (emacsql ekg-db [:update triples :set (= object $s1) :where (= object $s2) :and (= predicate 'tagged/tag)]
                         to-tag from-tag)))
    (triples-remove-type ekg-db from-tag 'tag)
    (triples-set-type ekg-db to-tag 'tag))
  (triples-backups-maybe-backup ekg-db ekg-db-file))

(defun ekg-tags ()
  "Return a list of all tags.
Does not include any trash tags."
  (ekg--connect)
  (seq-filter (lambda (tag) (not (ekg-tag-trash-p tag)))
              (triples-subjects-of-type ekg-db 'tag)))

(defun ekg-tags-including (substring)
  "Return all tags including SUBSTRING."
  (ekg--connect)
  (seq-filter (lambda (tag) (and (not (ekg-tag-trash-p tag))
                                 (string-match-p (rx (literal substring)) tag)))
              (triples-subjects-of-type ekg-db 'tag)))

(defun ekg-tags-display (tags)
  "Return a propertized representation of TAGS, a list.
The tags are separated by spaces."
  (mapconcat (lambda (tag) (propertize tag 'face 'ekg-tag))
             (sort tags #'string<) " "))

(defun ekg-display-note (note)
  "Display NOTE in buffer."
  (when (ekg--should-show-id-p (ekg-note-id note))
    (insert
     (propertize 
      (format "[%s]\n" (ekg-note-id note))
      'face 'ekg-resource)))
  (insert (ekg-displayable-note-text note))
  (insert "\n")
  (insert (ekg-tags-display (ekg-note-tags note))))

(defvar ekg-notes-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "a" #'ekg-notes-any-tags)
    (define-key map "c" #'ekg-notes-create)
    (define-key map "d" #'ekg-notes-delete)
    (define-key map "g" #'ekg-notes-refresh)
    (define-key map "n" #'ekg-notes-next)
    (define-key map "o" #'ekg-notes-open)
    (define-key map "b" #'ekg-notes-browse)
    (define-key map "p" #'ekg-notes-previous)
    (define-key map "r" #'ekg-notes-remove)
    (define-key map "t" #'ekg-notes-tag)
    map))

(define-derived-mode ekg-notes-mode fundamental-mode "ekg-notes"
  "Major mode for showing a list of notes that can be interacted with."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defvar-local ekg-notes-fetch-notes-function nil
  "Function to call to fetch the notes that define this buffer.")

(defvar-local ekg-notes-ewoc nil
  "Ewoc for the notes buffer.")

(defvar-local ekg-notes-hl nil
  "Highlight for the notes buffer.")

(defvar-local ekg-notes-tags nil
  "List of associated tags for creating and removing notes.")

(defun ekg--note-highlight ()
  "In the buffer, highlight the current note."
  (let ((node (ewoc-locate ekg-notes-ewoc)))
    (when (and node (ewoc-location node))
      (move-overlay ekg-notes-hl
                    (ewoc-location node)
                    (- (or (if-let (next (ewoc-next ekg-notes-ewoc node))
                               (ewoc-location next)
                             (point-max))) 1)))))

(defun ekg--current-note-or-error ()
  "Return the current `ekg-note'.
Raise an error if there is no current note."
  (unless (eq major-mode 'ekg-notes-mode)
    (error "This command can only be used in `ekg-notes-mode'."))
  (if-let (node (ewoc-locate ekg-notes-ewoc))
      (ewoc-data node)
    (error "No current note is available to act on!  Create a new note first with `ekg-capture'.")))

(defun ekg-notes-tag (&optional tag)
  "Show notes associated with TAG.
If TAG is nil, it will be read, selecting from the list of the current note's
tags."
  (interactive (list (completing-read "Tag: " (ekg-note-tags (ekg--current-note-or-error))))
              ekg-notes-mode)
  (ekg-show-tag tag))

(defun ekg-notes-open ()
  "Open the current note."
  (interactive nil ekg-notes-mode)
  (ekg-edit (ekg--current-note-or-error)))

(defun ekg-notes-delete ()
  "Delete the current note."
  (interactive nil ekg-notes-mode)
  (let ((note (ekg--current-note-or-error))
        (inhibit-read-only t))
    (when (y-or-n-p "Or you sure you want to delete this note?")
      (ekg-note-delete note)
      (ewoc-delete ekg-notes-ewoc (ewoc-locate ekg-notes-ewoc))
      (ekg--note-highlight))))

(defun ekg-notes-remove ()
  "Remove the current tags from the current note.
This prepends the tags with trash, which removes them from view,
but allows for re-instatement later."
  (interactive nil ekg-notes-mode)
  (let ((note (ekg--current-note-or-error)))
    (setf (ekg-note-tags note)
          (mapcar (lambda (tag)
                    (if (member tag ekg-notes-tags)
                        (ekg-mark-trashed tag)
                      tag)) (ekg-note-tags note)))
    (ekg-save-note note))
  (setq buffer-read-only nil)
    (ewoc-delete ekg-notes-ewoc (ewoc-locate ekg-notes-ewoc))
    (setq buffer-read-only t)
    (ekg--note-highlight))

(defun ekg-notes-browse ()
  "If the note is about a browseable resource, browse to it.
For URLs, this will use `browse-url'."
  (interactive nil ekg-notes-mode)
  (let ((note (ekg--current-note-or-error)))
    (cond ((ffap-url-p (ekg-note-id note))
           (browse-url (ekg-note-id note))))))

(defun ekg-notes-refresh ()
  "Refresh the current `ekg-notes' buffer."
  (interactive nil ekg-notes-mode)
  (ekg--show-notes ekg-notes-fetch-notes-function ekg-notes-tags))

(defun ekg-notes-create ()
  "Add a note that default in the tags in the buffer."
  (interactive nil ekg-notes-mode)
  (ekg-capture ekg-notes-tags))

(defun ekg-notes-next ()
  "Move to the next note, if possible."
  (interactive nil ekg-notes-mode)
  (if-let (next (ewoc-next ekg-notes-ewoc (ewoc-locate ekg-notes-ewoc)))
      (progn
        (goto-char (ewoc-location next))
        (ekg--note-highlight))))

(defun ekg-notes-previous ()
  "Move to the previous note, if possible."
  (interactive nil ekg-notes-mode)
  (if-let (prev (ewoc-prev ekg-notes-ewoc (ewoc-locate ekg-notes-ewoc)))
      (progn
        (goto-char (ewoc-location prev))
        (ekg--note-highlight))))

(defun ekg-notes-any-tags ()
  "Show notes with any of the tags in the current note."
  (interactive nil ekg-notes-mode)
  (ekg-show-tags-any (ekg-note-tags (ewoc-data (ewoc-locate ekg-notes-ewoc)))))

(defun ekg--show-notes (notes-func tags)
  "Display notes from NOTES-FUNC in buffer, with notes having TAGS."
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((ewoc (ewoc-create #'ekg-display-note (propertize (ekg-tags-display tags) 'face 'ekg-notes-mode-title))))
    (mapc (lambda (note) (ewoc-enter-last ewoc note))
          (sort
           (funcall notes-func)
           (lambda (a b)
             (< (ekg-note-creation-time a)
                (ekg-note-creation-time b)))))
    (ekg-notes-mode)
    (setq-local ekg-notes-ewoc ewoc
                ekg-notes-fetch-notes-function notes-func
                ekg-notes-hl (make-overlay 1 1)
                ekg-notes-tags tags)
    (overlay-put ekg-notes-hl 'face hl-line-face)
    ;; Move past the title
    (forward-line 1)
    (ekg--note-highlight)))

(defun ekg-show-tags-any (tags)
  "Show notes with any of TAGS."
  (interactive (list (completing-read-multiple "Tags: " (ekg-tags))))
  (let ((buf (get-buffer-create (format "ekg tags (any): %s" (mapconcat #'identity tags " ")))))
    (set-buffer buf)
    (ekg--show-notes
     (lambda () (seq-uniq (mapcan (lambda (tag) (ekg-get-notes-with-tag tag)) tags))) tags)
    (switch-to-buffer buf)))

(defun ekg-show-tags-all (tags)
  "Show notes that contain all TAGS."
  (interactive (list (completing-read-multiple "Tags: " (ekg-tags))))
  (let ((buf (get-buffer-create (format "ekg tags (all): %s" (mapconcat #'identity tags " ")))))
    (set-buffer buf)
    (ekg--show-notes (lambda () (ekg-get-notes-with-tags tags)) tags)
    (switch-to-buffer buf)))

(defun ekg-show-tag (tag)
  "Show notes that contain TAG."
  (interactive (list (completing-read "Tag: " (ekg-tags))))
  (let ((buf (get-buffer-create (format "ekg tag: %s" tag))))
    (set-buffer buf)
    (ekg--show-notes (lambda () (ekg-get-notes-with-tag tag)) (list tag))
    (switch-to-buffer buf)))

(defun ekg-show-trash ()
  "Show notes that have tags prefixed by tags."
  (interactive)
  (ekg-show-tags-any
   (seq-filter #'ekg-tag-trash-p (triples-subjects-of-type ekg-db 'tag))))

(defun ekg-show-today ()
  "Show all notes with today's date as a tag."
  (interactive)
  (ekg-show-tag (car (ekg-date-tag))))

(defun ekg-document-titles ()
  "Return an alist of all titles.
The key is the subject and the value is the title."
  (ekg--connect)
  (mapcan (lambda (sub)
            (mapcar (lambda (title) (cons sub title)) (plist-get (triples-get-type ekg-db sub 'titled) :title)))
          (seq-filter #'ekg-has-live-tags-p
                      (triples-subjects-of-type ekg-db 'titled))))

(defun ekg-browse-url (title)
  "Browse the url corresponding to TITLE.
If no corresponding URL is found, an error is thrown."
  (interactive (list (completing-read "Doc: "
                                      (mapcan
                                       (lambda (tcons)
                                         (when (ffap-url-p (car tcons))
                                           (list (cdr tcons))))
                                       (ekg-document-titles)))))
  (let ((subjects (seq-filter #'ffap-url-p (triples-subjects-with-predicate-object ekg-db 'titled/title title))))
    (when (= 0 (length subjects)) (error "Could not fetch existing URL title: %s" title))
    (when (> (length subjects) 1) (warn "Multiple URLs with the same title exist: %s" title))
    (browse-url (car subjects))))

;; Auto-tag functions

(defun ekg-tag-for-date (&optional date)
  (format-time-string "date/%F" date))

(defun ekg-date-tag ()
  "Get single tag representing the date as a ISO 8601 format."
  (list (ekg-tag-for-date)))

(defun ekg-upgrade-db ()
  "After updating, do any necessary upgrades needed by changes in schema or use.
This is designed so that it can be run an arbitrary number of
times, if there's nothing to do, it won't have any affect."
  (interactive)
  (ekg--connect)
  (cl-loop for tag in (seq-filter #'iso8601-valid-p (ekg-tags))
           do
           (ekg-rename-tag tag (format "date/%s" tag)))
  (cl-loop for sub in (seq-uniq (mapcar #'car (triples-with-predicate ekg-db :reference/url)))
           do
           (triples-db-delete ekg-db sub 'reference/url)))

;; Links for org-mode
(require 'ol)

(defun ekg--store-any-tags-link ()
  "Store a link to an any-tags ekg page."
  (when (eq major-mode 'ekg-notes-mode)
    ;; TODO: Stop assuming every notes mode is an any tags.
    (org-link-store-props :type "ekg-tags-any" :link (concat "ekg-tags-any:" (format "%S" ekg-notes-tags))
                          :description (format "EKG page for any of the tags: %s"
                                               (mapconcat #'identity ekg-notes-tags ", ")))))

(defun ekg--open-any-tags-link (stags)
  "Open a link to an ekg page given by STAGS.
STAGS is a string version of a tag, as stored in a link."
  (let ((tags (read stags)))
    (if (= 1 (length tags))
        (ekg-show-tag (car tags))
      (ekg-show-tags-any tags))))

(defun ekg--store-note-link ()
  "Store a link to an individual note."
  (let ((id (when (member 'ekg-edit-mode local-minor-modes)
              (ekg-note-id ekg-note))))
    (when id
      (org-link-store-props :type "ekg-note"
                            :link (concat "ekg-note:" (format "%S" id))
                            :description (format "EKG note: %S" id)))))

(defun ekg--open-note-link (id)
  "Open a link to a note given its ID."
  (ekg-edit (ekg-get-note-with-id (read id))))

(org-link-set-parameters "ekg-tags-any" :follow #'ekg--open-any-tags-link
                         :store #'ekg--store-any-tags-link)

(org-link-set-parameters "ekg-note" :follow #'ekg--open-note-link
                         :store #'ekg--store-note-link)

(provide 'ekg)

;;; ekg.el ends here
