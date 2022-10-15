;; -*- lexical-binding: t -*-
;;; ekg.el --- A system for recording and linking information in emacs.

;; Copyright (c) 2022  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
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
(require 'seq)
(require 's)
(require 'ewoc)
(require 'cl-macs)
(require 'kv)

(defgroup ekg nil
  "The emacs knowledge graph, an app for notes and structured data.")

(defcustom ekg-capture-default-mode 'org-mode
  "The default mode for all new notes."
  :type 'symbol
  :group 'ekg)

(defcustom ekg-capture-acceptable-modes '(org-mode markdown-mode fundamental-mode)
  "Modes that make sense to use as note types."
  :type '(set symbol)
  :group 'ekg)

(defcustom ekg-capture-auto-tag-funcs '(ekg-date-tag)
  "Functions to run to create tags automatically.
The functions are run in the note buffer before saving it.
Return a list of tags to add."
  :type '(set function)
  :group 'ekg)

(defcustom ekg-db-file "~/.emacs.d/ekg.db"
  "Location of DB file used by EKG."
  :type 'file
  :group 'ekg)

(defface ekg-title
  '((((type graphic)) :height 2.0 :box t :inherit hl-line)
    (((type tty))) :underline t :inherit hl-line)
  "Face shown for EKG titles")

(defface ekg-tag
  '((((type graphic)) :height 1.0 :box t :inherit default)
    (((type tty))) :underline t :inherit hl-line)
  "Face shown for EKG tags")

(defface ekg-metadata
  '((default :background "grey" :inherit default))
  "Face shown for the metadata section of notes.")

(defvar ekg-db nil
  "Live sqlite database connection.")

(defvar ekg-metadata-parsers '(("Tags" . ekg--metadata-update-tag)
                               ("URL" . ekg--metadata-update-url))
  "Alist of metadata field to a function that updates the buffer's
`ekg-note' with the results of the field. The function takes one
argument, the field metadata property value.")

(defvar ekg-metadata-labels '((reference/url . "URL"))
  "Alist of properties that can be on the note, and the labels they
have in the metadata section. The label needs to match the keys
in the `ekg-metadata-parsers' alist.")

(cl-defstruct ekg-note
  id text mode tags creation-time modified-time properties)

(defun ekg--connect ()
  "Ensure EKG-DB is connected."
  (unless ekg-db
    (setq ekg-db (triples-connect ekg-db-file))
    (ekg-add-schema)))

(defun ekg--close ()
  "Close the EKG-DB connection."
  (setq ekg-db nil))

(defun ekg-add-schema ()
  "Add schema necessary for EKG to function."
  (triples-add-schema ekg-db 'tagged 'tag)
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
  (triples-add-schema ekg-db 'reference '(url :base/type string)))

(defun ekg--normalize-note (note)
  "Make sure NOTE adheres to ekg-wide constraints before saving.
This makes sure all tags are lowercase. It also removes commas in
tags, since those are used to parse out."
  (setf (ekg-note-tags note)
        (mapcar (lambda (tag) (downcase (string-replace "," "" tag))) (ekg-note-tags note))))

(defun ekg-save-note (note)
  "Save NOTE in database, replacing note information there."
  (ekg--connect)
  (ekg--normalize-note note)
  (emacsql-with-transaction ekg-db
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
    (apply #'triples-set-types ekg-db (ekg-note-id note) (ekg-note-properties note))))

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
                    (let ((v (triples-get-subject ekg-db id)))
                      (make-ekg-note :id id
                                     :text (plist-get v :text/text)
                                     :mode (plist-get v :text/mode)
                                     :tags (plist-get v :tagged/tag)
                                     :creation-time (plist-get v :time-tracked/creation-time)
                                     :modified-time (plist-get v :time-tracked/modified-time)
                                     :properties (kvalist->plist
                                                  (seq-filter (lambda (kvcons)
                                                                (not (member (car kvcons)
                                                                             '(text/text
                                                                               text/mode
                                                                               tagged/tag
                                                                               time-tracked/creation-time
                                                                               time-tracked/modified-time))))
                                                              (kvplist->alist v))))))))

(defun ekg-get-notes-with-tag (tag)
  "Get all notes with TAG, returning a list of `ekg-note' structs."
  (ekg-get-notes-with-tags (list tag)))

(defun ekg-note-delete (note)
  "Delete NOTE from the database.
This doesn't actually delete, but rather prepends all tags with
 'trash/'. This can be garbage collected at a later time.
If all tags are trash tags, then the note is really deleted."
  (if (seq-every-p #'ekg-tag-trash-p (ekg-note-tags note))
      (triples-delete-subject ekg-db (ekg-note-id note))
    (setf (ekg-note-tags note)
          (mapcar (lambda (tag) (unless (ekg-tag-trash-p tag)
                                  (ekg-mark-trashed tag)))
                  (ekg-note-tags note)))
    (ekg-save-note note)))

(defun ekg-note-text (note)
  "Return text, with mode-specific properties, of NOTE.
A text property `ekg-note-id' is added with the id of the note."
  (with-temp-buffer
    (insert (ekg-note-text note))
    (when (ekg-note-mode note)
              (let ((mode-func (intern (format "%s-mode" (ekg-note-mode note)))))
                (if (fboundp mode-func) (funcall #'mode-func)
                  (funcall (ekg-note-mode note))))
              (font-lock-ensure)
              (put-text-property (point-min) (point-max) 'ekg-note-id (ekg-note-id note))
              (buffer-string))))


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

(define-minor-mode ekg-edit-mode
  "Minor mode for simple finish/cancel keybindings."
  :init-value nil
  :lighter "EKG ED"
  :interactive nil)

(defvar ekg-edit-mode-hook nil
  "Hook for `ekg-edit-mode'.")

(defvar ekg-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'ekg-edit-finalize)
    map)
  "Key map for `ekg-edit-mode', a minor mode.
This is used when editing existing blocks.")

(defvar-local ekg-note nil
  "Holds the note information for buffers adding or changing notes.")

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

(defun ekg--replace-metadata ()
  "Replace the metadata in a buffer."
  (let ((note ekg-note))
    (with-temp-buffer
      (insert
       (ekg--metadata-string "Tags"
                             (mapconcat (lambda (tag) (format "%s" tag))
                                        (ekg-note-tags note) ", ")))
      (mapc (lambda (pval-cons)
              (when-let (label (cdr (assoc (car pval-cons) ekg-metadata-labels)))
                (insert (ekg--metadata-string
                         label
                         (if (listp (cdr pval-cons))
                             (mapconcat (lambda (v) (format "%s" v))
                                        (cdr pval-cons) ", ")
                           (format "%s" (cdr pval-cons)))))))
            (kvplist->alist (ekg-note-properties note)))
      (buffer-string))))

(defun ekg-tag-completion-at-point ()
  "A completion-at-point function to complete ekg tags."
  (if (and (eq major-mode 'ekg-note-mode)
           ;; does our word start with a hash?
           (s-starts-with-p "#" (thing-at-point 'symbol)))
      (start end (completion-table-dynamic
                  (lambda (_) (ekg-tag))) :exclusive t)))

(defun ekg--metadata-overlay ()
  "Return the overlay used for metadata"
  (or (car (seq-filter
            (lambda (o) (eq 'ekg-metadata (overlay-get o 'category)))
            (overlays-in (point-min) (point-max))))
      (make-overlay (point-min) (point-max) nil nil t)))

(defun ekg-edit-display-metadata ()
  "Create or edit the overlay to show metadata."
  (let ((o (ekg--metadata-overlay))
        (inhibit-read-only t))
    (replace-region-contents (overlay-start o) (overlay-end o)
                             #'ekg--replace-metadata)
    (goto-char (overlay-end o))
    (insert "\n")
    (move-overlay o (point-min) (- (overlay-end o) 1))
    (overlay-put o 'category 'ekg-metadata)
    (overlay-put o 'face 'ekg-metadata)))

(defun ekg-capture (&optional tags)
  "Capture a new note, with at least TAGS."
  (interactive)
  (let ((buf (get-buffer-create "*EKG Capture*")))
    (set-buffer buf)
    (funcall ekg-capture-default-mode)
    (ekg-capture-mode 1)
    (let* ((time (time-convert (current-time) 'integer))
           (subject (sxhash (cons time (random 100)))))
      (setq ekg-note
            (ekg-note-create nil ekg-capture-default-mode
                             (seq-uniq (append
                                        tags
                                        (mapcan (lambda (f) (funcall f)) ekg-capture-auto-tag-funcs))))))
    (ekg-edit-display-metadata)
    (goto-char (point-max))
    (switch-to-buffer-other-window buf)))

(defun ekg-capture-url (&optional url)
  "Capture a new note given a URL."
  (interactive "MURL: ")
  (ekg-capture)
  (goto-char (overlay-start (ekg--metadata-overlay)))
  (insert "URL: " url "\n"))

(defun ekg-capture-change-mode (mode)
  "Change the mode of the current note."
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
  (let ((buf (get-buffer-create "*EKG Edit*")))
    (set-buffer buf)
    (erase-buffer)
    (when (ekg-note-mode note)
      (funcall (ekg-note-mode note)))
    (ekg-edit-mode 1)
    (setq-local completion-at-point-functions
                (cons #'ekg--capf completion-at-point-functions)
                header-line-format
                (substitute-command-keys
                 "\\<ekg-edit-mode-map>Capture buffer.  Finish \
`\\[ekg-edit-finalize]'.")
                ekg-note (copy-ekg-note note))
    (ekg-edit-display-metadata)
    (insert (ekg-note-text note))
    (goto-char (+ 1 (overlay-end (ekg--metadata-overlay))))
    (switch-to-buffer-other-window buf)))

(defun ekg--save-note-in-buffer ()
  "Save the current note.
Return the latest `ekg-note' object."
  (ekg--connect)
  (widen)
  (setf (ekg-note-text ekg-note)
        (buffer-substring (+ 1 (overlay-end (ekg--metadata-overlay)))
                          (point-max))
        (ekg-note-mode ekg-note) major-mode
        (ekg-note-tags ekg-note) (seq-uniq (ekg-note-tags ekg-note)))
  (ekg-save-note ekg-note)
  ekg-note)

(defun ekg--metadata-current-field ()
  "Return the label and value of the current metadata property.
If none can be found, return NIL."
  (when (< (point) (overlay-end (ekg--metadata-overlay)))
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
  (when-let (field (ekg--metadata-current-field))
    (when-let (completion-func (assoc (car field) ekg-capf-field-complete-funcs
                                      #'equal))
      (funcall (cdr completion-func)))))

(defun ekg--tags-complete ()
  "Completion function for tags, CAPF-style."
  (let ((end (save-excursion
               (skip-chars-forward "^,\t\n")
               (point)))
	    (start (save-excursion
                 (skip-chars-backward "^,\t\n")
                 (point))))
    (list start end (completion-table-dynamic
                     (lambda (_) (ekg-tags))))))

(defun ekg-capture-edit-remove-tags (tags)
  "Remove TAGS from the current list of tags."
  (interactive (list (completing-read-multiple "Remove tags: " (ekg-note-tags ekg-note)
                                               nil t)) ekg-capture-mode ekg-edit-mode)
  (setf (ekg-note-tags ekg-note) (seq-difference (ekg-note-tags ekg-note) tags  #'equal))
  (ekg-edit-display-tags))

(defun ekg-capture-edit-add-tags (tags)
  "Add TAGS to edit or capture buffer."
  (interactive (list (completing-read-multiple "Additional tags: "
                                               (seq-difference
                                                (ekg-tags)
                                                (ekg-note-tags ekg-note))))
               ekg-capture-mode ekg-edit-mode)
  (setf (ekg-note-tags ekg-note) (append tags (ekg-note-tags ekg-note)))
  (ekg-edit-display-tags))

(defun ekg-edit-finalize ()
  "Save the edited note and refresh where it appears."
  (interactive nil ekg-edit-mode)
  (ekg--update-from-metadata)
  (let ((note (ekg--save-note-in-buffer)))
    (kill-buffer)
    (cl-loop for b being the buffers do
           (with-current-buffer b
               (when (and (eq major-mode 'ekg-notes-mode)
                          (seq-intersection (ekg-note-tags note)
                                            ekg-notes-tags))
                 (let ((n (ewoc-nth ekg-notes-ewoc 0)))
                   (while n
                     (when (equal (ekg-note-id (ewoc-data n))
                                  (ekg-note-id note))
                       (ewoc-set-data n note))
                     (setq n (ewoc-next ekg-notes-ewoc n))))
                 (ewoc-refresh ekg-notes-ewoc))))))

(defun ekg--metadata-update-tag (val)
  "Update the tag field from the metadata VAL."
  (setf (ekg-note-tags ekg-note) (s-split (rx (seq ?\, (zero-or-more space))) val t)))

(defun ekg--metadata-update-url (val)
  "Update the url field from the metadata VAL."
  (plist-put
   (ekg-note-properties ekg-note)
   :reference/url (s-split (rx (seq ?\, (zero-or-more space))) val t)))

(defun ekg--update-from-metadata ()
  "Update the `ekg-note' object from the metadata."
  (save-excursion
    (let ((mo (ekg--metadata-overlay)))
      (goto-char (overlay-start mo))
      (while (< (point) (overlay-end mo))
        (if-let (field (ekg--metadata-current-field))
            (if-let (func (assoc (car field) ekg-metadata-parsers))
                (funcall (cdr func) (cdr field))
              (warn "EKG: No function found for field %s" (car field)))
          (warn "EKG: No field could be parsed from metadata line at point %s" (point)))
        (forward-line)))))

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
  "Return true if TAG is part of the trash."
  ;; All tags should be strings, but better to ignore violations here.
  (and (stringp tag)
       (s-starts-with-p "trash/" tag)))

(defun ekg-mark-trashed (tag)
  "Return TAG transformed to mark it as trash."
  (format "trash/%s" tag))

(defun ekg-tags ()
  "Return a list of all tags.
Does not include any 'trash' tags."
  (ekg--connect)
  (seq-filter (lambda (tag) (not (ekg-tag-trash-p tag)))
              (triples-subjects-of-type ekg-db 'tag)))

(defun ekg-tags-display (tags)
  "Return a propertized representation of TAGS, a list."
  (mapconcat (lambda (tag) (propertize tag 'face 'ekg-tag))
             (sort tags #'string<) " "))

(defun ekg-display-note (note)
  "Display NOTE in buffer."
  (insert (ekg-note-text note))
  (insert "\n")
  (insert (ekg-tags-display (ekg-note-tags note))))

(defvar-keymap ekg-notes-mode-map
  :full t
  :suppress 'nodigits
  "a" #'ekg-notes-any-tags
  "c" #'ekg-notes-create
  "d" #'ekg-notes-delete
  "g" #'ekg-notes-refresh
  "n" #'ekg-notes-next
  "o" #'ekg-notes-open
  "p" #'ekg-notes-previous
  "r" #'ekg-notes-remove
  "t" #'ekg-notes-tag)

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
  "Select amongst the tags of the current note."
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
          (mapcar (lambda (tag) (if (member tag ekg-notes-tags)
                            (ekg-marked-trash tag)
                          tag)) (ekg-note-tags note)))
    (ekg-save-note note))
  (setq buffer-read-only nil)
    (ewoc-delete ekg-notes-ewoc (ewoc-locate ekg-notes-ewoc))
    (setq buffer-read-only t)
    (ekg--note-highlight))

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
  (let ((ewoc (ewoc-create #'ekg-display-note (propertize (ekg-tags-display tags) 'face 'ekg-title))))
    (mapc (lambda (note) (ewoc-enter-last ewoc note))
          (funcall notes-func))
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
    (ekg--show-notes (lambda ()
                       (sort 
                        (seq-uniq (mapcan (lambda (tag) (ekg-get-notes-with-tag tag)) tags))
                        (lambda (a b)
                          ;; Sort more tags over fewer tags
                          (or (> (length (ekg-note-tags a)) (length (ekg-note-tags b)))
                              (string< (car (ekg-note-tags a)) (car (ekg-note-tags b))))))) tags)
    (switch-to-buffer buf)))

(defun ekg-show-tags-all (tags)
  "Show notes that contain all TAGS."
  (interactive (list (completing-read-multiple "Tags: " (ekg-tags))))
  (let ((buf (get-buffer-create (format "ekg tags (all): %s" (mapconcat #'identity tags " ")))))
    (set-buffer buf)
    (ekg--show-notes (lambda ()
                       (sort
                        (ekg-get-notes-with-tags tags)
                        (lambda (a b)
                          ;; Sort more tags over fewer tags
                          (or (> (length (ekg-note-tags a)) (length (ekg-note-tags b)))
                              (string< (car (ekg-note-tags a)) (car (ekg-note-tags b))))))) tags)
    (switch-to-buffer buf)))

(defun ekg-show-tag (tag)
  (interactive (list (completing-read "Tag: " (ekg-tags))))
  (let ((buf (get-buffer-create (format "ekg tag: %s" tag))))
    (set-buffer buf)
    (ekg--show-notes (lambda () (ekg-get-notes-with-tag tag)) (list tag))
    (switch-to-buffer buf)))

(defun ekg-show-today ()
  (interactive)
  (ekg-show-tag (car (ekg-date-tag))))

(cl-defstruct ekg-person
  "Data about a person, stored in EKG."
  names emails tags)

(defun ekg-add-people (people)
  "Add PEOPLE, `ekg-person' objects to EKG.
The subject of a person is the shortest email address they have."
  (ekg--connect)
  (emacsql-with-transaction ekg-db
      (mapc (lambda (person)
              (let* ((sorted-email (sort (ekg-person-emails person)
                                         (lambda (a b)
                                           (< (length a) (length b)))))
                     (subject (car sorted-email)))
                (triples-set-type ekg-db subject 'named
                                  :name (ekg-person-names person))
                (triples-set-type ekg-db subject 'email
                                  :address sorted-email)
                (triples-set-type ekg-db subject 'tagged
                                  :tag (ekg-person-tags person))
                (triples-set-type ekg-db subject 'person)))
            people)))

(defun ekg-people ()
  "Return a list of all people as `ekg-person' structs."
  (ekg--connect)
  (cl-loop for id in (triples-subjects-of-type ekg-db 'person)
           collect
           (let ((v (triples-get-subject ekg-db id)))
             (make-ekg-person
              :names (plist-get v :name)
              :emails (plist-get v :email/address)
              :tags (plist-get v :tagged/tag)))))

;; Auto-tag functions

(defun ekg-date-tag ()
  "Get single tag representing the date as a ISO 8601 format."
  (list (format-time-string "%F")))

;; Links for org-mode
(require 'ol)

(defun ekg--store-link ()
  "Store a link to this page."
  (when (eq major-mode 'ekg-notes-mode)
    (org-link-store-props :type "ekg" :link (concat "ekg:" (format "%S" ekg-notes-tags))
                          :description (format "EKG page for tags: %s"
                                               (mapconcat #'identity ekg-notes-tags ", ")))))

(defun ekg--open-link (stags)
  "Open a link to an ekg page given by TAGS."
  (let ((tags (read stags)))
    (if (= 1 (length tags))
        (ekg-show-tag (car tags))
      (ekg-show-tags-any tags))))

(org-link-set-parameters "ekg" :follow #'ekg--open-link
                         :store #'ekg--store-link)

(provide 'ekg)

;;; ekg.el ends here
