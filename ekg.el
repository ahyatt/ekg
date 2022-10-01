;; -*- lexical-binding: t -*-
;;; ekg.el --- A system for recording and linking information in emacs.

;;; Commentary:
;;

(require 'triples)
(require 'seq)
(require 's)
(require 'ewoc)
(require 'cl-macs)

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
Return a list of tags to add.")

(defface ekg-title
  '((((type graphic)) :height 2.0 :box t :inherit hl-line)
    (((type tty))) :underline t :inherit hl-line)
  "Face shown for EKG titles")

(defface ekg-tag
  '((((type graphic)) :height 1.0 :box t :inherit default)
    (((type tty))) :underline t :inherit hl-line)
  "Face shown for EKG tags")

(defvar ekg-db-file "~/.emacs.d/ekg.db"
  "Location of DB file used by EKG.")

(defvar ekg-db nil
  "Live sqlite database connection.")

(cl-defstruct ekg-note
  id text mode tags creation-time modified-time)

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
  (triples-add-schema ekg-db 'person))

(defun ekg-save-note (note)
  "Save NOTE in database, replacing note information there."
  (ekg--connect)
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
    (mapc (lambda (tag) (triples-set-type ekg-db tag 'tag)) (ekg-note-tags note))))

(defun ekg-get-notes-with-tag (tag)
  "Get all notes with TAG, returning a list of `ekg-note' struct."
  (ekg--connect)
  (cl-loop for id in (plist-get (triples-get-type ekg-db tag 'tag) :tagged)
           collect
           (let ((v (triples-get-subject ekg-db id)))
             (make-ekg-note :id id
                            :text (plist-get v :text/text)
                            :mode (plist-get v :text/mode)
                            :tags (plist-get v :tagged/tag)
                            :creation-time (plist-get v :time-tracked/creation-time)
                            :modified-time (plist-get v :time-tracked/modified-time)))))

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
  :interactive nil)

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

(defun ekg-edit-display-tags ()
  "Create or edit the overlay to show tags."
  (let ((os
         (or (seq-filter
              (lambda (o) (eq 'ekg-tag (overlay-get o 'category)))
              (overlays-in (point-min) (point-max)))
             (list (make-overlay (point-min) (point-max))))))
    (mapc (lambda (o)
            (overlay-put o 'after-string (ekg-tags-display (ekg-note-tags ekg-note)))) os)))

(defun ekg-capture (&optional tags)
  "Capture a new note, with at least TAGS."
  (interactive)
  (let ((buf (get-buffer-create "*EKG Capture*")))
    (set-buffer buf)
    (funcall ekg-capture-default-mode)
    (setq-local header-line-format
                (substitute-command-keys
                 "\\<ekg-capture-mode-map>Capture buffer.  Finish \
`\\[ekg-capture-finalize]'."))
    (ekg-capture-mode 1)
    (let* ((time (time-convert (current-time) 'integer))
           (subject (sxhash (cons time (random 100)))))
      (setq ekg-note
            (ekg-note-create nil ekg-capture-default-mode
                             (seq-uniq (append
                                        tags
                                        (mapcan (lambda (f) (funcall f)) ekg-capture-auto-tag-funcs))))))
    (open-line 1)
    (ekg-edit-display-tags)
    (switch-to-buffer-other-window buf)))

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
    (insert (ekg-note-text note))
    (when (ekg-note-mode note)
      (funcall (ekg-note-mode note)))
    (setq-local header-line-format
                (substitute-command-keys
                 "\\<ekg-edit-mode-map>Capture buffer.  Finish \
`\\[ekg-edit-finalize]'."))
    (ekg-edit-mode 1)
    (setq ekg-note note)
    (unless (= (point-min) (point-max)) (open-line 1))
    (let ((o (make-overlay (point-min) (point-max))))
      (overlay-put o 'after-string (ekg-tags-display (ekg-note-tags ekg-note))))
    (switch-to-buffer-other-window buf)))

(defun ekg--save-note-in-buffer ()
  "Save the current note.
Return the latest `ekg-note' object."
  (ekg--connect)
  (setf (ekg-note-text ekg-note) (buffer-string)
        (ekg-note-mode ekg-note) major-mode
        (ekg-note-tags ekg-note) (seq-uniq (ekg-note-tags ekg-note)))
  (ekg-save-note ekg-note)
  ekg-note)

(defun ekg-capture-edit-remove-tags (tags)
  "Remove TAGS from the current list of tags."
  (interactive (list (completing-read-multiple "Remove tags: " (ekg-note-tags ekg-note)
                                               nil t)) ekg-capture-mode ekg-edit-mode)
  (setf (ekg-note-tags ekg-note) (seq-difference tags (ekg-note-tags ekg-note) #'equal))
  (ekg-edit-display-tags))

(defun ekg-capture-edit-add-tags (tags)
  "Add TAGS to edit or capture buffer."
  (interactive (list (completing-read-multiple "Additional tags: "
                                               (seq-difference
                                                (ekg-tags) (ekg-note-tags ekg-note))))
               ekg-capture-mode ekg-edit-mode)
  (setf (ekg-note-tags ekg-note) (append tags (ekg-note-tags ekg-note)))
  (ekg-edit-display-tags))

(defun ekg-edit-finalize ()
  "Save the edited note and refresh where it appears."
  (interactive nil ekg-edit-mode)
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

(defun ekg-capture-finalize ()
  "Save the current note."
  (interactive nil ekg-capture-mode)
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

(defvar-keymap ekg-notes-mode-map
  :full nil
  "a" #'ekg-notes-any-tags
  "c" #'ekg-notes-create
  "d" #'ekg-notes-delete
  "g" #'ekg-notes-refresh
  "n" #'ekg-notes-next
  "o" #'ekg-notes-open
  "p" #'ekg-notes-previous
  "r" #'ekg-notes-remove
  "t" #'ekg-notes-tag)

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
  (let ((note (ekg--current-note-or-error)))
    (when (y-or-n-p "Or you sure you want to delete this note?")
      (ekg-note-delete note)
      (setq buffer-read-only nil)
      (ewoc-delete ekg-notes-ewoc (ewoc-locate ekg-notes-ewoc))
      (setq buffer-read-only t)
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
                          ;; Sort more tags over fewer TAGS
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
