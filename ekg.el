;;; ekg.el --- A system for recording and linking information -*- lexical-binding: t -*-

;; Copyright (c) 2022-2025  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Package-Requires: ((triples "0.5.1") (emacs "28.1") (llm "0.18.0"))
;; Keywords: outlines, hypermedia
;; Version: 0.7.1
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
;; EKG is a note-taking and information storing application, centered around
;; tags, but with the ability to have other note metadata.

(require 'triples)
(require 'triples-backups)
(require 'triples-upgrade)
(require 'triples-fts)
(require 'seq)
(require 'ewoc)
(require 'cl-lib)
(require 'map)
(require 'ffap)
(require 'hl-line)
(require 'iso8601)
(require 'url-parse)

(declare-function org-open-at-point "org")
(declare-function org-redisplay-inline-images "org")
(declare-function org-activate-links "org")
(declare-function org-in-regexp "org")
(declare-function markdown-follow-thing-at-point "markdown-mode")
(declare-function markdown-wiki-link-p "markdown-mode")
(declare-function markdown-wiki-link-link "markdown-mode")

;;; Code:

(defgroup ekg nil
  "The Emacs knowledge graph, an app for notes and structured data."
  :group 'applications)

(defcustom ekg-capture-default-mode 'org-mode
  "The default mode for all new notes."
  :type 'symbol
  :group 'ekg)

(defcustom ekg-acceptable-modes '(org-mode markdown-mode text-mode)
  "Modes that make sense to use as note types."
  :type '(set symbol)
  :group 'ekg)

(defcustom ekg-capture-auto-tag-funcs '(ekg-date-tag)
  "Functions to run to create tags automatically.
The functions are run in the note buffer while creating it.
Return a list of tags to add."
  :type '(set function)
  :group 'ekg)

(defcustom ekg-format-funcs '()
  "Functions to run to format what we display to the user.
What we display on the UI is different than what we store
internally, which is the document itself.  Each function here
will be run in sequence on a temporary buffer, and is responsible
for making the changes in the current buffer to affect the
formatting however it wants.  It is the responsibility of each
function to check for the mode of the buffer."
  :type '(set function)
  :group 'ekg)

(defcustom ekg-linkify-inline-tags t
  "When non-nil, turn inline tags into links to ekg tags.
This will also make sure to detect the links when detecting tags to add
when saving notes."
  :type 'boolean
  :group 'ekg)

(defcustom ekg-notes-size 20
  "How many recently created or updated notes to show."
  :type 'integer
  :group 'ekg)

(defcustom ekg-db-file nil
  "The filename for the ekg database.
Initially set as nil, which will mean that we use
`triples-default-database-filename'.  If you don't want to do
that use this, set to the filename you want to use.  If the file
named by `ekg-db-file-obsolete' exists, that is used instead."
  :type 'file
  :group 'ekg)

(defcustom ekg-template-tag "template"
  "Special tag marking notes acting as templates for other tags.
See `ekg-on-add-tag-insert-template' for details on how this works."
  :type '(string :tag "tag")
  :group 'ekg)

(defcustom ekg-function-tag "tag-defun"
  "Special tag marking notes with elisp code run for other tags.
This takes effect on the note buffer when editing or capturing,
and will take effect for all other tags on the same note that
holds this tag."
  :type '(string :tag "tag")
  :group 'ekg)

(defcustom ekg-trash-tag "trash"
  "The tag that is used to mark a note as a trashed noted."
  :type '(string :tag "tag")
  :group 'ekg)

(defcustom ekg-draft-tag "draft"
  "The tag that is used to mark a note as a draft.
If this is nil, then there is no distinction between a draft and
a saved note.  This may mean that saving a note in progress may
take longer, as more processing will be run on it."
  :type '(string :tag "tag")
  :group 'ekg)

(defcustom ekg-note-inline-max-words 500
  "How many words to display for inlines if the caller does not specify."
  :type 'integer
  :group 'ekg)

(defcustom ekg-truncation-method 'word
  "Method used for truncating text.
Possible values are:
- 'word: Truncate text based on word count (default).
- 'character: Truncate text based on character count.
This affects functions like `ekg-truncate-at` and text selection for embeddings."
  :type '(choice (const :tag "Word-based" 'word)
                 (const :tag "Character-based" 'character))
  :group 'ekg)

(defcustom ekg-display-note-template "%n(id)%n(tagged)%n(titled)%n(text 500)%n(other)"
  "Template for displaying notes in notes buffers.
This follows normal templating rules, but it is most likely the
user is interested in the various %n templates that correspond to
the types that the note can have.  An exception is the %n(other)
inline, which displays all other types that are displayable, but
not in the template.

Most formatters can take a maximum number of words to output, and a
format list, which is a list of desired output properties as symbols.
Right now, the possibilities are:
  - `oneline', the results must be on one line only.
  - `plaintext', the results must not have string properties.

For OTHER formatters, everything must
be standardized to take just the number of words and output properties,
which are applied to each non-standard property in turn."
  :type 'string
  :group 'ekg)

(defcustom ekg-oneliner-note-template "%n(tagged 20 oneline) %n(titled 20 oneline) %n(text 40 oneline)"
  "Template for displaying one-line notes in a note buffer.
Otherwise the same format as `ekg-display-note-template'")

(defcustom ekg-metadata-separator-text "--text follows this line--"
  "Separator between the metadata of the note and the note text."
  :type 'string
  :group 'ekg)

(defcustom ekg-notes-display-images t
  "Whether images are displayed by default."
  :type 'boolean
  :group 'ekg)

(defcustom ekg-save-no-message nil
  "Non-nil means do not print any message when saving."
  :type 'boolean
  :group 'ekg)

(defcustom ekg-search-max-results 20
  "Maximum number of results to show in the `ekg-search' preview."
  :type 'integer
  :group 'ekg)

(defcustom ekg-confirm-on-buffer-kill nil
  "Non-nil if the user should confirm before killing a note buffer.
The affects killing both the capture and edit buffer, only when
there are unsaved changes."
  :type 'boolean
  :group 'ekg)

(defcustom ekg-inline-custom-tag-completion-symbols '((?@ . "person")
                                                      (?! . "idea"))
  "A map of custom symbols to tag prefixes.
The character in the car of the aliast will be used to trigger
completion, and complete tags with the prefix of the cdr.  The
character `#' will always be used to complete arbitrary tags, and
should not appear here."
  :type '(alist :key-type character :value-type string)
  :group 'ekg)

(defcustom ekg-inline-populate-inline-text-tags t
  "Whether to detect and link inline tags out of text tags.
This happens when saving notes, which, if this is non-nil, will
look for various kinds of tags in the text, with the `#' or
prefixes in `ekg-inline-custom-tag-completion-symbols', and make
links out of them, as well as adding them to the note text."
  :type 'boolean
  :group 'ekg)

(defcustom ekg-command-regex-for-narrowing '("^org-insert" "org-meta-return")
  "A list of regex for commands which need a narrowed buffer."
  :type '(repeat string)
  :group 'ekg)

(defconst ekg-db-file-obsolete (file-name-concat user-emacs-directory "ekg.db")
  "The original database name that ekg started with.")

(defconst ekg-default-num-backups 5
  "The number of backups to set when first using the database.
This can be overwritten by other database users, and will not be
set again.  If you want to change the number of backups in your
database after it has been created, run `triples-backups-setup'.")

(defconst ekg-default-backups-strategy 'daily
  "The default database backup strategy when first setting up the database.
This can be overwritten by other database users, and
will not be set again.  If you want to change the number of
backups in your database after it has been created, run
`triples-backups-setup'.")

(defface ekg-notes-mode-title
  '((((type graphic)) :height 2.0 :box t)
    (((type tty))) :underline t)
  "Face shown for the titles of EKG notes mode.")

(defface ekg-tag
  '((((type graphic)) :height 1.0 :box t)
    (((type tty))) :underline t)
  "Face shown for EKG tags.")

(defface ekg-title
  '((((type graphic)) :height 1.2 :underline t)
    (((type tty))) :underline t)
  "Face shown for EKG titles.")

(defface ekg-resource
  '((((type graphic)) :inherit fixed-pitch)
    (((type tty))) :underline t)
  "Face shown for EKG resource.")

(defface ekg-metadata
  '((default :inherit default :weight bold))
  "Face shown for the metadata section of notes.")

(defvar ekg-db nil
  "Live sqlite database connection.")

(defvar ekg-metadata-parsers '(("Tags" . ekg--metadata-update-tag)
                               ("Resource" . ekg--metadata-update-resource)
                               ("Title" . ekg--metadata-update-title))
  "Functions that update a note from the buffer's metadata text.
Each function takes its field's property value and updates the
buffer's `ekg-note' with the results of parsing that value.  The
function takes one argument, a list of the field metadata
property values for multivalue types, or a single one for single
value types.  If `ekg-property-multivalue-type' has an entry, it
is a multivalue type.")

(defconst ekg-property-multivalue-type '(("Tags" . comma)
                                         ("Title" . line))
  "Defines per typehow multiple values are separated.
The values are symbols, COMMA means a comma-separated value.
LINE means each value gets its own property line.")

(defvar ekg-metadata-labels '((:titled/title . "Title"))
  "Alist of properties that can be on the note and their labels.
The label needs to match the keys in the `ekg-metadata-parsers' alist.")

(defvar ekg-add-schema-hook nil
  "Hook run when ensuring schema for ekg.
This is run on connection.  Calls to `triples-add-schema' are
idempotent, so it's recommended to run them without checking if
the schema already exists.")

(defvar ekg-note-pre-save-hook nil
  "Hook run before saving a note.
This is not run in the same database transaction as the save.
All functions are called with the `ekg-note' as the single
argument.")

(defvar ekg-note-save-hook nil
  "Hook run after saving a note, within the save transaction.
At this point the note itself has been saved.  All functions are
called with the `ekg-note' as the single argument.")

(defvar ekg-note-pre-delete-hook nil
  "Hook run before deleting a note.
This is not run in the same database transaction as the delete.
All functions are called with the ID of the note that is being
deleted as the single argument.")

(defvar ekg-note-delete-hook nil
  "Hook run after deleting a note.
This is run in the same transaction as the deletion.  All
functions are called with the ID of the note that is being
deleted as the single argument.")

(defvar ekg-note-add-tag-hook '(ekg-on-add-tag-insert-template)
  "Hook run after a new tag is added to a note.
This includes new notes that start with tags.  All functions are
called with the tag as the single argument, and run in the buffer
editing the note.")

(defvar ekg-query-pred-abbrevs '(("tag" . "tagged/tag")
                                 ("title" . "titled/title")
                                 ("text" . "text/text"))
  "Abbreviations for predicates in queries.")

(defconst ekg-version "0.7.1"
  "The version of ekg.

This is used to understand when the database needs upgrading.")

(cl-defstruct ekg-note
  id text mode tags creation-time modified-time properties inlines)

(cl-defstruct ekg-inline pos command type)

(defun ekg-db-file ()
  "Return the database file we should use in ekg.
If `ekg-db-file' is nil and `ekg-db-file-obsolete' exists, use
`ekg-db-file-obsolete', otherwise use `ekg-db-file'.  If that is
non-nil, it will be used as the filename, otherwise
`triples-default-database-filename' is used."
  (if (and (null ekg-db-file) (file-exists-p ekg-db-file-obsolete))
      ekg-db-file-obsolete
    ekg-db-file))

(defun ekg--notes-directory ()
  "Return the directory ekg notes buffers have as the default directory.
This will be the location of the database file."
  (file-name-directory
   (or (ekg-db-file)
       triples-default-database-filename)))

;; `ekg-connect' will do things that might themselves call `ekg-connect', so we
;; need to protect against an infinite recursion.
(defalias 'ekg-connect
  (let ((ekg--in-connect-call))
    (lambda ()
      (unless ekg--in-connect-call
        (setq ekg--in-connect-call t)
        (unwind-protect
            (progn (unless ekg-db
                     (setq ekg-db (triples-connect (ekg-db-file))))
                   (let ((ekg-plist (triples-get-type ekg-db 'ekg 'ekg)))
                     (when (or (null (plist-get ekg-plist :version))
                               (version-list-< (plist-get ekg-plist :version) (version-to-list ekg-version)))
                       (ekg-add-schema)
                       (ekg-upgrade-db (plist-get ekg-plist :version))
                       (triples-set-type ekg-db 'ekg 'ekg :version (version-to-list ekg-version))))
                   (unless (triples-backups-configuration ekg-db)
                     (triples-backups-setup ekg-db ekg-default-num-backups
                                            ekg-default-backups-strategy)))
          (setq ekg--in-connect-call nil)))))
  "Ensure EKG-DB is connected.
Also make sure the database is set up correctly. This should be
called before any access to triples, unless we are sure all
callers have already called this function")

(defun ekg-close ()
  "Close the EKG-DB connection."
  (interactive)
  (when ekg-db
    (triples-close ekg-db)
    (setq ekg-db nil)))

(defun ekg-add-schema ()
  "Add schema necessary for EKG to function."
  ;; Schema here needs to also be taken care of when deleted in ekg-note-delete
  ;; or ekg-tag-delete.
  (triples-add-schema ekg-db 'tagged '(tag :base/type string))
  (triples-add-schema ekg-db 'text
                      '(text :base/unique t :base/type string)
                      '(mode :base/unique t :base/type symbol)
                      '(inlines :base/virtual-reversed inline/for-text))
  (triples-add-schema ekg-db 'time-tracked
                      '(creation-time :base/unique t :base/type integer)
                      '(modified-time :base/unique t :base/type integer))
  (triples-add-schema ekg-db 'inline
                      '(command :base/unique t :base/type symbol)
                      'args
                      '(pos :base/unique t :base/type integer)
                      '(type :base/unique t :base/type symbol)
                      '(for-text :base/unique t))
  (triples-add-schema ekg-db 'tag
                      '(tagged :base/virtual-reversed tagged/tag))
  ;; A URL can be a subject too, and has data, including the title. The title is
  ;; something that can be used to select the subject via completion.
  (triples-add-schema ekg-db 'titled '(title :base/type string))
  (triples-add-schema ekg-db 'ekg '(version :base/type cons :base/unique t))
  (run-hooks 'ekg-add-schema-hook))

(defvar ekg-schema-text-cotypes '(text tagged time-tracked titled)
  "All the types that are used in the ekg schema on text entities.
Extensions can add to this list, but should not remove from it.
Any type here in considered owned by ekg and will be removed
during note deletion.

These are not guaranteed to be only on text entities, however.")

(defun ekg--generate-id ()
  "Return a unique ID for a note.
This is not suitable for generating a large number of IDs in a
small time frame.  About one ID per second is reasonable."
  (sxhash (cons (time-convert (current-time) 'integer)  (random 100))))

(defun ekg--normalize-tag (tag)
  "Return a normalized version of TAG.
No tag should be input from the user without being normalized
before storage."
  (string-trim (downcase (string-replace "," "" tag))))

(defun ekg--normalize-note (note)
  "Make sure NOTE adheres to ekg-wide constraints before saving.
This
  1) makes sure all tags are lowercase and trimmed.
  2) removes commas in tags, since those are used to separate tags.
  3) removes any properties from the text.
  4) makes sure the note has a reasonable ID, otherwise generates one.

Note: we used to also trim text, but with inline commands, that
is not a great idea, because an inline command sits outside of
the text and may be after trailing whitespace."
  (setf (ekg-note-tags note)
        (mapcar #'ekg--normalize-tag (ekg-note-tags note)))
  (setf (ekg-note-text note)
        (substring-no-properties (ekg-note-text note)))
  (when (or (equal (ekg-note-id note) "") (not (ekg-note-id note)))
    (setf (ekg-note-id note) (ekg--generate-id))))

(defun ekg-backup (&optional force)
  "Backup the database.

FORCE, if non-nil, will make sure a backup is stored, regardless
of the settings of max-backups.  Otherwise, a backup is just made
if it is time for one, according to the settings in
`ekg-default-num-backups' and `ekg-default-backups-strategy'."
  (condition-case err
      (if force
          (triples-backup ekg-db ekg-db-file most-positive-fixnum)
        (triples-backups-maybe-backup ekg-db (ekg-db-file)))
    (file-missing
     (let ((msg "Could not backup database, perhaps because of missing sqlite3 executable. Ensure the executable exists at the location specified by `triples-sqlite-executable' or `emacsql-sqlite-executable': %s"))
       ;; If we are forcing the backup, this error probably is serious and
       ;; should be investigated.
       (if force
           (error msg (cdr err))
         (lwarn :error 'ekg msg (cdr err)))))))

(defun ekg-save-note (note)
  "Save NOTE in database, replacing note information there."
  (ekg-connect)
  (ekg--normalize-note note)
  (when (and (eq (ekg-note-mode note) 'org-mode) ekg-linkify-inline-tags)
    (ekg--convert-inline-tags-to-links note))
  (ekg--populate-inline-tags note)
  (run-hook-with-args 'ekg-note-pre-save-hook note)
  (triples-with-transaction
    ekg-db
    (triples-set-type ekg-db (ekg-note-id note) 'tagged :tag (ekg-note-tags note))
    (triples-set-type ekg-db (ekg-note-id note) 'text
                      :text (ekg-note-text note)
                      :mode (ekg-note-mode note))
    ;; Delete any previous linked inlines.
    (cl-loop for inline-id in (triples-subjects-with-predicate-object
                               ekg-db 'inline/for-text (ekg-note-id note))
             do (triples-remove-type ekg-db inline-id 'inline))
    ;; Now store the new inlines.
    (cl-loop for inline in (ekg-note-inlines note) do
             (triples-set-type ekg-db (format "%S/pos:%d" (ekg-note-id note)
                                              (ekg-inline-pos inline))
                               'inline
                               :command (car (ekg-inline-command inline))
                               :args (cdr (ekg-inline-command inline))
                               :pos (ekg-inline-pos inline)
                               :for-text (ekg-note-id note)
                               :type (ekg-inline-type inline)))
    ;; Note that we recalculate modified time here, since we are modifying the
    ;; entity.
    (let ((modified-time (time-convert (current-time) 'integer)))
      (triples-set-type ekg-db (ekg-note-id note) 'time-tracked
                        :creation-time (ekg-note-creation-time note)
                        :modified-time modified-time)
      (setf (ekg-note-modified-time note) modified-time))
    (mapc (lambda (tag) (triples-set-type ekg-db tag 'tag)) (ekg-note-tags note))
    (apply #'triples-set-types ekg-db (ekg-note-id note) (ekg-note-properties note))
    ;; For any properties that no longer have a value, delete the type.  Iterate
    ;; over the plist, and for each key, if the value is nil, delete the type.
    (let ((empty-types)
          (nonempty-types))
      (cl-loop for (key value) on (ekg-note-properties note) by #'cddr do
               (push (car (triples-combined-to-type-and-prop key))
                     (if value nonempty-types empty-types)))
      (mapc (lambda (type) (triples-remove-type ekg-db (ekg-note-id note) type))
            (seq-difference empty-types nonempty-types)))
    (run-hook-with-args 'ekg-note-save-hook note))
  (ekg-backup)
  (set-buffer-modified-p nil))

(defun ekg-get-notes-with-any-tags (tags)
  "Get all notes with any of the tags in TAGS.
This returns notes that have any of the tags in TAGS.  Special
tags are not returned.

The notes returtned are sorted in reverse chronological order."
  (ekg-connect)
  (sort
   (seq-uniq (mapcan (lambda (tag) (ekg-get-notes-with-tag tag))
                     (seq-difference tags (list ekg-draft-tag
                                                ekg-trash-tag
                                                ekg-function-tag))))
   #'ekg-sort-by-creation-time))

(defun ekg-get-notes-with-tags (tags)
  "Get all notes with TAGS, returning a list of `ekg-note' structs.
This returns only notes that have all the tags in TAGS.
Draft notes are not returned, unless TAGS contains the draft tag."
  (ekg-connect)
  (let ((ids-by-tag
         (mapcar (lambda (tag)
                   (plist-get (triples-get-type ekg-db tag 'tag) :tagged))
                 tags)))
    (seq-filter
     (lambda (note)
       ;; Remove draft and trash notes unless they are specifically requested.
       (not
        (or (and (not (member ekg-draft-tag tags))
                 (member ekg-draft-tag (ekg-note-tags note)))
            (and (not (member ekg-trash-tag tags))
                 (member ekg-trash-tag (ekg-note-tags note))))))
     (mapcar #'ekg-get-note-with-id
             (seq-reduce #'seq-intersection
                         ids-by-tag
                         (car ids-by-tag))))))

(defun ekg-get-notes-with-tag (tag)
  "Get all notes with TAG, returning a list of `ekg-note' structs."
  (ekg-get-notes-with-tags (list tag)))

(defun ekg-note-with-id-exists-p (id)
  "Return non-nil if a note with ID exists."
  (ekg-connect)
  (triples-get-subject ekg-db id))

(defun ekg-get-note-with-id (id)
  "Get the specific note with ID.
If the ID does not exist, create a new note with that ID."
  (ekg-connect)
  (let* ((v (triples-get-subject ekg-db id))
         (inlines (mapcar (lambda (iid)
                            (let ((iv (triples-get-type ekg-db iid
                                                        'inline)))
                              (make-ekg-inline :pos (plist-get iv :pos)
                                               :command (cons
                                                         (plist-get iv :command)
                                                         (plist-get iv :args))
                                               :type (plist-get iv :type))))
                          (plist-get v :text/inlines))))
    (make-ekg-note :id id
                   :text (plist-get v :text/text)
                   :mode (plist-get v :text/mode)
                   :inlines inlines
                   :tags (plist-get v :tagged/tag)
                   :creation-time (plist-get v :time-tracked/creation-time)
                   :modified-time (plist-get v :time-tracked/modified-time)
                   :properties (map-into
                                (map-filter
                                 (lambda (plist-key _)
                                   (not (member plist-key
                                                '(:text/text
                                                  :text/mode
                                                  :text/inlines
                                                  :tagged/tag
                                                  :time-tracked/creation-time
                                                  :time-tracked/modified-time)))) v)
                                'plist))))

(defun ekg-get-notes-with-title (title)
  "Get a list of note structs with TITLE."
  (ekg-connect)
  (mapcar #'ekg-get-note-with-id (triples-subjects-with-predicate-object ekg-db 'titled/title title)))

(defun ekg-note-delete (note)
  "Delete NOTE from the database."
  (ekg-note-delete-by-id (ekg-note-id note)))

(defun ekg-note-delete-by-id (id)
  "Delete all note data associated with ID."
  (ekg-connect)
  (run-hook-with-args 'ekg-note-pre-delete-hook id)
  (triples-with-transaction
    ekg-db
    (cl-loop for type in ekg-schema-text-cotypes do
             (triples-remove-type ekg-db id type))
    (cl-loop for inline-id in (triples-subjects-with-predicate-object
                               ekg-db 'inline/for-text id)
             do (triples-remove-type ekg-db inline-id 'inline))
    (run-hook-with-args 'ekg-note-delete-hook id)))

(defun ekg-tag-delete (tag)
  "Delete all tag data associated with TAG."
  (ekg-connect)
  (triples-remove-type ekg-db tag 'tag))

(defun ekg-note-trash (note)
  "Add the trash tag to NOTE."
  (ekg-connect)
  (if (member ekg-trash-tag (ekg-note-tags note))
      (ekg-note-delete note)
    (triples-with-transaction
      ekg-db
      (push ekg-trash-tag (ekg-note-tags note))
      (ekg-save-note note)))
  (ekg-backup))

(defun ekg-content-tag-p (tag)
  "Return non-nil if TAG represents user content.
This is opposed to tags that are used for internal purposes."
  (not (member tag
               (list ekg-draft-tag ekg-template-tag ekg-function-tag ekg-trash-tag))))

(defun ekg-note-active-p (note)
  "Return non-nil if NOTE is active.
This is similar to `ekg-active-id-p', but takes a note, which may
be unsaved."
  (not (seq-intersection (list ekg-draft-tag ekg-trash-tag)
                         (ekg-note-tags note))))

(defun ekg-note-is-content-p (note)
  "Return non-nil if NOTE has no control-type tags.

Those tags are things such as `ekg-draft-tag', or `ekg-function-tag'."
  (seq-every-p #'ekg-content-tag-p (ekg-note-tags note)))

(defun ekg-active-id-p (id)
  "Return non-nil if the note with ID is active.
This will return true if the note is not a draft, and has at
least one non-trash tag."
  (ekg-connect)
  (ekg-note-active-p (ekg-get-note-with-id id)))

(defun ekg-live-id-p (sub)
  "Return non-nil if SUB represents an undeleted note."
  (ekg-connect)
  (not (seq-some (lambda (tag) (equal tag ekg-trash-tag)) (plist-get (triples-get-type ekg-db sub 'tagged) :tag))))

(defun ekg-extract-inlines (text)
  "Return a cons of TEXT without inline commands, and the commands.
The commands returned are the most specific type of struct known,
or, if unknown, `ekg-inline'."
  (let ((inlines)
        (newtext text)
        (inline-rx (rx (seq (group-n 1 "%"
                                     (group-n 2 (zero-or-one "n"))
                                     (literal "(")
                                     (group-n 3 (*? anychar))
                                     (literal ")"))))))
    ;; Keep removing commands left to right to make sure our positions are
    ;; without commands, since that's how they will need to be inserted.
    (while (when-let (index (string-match inline-rx newtext))
             (push (make-ekg-inline :pos index
                                    :command (read (format "(%s)" (match-string 3 newtext)))
                                    :type (pcase (match-string 2 newtext)
                                            ("" 'command)
                                            ("n" 'note)))
                   inlines)
             (setq newtext (replace-match "" nil nil newtext 1))))
    (cons newtext (nreverse inlines))))

(defun ekg-truncate-at (s num)
  "Return S truncated, with ellipses.
Truncation method depends on `ekg-truncation-method`.
If `ekg-truncation-method` is 'word, NUM is the number of words.
If `ekg-truncation-method` is 'character, NUM is the number of characters.
If NUM is greater than the number of words/characters of S, return S
unchanged."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min)) ; Changed from 0 to (point-min) for clarity
    (cond
     ((eq ekg-truncation-method 'word)
      (cl-loop with i = 0 while (and (< i num)
                                     (forward-word 1))
               do (cl-incf i)))
     ((eq ekg-truncation-method 'character)
      (goto-char (min (point-max) (+ (point-min) num)))))
    (when (< (point) (point-max))
      (insert "…")
      (delete-region (point) (point-max)))
    (buffer-string)))


(defun ekg-insert-inlines-and-process (text inlines func)
  "Return the result of inserting INLINES into TEXT.
FUNC is executed with the cdr of each inline command and its
return value is inserted into the buffer at the appropriate
point.  NUMTOK is the number of tokens available to be used."
  (with-temp-buffer
    (insert text)
    (let ((mils (cl-loop for il in inlines do
                         (goto-char (+ (ekg-inline-pos il) 1))
                         collect (cons (point-marker)
                                       (condition-case err
                                           (funcall func il)
                                         (error
                                          (propertize
                                           (format "Error executing inline command %s: %s"
                                                   (ekg-inline-to-text il)
                                                   (error-message-string err))
                                           'face 'error)))))))
      (cl-loop for mil in mils do
               (when (cdr mil)
                 (goto-char (car mil))
                 (insert-before-markers (cdr mil)))))
    (buffer-string)))

(defun ekg-inline-to-text (inline)
  "Return the text representation of INLINE."
  (format "%%%s%S"
          (if (eq 'note (ekg-inline-type inline))
              "n" "") (ekg-inline-command inline)))

(defun ekg-insert-inlines-representation (text inlines)
  "Return the result of inserting INLINES into TEXT.
INLINES are inserted in their unevaluated text forms."
  (ekg-insert-inlines-and-process
   text inlines #'ekg-inline-to-text))

(defun ekg-inline-to-result (inline note)
  "Return the result of evaluating INLINE with NOTE as context."
  (let ((f (intern (format
                    (pcase (ekg-inline-type inline)
                      ('command "ekg-inline-command-%s")
                      ('note "ekg-display-note-%s")
                      (_ (error "Unknown inline type %s" (ekg-inline-type inline))))
                    (car (ekg-inline-command inline))))))
    (if (fboundp f)
        (pcase (ekg-inline-type inline)
          ('command (apply f (cdr (ekg-inline-command inline))))
          ('note (progn
                   (unless note
                     (error "No note supplied for display inline"))
                   (apply f note (cdr (ekg-inline-command inline))))))
      (format "%%Unknown command %s: `%s' not found"
              (car (ekg-inline-command inline)) (symbol-name f)))))

(defun ekg-insert-inlines-results (text inlines note)
  "Return the results of executing INLINES into TEXT.
NOTE is the `ekg-note' that needs to exist for the `display'
inlines."
  (ekg-insert-inlines-and-process
   text inlines
   (lambda (inline) (ekg-inline-to-result inline note))))

(defun ekg--transclude-titled-note-completion ()
  "Completion function for file transclusion."
  (let ((begin (save-excursion
                 (search-backward ">t" (line-beginning-position) t)
                 (+ 2 (point))))
        (end (point)))
    (when (<= begin end)
      (list begin end
            (completion-table-dynamic (lambda (_)
                                        (mapcar (lambda (title-cons)
                                                  (cons (cdr title-cons)
                                                        (car title-cons)))
                                                (ekg-document-titles))))
            :exclusive t :exit-function #'ekg--transclude-cap-exit))))

(defun ekg--transclude-cap-exit (completion finished)
  "Clean up CAP after COMPLETION.
FINISHED is non-nil if the completion has been finished by the
user."
  (when finished
    (save-excursion
      (let* ((docs (mapcar (lambda (title-cons)
                             (cons (cdr title-cons)
                                   (car title-cons)))
                           (ekg-document-titles)))
             (id (cdr (assoc completion docs #'equal))))
        (unless id (error "No document with title %s" completion))
        (when (search-backward (format ">%s" completion) (line-beginning-position) t)
          (replace-match (format "%%(transclude-note %S)" id)))))))

(defun ekg-display-note-id (note &optional force)
  "Show the id of NOTE, if it is interesting.
Interesting is defined by whether it has meaning in itself.
However, if FORCE is non-nil, it will be shown regardless."
  (if (or force
          (ekg-should-show-id-p (ekg-note-id note)))
      (propertize
       (format "[%s]\n" (ekg-note-id note))
       'face 'ekg-resource)
    ""))

(defun ekg-display--format (text numwords format)
  "Format TEXT, only displaying NUMWORDS.

FORMAT is a list of formatting options.  See `ekg-display-note-template'
for details."
  (let ((unformatted (concat (string-trim-right
                              (ekg-truncate-at text
                                               (or numwords ekg-note-inline-max-words))) "\n")))
    (dolist (f format)
      (pcase f
        ('oneline (setq unformatted (string-replace "\n" " " unformatted)))
        ('plaintext (setq unformatted (substring-no-properties unformatted)))))
    unformatted))

(defun ekg-display-note-text (note &optional numwords &rest format)
  "Return text, with mode-specific properties, of NOTE.

NUMWORDS and FORMAT are standard options, see
`ekg-display-note-template' for details."
  (with-temp-buffer
    (when (ekg-note-text note)
      (insert (ekg-insert-inlines-results
               (ekg-note-text note)
               (ekg-note-inlines note)
               note)))
    (when (and (not 'plaintext) (ekg-note-mode note))
      (let ((mode-func (intern (format "%s-mode" (ekg-note-mode note)))))
        (if (fboundp mode-func) (funcall mode-func)
          (funcall (ekg-note-mode note)))))
    (mapc #'funcall ekg-format-funcs)
    (unless (member 'plaintext format)
      (font-lock-ensure)
      (put-text-property (point-min) (point-max) 'ekg-note-id (ekg-note-id note)))
    (ekg-display--format (buffer-string) numwords format)))

(defun ekg-display-note-tagged (note &optional numwords &rest format)
  "Return text of the tags of NOTE.
NUMWORDS and FORMAT are as described in `ekg-display-note-template'."
  (ekg-display--format
   (concat (mapconcat (lambda (tag) (propertize tag 'face 'ekg-tag))
                      (ekg-note-tags note) " ") "\n")
   numwords format))

(defun ekg-display-note-time-tracked (note &optional format-str &rest format)
  "Return text of the times NOTE was created and modified.

FORMAT-STR controls how the time is formatted.

FORMAT is the list of
desired output properties as described in `ekg-display-note-template'."
  (let ((format-str (or format-str "%Y-%m-%d")))
    (format "Created: %s   Modified: %s%s"
            (format-time-string format-str (ekg-note-creation-time note))
            (format-time-string format-str (ekg-note-modified-time note))
            (if (member 'oneline format)
                "" "\n"))))

(defun ekg-display-note-titled (note &optional numwords &rest format)
  "Return text of the title of NOTE.
NUMWORDS and FORMAT are as described in `ekg-display-note-template'."
  (ekg-display--format (if-let (titles (plist-get (ekg-note-properties note)
                                                  :titled/title))
                           (propertize (concat (mapconcat #'identity titles ", ") "\n")
                                       'face 'ekg-title)
                         "")
                       numwords format))

(defun ekg-inline-command-transclude-note (id &optional numwords)
  "Return the text of ID.
NUMWORDS is the maximum number of words to use."
  (string-trim-right (ekg-display-note-text (ekg-get-note-with-id id) numwords) "\n"))

(defun ekg-inline-command-transclude-file (file &optional numwords)
  "Return the contents of FILE.
NUMWORDS is the maximum number of words to use."
  (with-temp-buffer
    (insert-file-contents file)
    (ekg-truncate-at (buffer-string) (or numwords ekg-note-inline-max-words))))

(defun ekg-inline-command-transclude-website (url &optional numwords)
  "Return the contents of the URL.
NUMWORDS is the maximum number of words to use."
  (let ((url-buffer (url-retrieve-synchronously url)))
    (with-current-buffer url-buffer
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move) ; skip headers
      (shr-render-region (point) (point-max))
      (ekg-truncate-at (buffer-string) (or numwords ekg-note-inline-max-words)))))

(defun ekg-select-note ()
  "Select a note interactively.
Returns the ID of the note."
  (if (y-or-n-p "Select note by title? ")
      (let* ((title-id-pairs (mapcar (lambda (note) (cons (cdr note) (car note)))
                                     (ekg-document-titles)))
             (selected-title (completing-read "Title: " title-id-pairs nil t)))
        (cdr (assoc selected-title title-id-pairs)))
    (let* ((notes (ekg-get-notes-with-tag
                   (completing-read "Tag: " (ekg-tags) nil t)))
           (completion-pairs (mapcar
                              (lambda (note)
                                (cons (ekg-display-note-text note 10)
                                      note)) notes)))
      (ekg-note-id (cdr (assoc (completing-read "Note: " completion-pairs nil t)
                               completion-pairs))))))

(defun ekg-edit-add-inline ()
  "Add an inline command to the current note."
  (interactive)
  (let ((command (completing-read
                  "Add inline: "
                  (mapcar (lambda (name)
                            (string-remove-prefix "ekg-inline-command-" name))
                          (seq-difference
                           (mapcar #'symbol-name
                                   (apropos-internal "^ekg-inline-command-"))
                           '("ekg-inline-command--cmacro")))))
        (args))
    (pcase command
      ("transclude-note" (setq args (list (ekg-select-note))))
      ("transclude-file" (setq args (list (read-file-name "File: ")))))
    (insert (format "%%%S" (cons (intern command) args)))))

(defun ekg-note-snippet (note &optional max-length)
  "Return a short snippet for NOTE.
The snippet is just the beginning of the text, cut off after
MAX-LENGTH characters, with ellipses afterwards.  If MAX-LENGTH is
not supplied, we use a default of 10."
  (let ((display-length (min (length (ekg-note-text note)) (or max-length 10))))
    (format "%s%s" (substring-no-properties (ekg-note-text note) 0 display-length)
            (if (> (length (ekg-note-text note)) display-length) "…" ""))))

(defun ekg--kill-buffer-query-function ()
  "Action to take after a kill is run on a capture or edit buffer.
If final result returns t, the buffer will be killed.  If it
returns nil, the buffer will be left open.  This always returns t
for saved buffers."
  (or (not ekg-confirm-on-buffer-kill)
      (not (buffer-modified-p))
      (yes-or-no-p "Unsaved changes exist, do you still want to exit? ")))

(defun ekg--markdown-follow-thing-at-point (arg)
  "Follow the thing at point, including ekg wiki links.
ARG is the prefix argument, if used it opens in another window."
  (interactive "P")
  (if (markdown-wiki-link-p)
      (let* ((first-char (string-to-char (markdown-wiki-link-link)))
             (symbol (when (member first-char (mapcar #'car ekg-inline-custom-tag-completion-symbols))
                       first-char)))
        (when arg
          (other-window 1))
        (ekg-show-notes-with-tag (if symbol
                                     (ekg--add-prefix-to-inline-tag
                                      (substring (markdown-wiki-link-link) 1)
                                      (char-to-string symbol))
                                   (markdown-wiki-link-link))))
    (markdown-follow-thing-at-point arg)))

(defun ekg--set-local-variables ()
  "Set some common local variables."
  (setq-local
   completion-at-point-functions
   (append (list #'ekg--capf #'ekg--transclude-titled-note-completion
                 #'ekg--inline-tag-completion)
           completion-at-point-functions)
   kill-buffer-query-functions
   (append (list #'ekg--kill-buffer-query-function)
           kill-buffer-query-functions)
   header-line-format (ekg--header-line-format))
  (add-hook 'pre-command-hook #'ekg-narrow-for-command nil t)
  (add-hook 'post-command-hook #'ekg-unnarrow-for-command nil t)

  (when (eq major-mode 'markdown-mode)
    (setq-local markdown-enable-wiki-links t
                markdown-wiki-link-fontify-missing nil)
    ;; We can't redefine how wiki links are handled, and we shouldn't use
    ;; advice, so instead we create a new function that handles wiki links, and
    ;; use it in keybindings.
    (define-key (current-local-map)
                [remap markdown-follow-thing-at-point]
                #'ekg--markdown-follow-thing-at-point)))

(defvar ekg-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'ekg-capture-finalize)
    (define-key map "\C-c\C-k" #'ekg-capture-abort)
    (define-key map "\C-c#" #'ekg-edit-add-inline)
    (substitute-key-definition #'save-buffer #'ekg-save-draft map global-map)
    map)
  "Key map for `ekg-capture-mode', a minor mode.
This is used when capturing new notes.")

(define-minor-mode ekg-capture-mode
  "Minor mode for simple finish/cancel keybindings."
  :init-value nil
  :lighter " EKG-CAP"
  :interactive nil
  (ekg--set-local-variables))

(defvar ekg-capture-mode-hook nil
  "Hook for `ekg-capture-mode'.")

(defvar ekg-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'ekg-edit-finalize)
    (define-key map "\C-c#" #'ekg-edit-add-inline)
    (substitute-key-definition #'save-buffer #'ekg-edit-save map global-map)
    map)
  "Key map for `ekg-edit-mode', a minor mode.
This is used when editing existing notes.")

(define-minor-mode ekg-edit-mode
  "Minor mode for simple finish/cancel keybindings."
  :init-value nil
  :lighter " EKG-ED"
  :interactive nil)

(defvar-local ekg-note nil
  "Holds the note information for buffers adding or changing notes.")

(defvar-local ekg-note-orig-note nil
  "Holds the original note before edit.")

(defvar-local ekg-note-orig-id nil
  "Holds the original ID (subject) for this note.
This is needed to identify references to refresh when the subject is changed.")

(defvar-local ekg-note-orig-fields nil
  "Holds the fields that were populated when the note was loaded.")

(defvar-local ekg-note-auto-narrowed nil
  "If we are currently in an auto-narrowed state.
This happens when a command is run that likely needs to see a
narrowed part of the buffer.  It is only non-nil when running a
command that needs to be narrowed for it, and it is needed so we
can keep track of whether we need to unnarrow or not.")

(defvar ekg-notes-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "A" #'ekg-notes-any-tags)
    (define-key map "a" #'ekg-notes-any-note-tags)
    (define-key map "c" #'ekg-notes-create)
    (define-key map "d" #'ekg-notes-delete)
    (define-key map "g" #'ekg-notes-refresh)
    (define-key map "n" #'ekg-notes-next)
    (define-key map "o" #'ekg-notes-open)
    (define-key map "b" #'ekg-notes-browse)
    (define-key map "B" #'ekg-notes-select-and-browse-url)
    (define-key map "p" #'ekg-notes-previous)
    (define-key map "t" #'ekg-notes-tag)
    (define-key map "q" #'kill-current-buffer)
    (define-key map "k" #'ekg-notes-kill)
    map))

(define-derived-mode ekg-notes-mode fundamental-mode "ekg-notes"
  "Major mode for showing a list of notes that can be interacted with."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (visual-line-mode 1)
  (if (eq ekg-capture-default-mode 'org-mode)
      (progn
        (require 'org)
        (define-key ekg-notes-mode-map "\C-c\C-o" #'org-open-at-point))))

(defun ekg--header-line-format ()
  "Header line format for the ekg capture or edit buffer."
  (if ekg-capture-mode
      (substitute-command-keys
       "\\<ekg-capture-mode-map>Capture buffer. \
Finish `\\[ekg-capture-finalize]'  \
Save as draft `\\[ekg-save-draft]'  \
Abort and delete draft `\\[ekg-capture-abort]'.")
    (substitute-command-keys
     "\\<ekg-edit-mode-map>Edit buffer. \
Finish `\\[ekg-edit-finalize]'  \
Save `\\[ekg-edit-save]'")))

(defun ekg-narrow-for-command ()
  "Narrow buffer if the command requires it.
This is based on `ekg-command-regex-for-narrowing'."
  (condition-case err
      (when (and
             (not ekg-note-auto-narrowed)
             (seq-some
              (lambda (s) (string-match-p s (symbol-name this-command)))
              ekg-command-regex-for-narrowing))
        (narrow-to-region (1+ (overlay-end (ekg--metadata-overlay)))
                          (point-max))
        ;; execute-extended-command will then call another command, and we need
        ;; to also possibly narrow for that.
        (unless (eq this-command 'execute-extended-command)
          (setq ekg-note-auto-narrowed t)))
    (error (lwarn :error 'ekg "Error narrowing for command: %s"
                  (error-message-string err)))))

(defun ekg-unnarrow-for-command ()
  "Unnarrow the buffer if it was automatically narrowed."
  (condition-case err
      (when ekg-note-auto-narrowed
        (widen)
        (setq ekg-note-auto-narrowed nil))
    (error (lwarn :error 'ekg "Error unnarrowing for command: %s"
                  (error-message-string err)))))

(defun ekg--possible-inline-tags-prefix-regexp ()
  "Return a regexp of the possible inline tag prefixes."
  (format "[%s]" (mapconcat (lambda (c) (format "%c" c))
                            (cons ?# (mapcar #'car ekg-inline-custom-tag-completion-symbols))
                            "")))

(defun ekg--add-prefix-to-inline-tag (tag tag-symbol)
  "Return TAG with the prefix denoted by TAG-SYMBOL.
TAG-SYMBOL is a string.  If TAG-SYMBOL is `#', then there is no
prefix."
  (if (equal tag-symbol "#")
      tag
    (concat (assoc-default (string-to-char tag-symbol)
                           ekg-inline-custom-tag-completion-symbols) "/" tag)))

(defconst ekg--nonlink-tag-regexp
  (rx (seq (group-n 1 (or whitespace line-start ?: ?\( ?\[))
           (group-n 2 (regexp (ekg--possible-inline-tags-prefix-regexp)))
           (= 1 ?\[) (group-n 3 (one-or-more (or (any ?/ ?_ ?-)
                                                 word
                                                 (any "🏻-🏿")
                                                 (char (128000 . 129750))
                                                 whitespace)))
           ?\]))
  "Regexp for detecting inline tags that are not org links.")

(defun ekg--inline-tag-replace-with-org-link (prefix tag-identifier symbol)
  "Replace a match to inline TAG-IDENTIFIER with an org link.
SYMBOL is the prefix to the tag identifier.
PREFIX is the prefix that is before the tag identifier."
  (replace-match
   (concat prefix
           symbol
           (org-link-make-string
            (ekg--link-for-tag
             (ekg--add-prefix-to-inline-tag
              tag-identifier
              symbol))
            tag-identifier))))

(defun ekg--convert-inline-tags-to-links (note)
  "Convert any inline tags of NOTE to links.
This allows the user to create tags that don't exist yet, and
will be turned into links before this note is saved.  The tag
will be added and will exist when `ekg--populate-inline-tags'
runs and the note is saved.

This will always run, so don't call without making sure it is
appropriate first based on the mode and
`ekg-linkify-inline-tags'.

This will only run if `ekg-inline-populate-inline-text-tags' is
non-nil."
  (when ekg-inline-populate-inline-text-tags
    (with-temp-buffer
      (insert (ekg-note-text note))
      (goto-char (point-min))
      (while (re-search-forward ekg--nonlink-tag-regexp nil t)
        (unless (and (eq 'org-mode (ekg-note-mode note))
                     ;; This should result in us detecting any blocks.
                     (save-match-data
                       (defvar org-block-regexp)
                       (org-in-regexp org-block-regexp)))
          (let ((symbol (match-string 2))
                (tag (match-string 3)))
            (ekg--inline-tag-replace-with-org-link (match-string 1) tag symbol))))
      (setf (ekg-note-text note) (buffer-substring-no-properties (point-min) (point-max))))))

(defun ekg--populate-inline-tags (note)
  "Populate tags found in text of NOTE.
Tags are prefixed by a hash symbol or a symbol in
`ekg-inline-custom-tag-completion-symbols', and are enclosed in brackets
if they have more than one word.  They aso can be org or markdown links,
depending on the note mode.  The tags are added to the end of the tag
list."
  (let ((use-links (and (member (ekg-note-mode note) '(org-mode markdown-mode))
                        ekg-linkify-inline-tags)))
    (with-temp-buffer
      (insert (ekg-note-text note))
      (goto-char (point-min))
      (let ((tags))
        (while (re-search-forward
                ;; If the tag has a space or punctuation, it needs to be enclosed
                ;; in brackets.
                (if use-links
                    (cond
                     ((eq (ekg-note-mode note) 'org-mode)
                      (rx (group-n 2 (regexp (ekg--possible-inline-tags-prefix-regexp)))
                          ?\[ ?\[ "ekg-tag:"
                          (group-n 3 (one-or-more (any word whitespace ?_ ?/ ?-)))
                          ?\]))
                     ((eq (ekg-note-mode note) 'markdown-mode)
                      (rx (seq
                           ?\[ ?\[
                           (group-n 2 (? (regexp (ekg--possible-inline-tags-prefix-regexp))))
                           (group-n 3 (one-or-more (any word whitespace ?_ ?/ ?-)))
                           ?\] ?\]))))
                  ekg--nonlink-tag-regexp)
                nil t)
          (let ((symbol (if (= 0 (length (match-string 2)))
                            "#"
                          (match-string 2)))
                (tag (match-string 3)))
            (push (if (and use-links (eq (ekg-note-mode note) 'org-mode)) tag
                    (ekg--add-prefix-to-inline-tag tag symbol))
                  tags)))
        (setf (ekg-note-tags note) (seq-uniq (append (ekg-note-tags note) (nreverse tags))))))))

(defun ekg--inline-tag-completion ()
  "Completion function for tags in notes.
This will tags that are prefixed by a hash symbol.  The tag will
complete, and if the tag has a space, it will be enclosed in
brackets."
  (let* ((begin (progn
                  (save-excursion
                    (let ((pos (search-backward-regexp
                                (rx (seq (group-n 1 (or space line-start))
                                         (group-n 2 (regexp (ekg--possible-inline-tags-prefix-regexp)))))
                                (save-excursion
                                  (or (search-backward " " (line-beginning-position) t) (line-beginning-position))) t)))
                      (when pos (+ pos (1+ (length (match-string 1)))))))))
         (end (point))
         (type (match-string-no-properties 2)))
    (when (and begin (<= begin end))
      (list begin end
            (completion-table-dynamic (lambda (_)
                                        (if (equal type "#")
                                            (ekg-tags)
                                          (let ((prefix (concat (assoc-default (string-to-char type) ekg-inline-custom-tag-completion-symbols) "/")))
                                            (mapcar (lambda (tag) (substring-no-properties tag (length prefix)))
                                                    (ekg-tags-with-prefix prefix))))))
            :exclusive nil :exit-function #'ekg--inline-tag-exit))))

(defun ekg--inline-tag-exit (completion finished)
  "When a tag is completed, add brackets.
COMPLETION is the completion chosen.
FINISHED is true if the completion has been finished."
  (when finished
    (save-excursion
      (when (search-backward completion (line-beginning-position) t)
        (let ((symbol
               ;; We should be just after the symbol
               (save-excursion (buffer-substring (- (point) 1) (point)))))
          (if (and ekg-linkify-inline-tags (eq major-mode 'org-mode))
              (replace-match
               (org-link-make-string
                (ekg--link-for-tag
                 (ekg--add-prefix-to-inline-tag
                  completion
                  symbol))
                completion))
            (cond
             ((eq major-mode 'org-mode)
              (replace-match (format "[%s]" completion)))
             ((eq major-mode 'markdown-mode)
              ;; Remove the symbol prefixing the tag
              (replace-match
               (format "[[%s%s]]"
                       (if (equal symbol "#") "" symbol)
                       completion))
              (save-excursion
                (goto-char (match-beginning 0))
                (delete-char -1))))))))))

(defvar-local ekg-notes-fetch-notes-function nil
  "Function to call to fetch the notes that define this buffer.
The order the notes are returned in is the order that they are
displayed.")

(defvar-local ekg-notes-name ""
  "Name displayed at the top of the buffer.")

(defvar-local ekg-notes-ewoc nil
  "Ewoc for the notes buffer.")

(defvar-local ekg-notes-hl nil
  "Highlight for the notes buffer.")

(defvar-local ekg-notes-tags nil
  "List of associated tags for creating and removing notes.")

(cl-defun ekg-note-create (&key text mode tags properties id)
  "Create a new `ekg-note' with TEXT, MODE, TAGS, PROPERTIES and ID."
  (let* ((time (time-convert (current-time) 'integer))
         (id (or id (ekg--generate-id)))
         (text (or text ""))
         (mode (or mode ekg-capture-default-mode)))
    (make-ekg-note :id id
                   :text text
                   :mode mode
                   :tags tags
                   :creation-time time
                   :modified-time time
                   :properties properties)))

(defun ekg--metadata-string-to-tag (s)
  "Return string S as a tag."
  (replace-regexp-in-string (rx ?\") "" s))

(defun ekg--metadata-string (property value)
  "Return a representation of PROPERTY with VALUE for the metadata.
This will be displayed at the top of the note buffer."
  (let ((read-only (string= property "Tags")))
    (format "%s%s%s"
            (concat
             (propertize (concat property ":") 'face 'bold 'read-only read-only)
             (propertize " " 'read-only nil 'rear-nonsticky t))
            value
            (propertize "\n" 'read-only read-only 'rear-nonsticky t))))

(defun ekg-should-show-id-p (id)
  "Return non-nil if the note ID should be shown to the user.
True when the ID represents a meaningful resource to the user,
rather than an auto-generated number."
  (not (numberp id)))

(defun ekg--replace-metadata ()
  "Replace the metadata in a buffer."
  (let ((note ekg-note))
    (with-temp-buffer
      (when (ekg-should-show-id-p (ekg-note-id note))
        (insert (ekg--metadata-string "Resource" (ekg-note-id note))))
      (insert
       (ekg--metadata-string "Tags"
                             (mapconcat (lambda (tag) (format "%s" tag))
                                        (ekg-note-tags note) ", ")))
      (map-apply (lambda (k v)
                   (when-let ((label (assoc-default k ekg-metadata-labels)))
                     (if (listp v)
                         (pcase (assoc-default label ekg-property-multivalue-type)
                           ('line (cl-loop for val in v do
                                           (insert (ekg--metadata-string label val))))
                           ('comma (insert (ekg--metadata-string
                                            label
                                            (if (listp v)
                                                (mapconcat (lambda (v) (format "%s" v))
                                                           v ", ")
                                              (format "%s" v))))))
                       (insert (ekg--metadata-string label v)))))
                 (ekg-note-properties note))
      (buffer-string))))

(defun ekg--metadata-overlay ()
  "Return the overlay used for metadata."
  (or (car (seq-filter
            (lambda (o) (eq 'ekg-metadata (overlay-get o 'category)))
            (overlays-in (point-min) (point-max))))
      (make-overlay (point-min) (point-max) nil nil t)))

(defun ekg--metadata-on-insert-behind (_ after begin-mod end-mod &optional _)
  "Make sure nothing is inserted behind the metadata overlay.
Also make sure we always have a line with which the user can add text.

AFTER is non-nil if the text is to be insert afterwards.
BEGIN-MOD is the beginning of the modification, and END-MOD is
the end."
  (when after
    (delete-region begin-mod end-mod)
    (when (= (point) (point-max))
      (insert "\n"))))

(defun ekg--metadata-modification (overlay after _ _ &optional _)
  "Make sure the metadata region doesn't interfere with editing.
This function is called on modification within the metadata.

BEGIN-MOD and END-MOD are the beginning and end points of the
modification by the user.

We want to make sure of a few things:
  1) The user isn't adding more than one empty line.

  2) There is at least one non-metadata line in the buffer.
Argument OVERLAY is the overlay whose modification triggers this
method.  Argument AFTER is non-nil if method is being called
after the modification.

  3) The user can't delete the metadata - if the user tries to
delete from the end of the metadata, we need to fix it back up."
  (when after
    (ekg-detect-tag-completion)
    ;; If we're at the end of the metadata, we need to make sure we don't delete
    ;; it from the previous line. Moving after is also suspicious, because we
    ;; don't know where to move it. It's easiest and clearest if we just do
    ;; nothing.
    (save-excursion
      (forward-line -1)
      (while (looking-at (rx (seq line-start (zero-or-more space) line-end)))
        (if (fboundp 'delete-line)
            (delete-line)
          (progn
            (beginning-of-line)
            (kill-line)
            (kill-line)))
        (forward-line -1)))
    (when (= (overlay-end overlay)
             (buffer-end 1))
      (let ((p (point)))
        (goto-char (buffer-end 1))
        ;; Walk backward until we get to content
        (while (looking-at (rx (seq line-start (zero-or-more space) line-end)))
          (forward-line -1))
        (forward-line)
        (setq p (point))
        (delete-region (point) (overlay-end overlay))
        (insert "\n")
        (move-overlay overlay (overlay-start overlay) p)))
    ;; Make sure the overlay ends on a newline, if not, insert one.
    (when
        (save-excursion
          (goto-char (- (overlay-end overlay) 1))
          (unless (looking-at "\n")
            (forward-char 1)
            (insert "\n")
            t ;; Return true if we inserted a newline.
            ))
      (goto-char (overlay-end overlay)))))

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
    (overlay-put o 'after-string (propertize (concat ekg-metadata-separator-text "\n")
                                             'read-only t 'rear-nonsticky t))
    (overlay-put o 'category 'ekg-metadata)
    (overlay-put o 'modification-hooks '(ekg--metadata-modification))
    (overlay-put o 'insert-behind-hooks '(ekg--metadata-on-insert-behind))
    (overlay-put o 'face 'ekg-metadata)
    (overlay-put o 'local-map global-map)
    (buffer-enable-undo)
    ;; If org-mode is on, the metadata messes up the org-element-cache, so let's disable it.
    (when (eq major-mode 'org-mode)
      (setq-local org-element-use-cache nil))))

;;;###autoload
(cl-defun ekg-capture (&key text mode tags properties id)
  "Capture a new note, with TEXT, MODE, TAGS and other PROPERTIES.
If ID is given, force the triple subject to be that value."
  (interactive)
  (let* ((id (or id (ekg--generate-id)))
         (buf (get-buffer-create (format "*EKG Capture (note %s)*" id)))
         (text (or text ""))
         (auto-tags (mapcan (lambda (f) (funcall f)) ekg-capture-auto-tag-funcs))
         (tags (seq-uniq (append tags auto-tags))))
    (set-buffer buf)
    (funcall (or mode ekg-capture-default-mode))
    (ekg-capture-mode 1)
    (setq ekg-note
          (ekg-note-create :id id
                           :text text
                           :mode mode
                           :tags tags
                           :properties properties))
    (ekg-edit-display-metadata)
    (goto-char (point-max))
    (mapc (lambda (tag) (run-hook-with-args 'ekg-note-add-tag-hook tag))
          (ekg-note-tags ekg-note))
    (mapc #'ekg-maybe-function-tag (ekg-note-tags ekg-note))
    (insert text)
    (if (and (eq mode 'org-mode)
             ekg-notes-display-images)
        (condition-case nil
            (org-redisplay-inline-images)
          (wrong-type-argument (message "Problem showing image for note ID %s (content: \"%s\")"
                                        id (ekg-truncate-at text 10)))))
    (set-buffer-modified-p nil)
    (pop-to-buffer buf)))

;;;###autoload
(defun ekg-capture-url (&optional url title)
  "Capture a new note given a URL and its TITLE.
However, if URL already exists, we edit the existing note on it."
  (interactive "MURL: \nMTitle: \n")
  (ekg-connect)
  (let ((cleaned-title (string-replace "," "" title))
        (existing (triples-get-subject ekg-db url)))
    (if existing
        (ekg-edit (ekg-get-note-with-id url))
      (ekg-capture :tags (list (concat "doc/" (downcase cleaned-title)))
                   :properties `(:titled/title ,(list title))
                   :id url))))

;;;###autoload
(defun ekg-capture-file ()
  "Capture a new note about the file the user is visiting.
This can only be called when in a buffer that has an associated
file.  If not, an error will be thrown."
  (interactive)
  (ekg-connect)
  (let ((file (buffer-file-name)))
    (unless file (error "Cannot capture: no file associated with this buffer"))
    (let* ((file (format "file:%s" (file-truename file)))
           (existing (triples-get-subject ekg-db file)))
      (if existing
          (ekg-edit (ekg-get-note-with-id file))
        (ekg-capture :tags (list (concat "doc/" (downcase (file-name-nondirectory file))))
                     :properties `(:titled/title ,(list (file-name-nondirectory file)))
                     :id file)))))

(defun ekg-change-mode (mode)
  "Change the mode of the current note to MODE."
  (interactive (list (intern (completing-read "Mode: " ekg-acceptable-modes)))
               ekg-capture-mode ekg-edit-mode)
  (let ((note ekg-note)
        (minor-mode (if ekg-capture-mode 'ekg-capture-mode 'ekg-edit-mode)))
    (funcall mode)
    (funcall minor-mode)
    (ekg--set-local-variables)
    (setq ekg-note note)))

(defun ekg-edit (note)
  "Edit an existing NOTE."
  (let ((buf (get-buffer-create (format "*EKG Edit: %s*" (ekg-note-id note)))))
    (set-buffer buf)
    (when (= 0 (buffer-size))
      (when (ekg-note-mode note)
        (funcall (ekg-note-mode note)))
      (ekg-edit-mode 1)
      (ekg--set-local-variables)
      (setq-local ekg-note (copy-ekg-note note)       ; shallow copy
                  ekg-note-orig-note (copy-tree note) ; deep copy to avoid later change
                  ekg-note-orig-id (ekg-note-id note))
      ;; When re-editing a note that's a draft, we need to remove the draft tag
      ;; so that when we save it, it's not a draft anymore.
      (setf (ekg-note-tags ekg-note)
            (seq-difference (ekg-note-tags ekg-note) (list ekg-draft-tag)))
      (ekg-edit-display-metadata)
      (setq-local ekg-note-orig-fields (seq-uniq
                                        (mapcar #'car (ekg--metadata-fields note))
                                        #'string=))
      (insert (ekg-insert-inlines-representation
               (ekg-note-text note) (ekg-note-inlines note)))
      (goto-char (+ 1 (overlay-end (ekg--metadata-overlay))))
      (mapc #'ekg-maybe-function-tag (ekg-note-tags ekg-note))
      (if (and (eq (ekg-note-mode note) 'org-mode)
               ekg-notes-display-images)
          (org-redisplay-inline-images)))
    (set-buffer-modified-p nil)
    (pop-to-buffer buf)))

(defun ekg--save-note-in-buffer ()
  "Save the current note.
Return the latest `ekg-note' object."
  (ekg-connect)
  (widen)
  (let* ((text (buffer-substring (+ 1 (overlay-end (ekg--metadata-overlay)))
                                 (point-max)))
         (ticons (ekg-extract-inlines text)))
    (setf (ekg-note-text ekg-note) (car ticons)
          (ekg-note-inlines ekg-note) (cdr ticons)
          (ekg-note-mode ekg-note) major-mode
          (ekg-note-tags ekg-note) (seq-uniq (ekg-note-tags ekg-note))))
  ;; Even though we will do this later in `ekg--normalize-note', we have to do
  ;; this now in case we removed the resource.
  (when (or (equal (ekg-note-id ekg-note) "") (not (ekg-note-id ekg-note)))
    (setf (ekg-note-id ekg-note) (ekg--generate-id)))
  (triples-with-transaction
    ekg-db
    (when (and ekg-note-orig-id
               (not (eq ekg-note-orig-id (ekg-note-id ekg-note)))
               (ekg-note-with-id-exists-p ekg-note-orig-id)
               (not (ekg-note-with-id-exists-p (ekg-note-id ekg-note)))
               (y-or-n-p "Changing the resource of this note will also change all references to it.  Are you sure?"))
      (let* ((existing-types (triples-get-types ekg-db (ekg-note-id ekg-note)))
             (conflicting-types (seq-intersection existing-types '(text tag titled))))
        (when (and conflicting-types
                   (y-or-n-p "Existing data exists on this resource, replace?"))
          (mapc (lambda (type) (triples-remove-type ekg-db ekg-note-orig-id type))
                conflicting-types))
        (triples-move-subject ekg-db ekg-note-orig-id (ekg-note-id ekg-note))))
    (ekg-save-note ekg-note))
  ekg-note)

(defun ekg--in-metadata-p ()
  "Whether the point is in the metadata section."
  (< (point) (overlay-end (ekg--metadata-overlay))))

(defun ekg--metadata-current-field ()
  "Return the label and value of the current metadata property.
If none can be found, return NIL."
  (when (ekg--in-metadata-p)
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (when (string-match
             (rx (seq (group (seq (one-or-more (not ?\:))))
                      ?\: (zero-or-more whitespace)
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
  ;; Only do something when we aren't in a read-only space.
  (when
      (or (null (ekg--metadata-current-field))
          ;; + 2 for the colon and space
          (>= (current-column) (+ 2 (length (car (ekg--metadata-current-field))))))
    (if-let (field (ekg--metadata-current-field))
        (when-let (completion-func (assoc (car field) ekg-capf-field-complete-funcs
                                          #'equal))
          (funcall (cdr completion-func)))
      ;; There's no current field, but we're in the metadata, so let's complete
      ;; the possible fields.
      (when (ekg--in-metadata-p)
        (ekg--field-name-complete)))))

(defun ekg--field-name-complete ()
  "Completion function for metadata field names."
  (list (save-excursion (beginning-of-line) (point))
        (save-excursion (skip-chars-forward "^:\s\t\n") (point))
        (completion-table-dynamic
         (lambda (_)
           ;; Complete all fields, except single-value fields (which have no
           ;; entry in ekg-property-multivalue-type) with an existing field
           ;; already in existence.
           (seq-difference (mapcar #'car ekg-metadata-parsers)
                           (seq-difference
                            (mapcar #'car (ekg--metadata-fields nil))
                            (mapcar #'car ekg-property-multivalue-type)))))
        :exclusive t :exit-function (lambda (_completion finished)
                                      (when finished (insert ": ")))))

(defun ekg--tags-complete ()
  "Completion function for tags, CAPF-style."
  (let ((end (save-excursion
               (skip-chars-forward "^,\t\n")
               (point)))
        (start (save-excursion
                 (skip-chars-backward "^,\t\n:")
                 ;; We are at the right boundary, but now ignore whitespace.
                 (skip-chars-forward "\s+")
                 (point))))
    (list start end (completion-table-dynamic
                     (lambda (_)
                       (seq-difference (ekg-tags) (ekg-note-tags ekg-note))))
          :exclusive t)))

(defun ekg-detect-tag-completion ()
  "After a modification, check if the user has completed a tag.
If so, call the necessary hooks."
  (let ((field (ekg--metadata-current-field)))
    (when (equal "Tags" (car field))
      (let ((current-tags (ekg-note-tags ekg-note)))
        (dolist (maybe-tag (mapcar #'string-trim (split-string (cdr field) ",")))
          (when (and (not (member maybe-tag current-tags))
                     (member maybe-tag (ekg-tags)))
            (ekg--update-from-metadata)
            (run-hook-with-args 'ekg-note-add-tag-hook maybe-tag)
            (ekg-maybe-function-tag maybe-tag)))))))

(defun ekg-save-draft ()
  "Save the current note as a draft."
  (interactive nil ekg-edit-mode ekg-capture-mode)
  (ekg--update-from-metadata)
  (when ekg-draft-tag
    (push ekg-draft-tag (ekg-note-tags ekg-note)))
  (ekg--save-note-in-buffer)
  (unless ekg-save-no-message
    (message "Note saved to drafts.")))

(defun ekg-edit-save ()
  "Save the edited note and refresh where it appears."
  (interactive nil ekg-edit-mode)
  (ekg--update-from-metadata)
  (let ((note (ekg--save-note-in-buffer))
        (orig-id ekg-note-orig-id))
    (unless ekg-save-no-message
      (message "Note saved."))
    (cl-loop for b being the buffers do
             (with-current-buffer b
               (when (and (eq major-mode 'ekg-notes-mode) ekg-notes-ewoc)
                 (let ((n (ewoc-nth ekg-notes-ewoc 0)))
                   (while n
                     (when (or (equal (ekg-note-id (ewoc-data n))
                                      (ekg-note-id note))
                               (and orig-id
                                    (equal orig-id (ekg-note-id (ewoc-data n)))))
                       (ewoc-set-data n note)
                       (ewoc-invalidate ekg-notes-ewoc n))
                     (setq n (ewoc-next ekg-notes-ewoc n)))))))))

(defun ekg-edit-finalize ()
  "Save the edited note and refresh where it appears."
  (interactive nil ekg-edit-mode)
  (ekg-edit-save)
  (kill-buffer))

(defun ekg--split-metadata-string (val)
  "Split multi-valued metadata field VAL into the component values.
The metadata fields are comma separated."
  (split-string val (rx (seq ?\, (zero-or-more space))) t (rx (1+ space))))

(defun ekg--metadata-update-tag (val)
  "Update the tag field from the metadata VAL."
  (setf (ekg-note-tags ekg-note) val))

(defun ekg--metadata-update-title (val)
  "Update the title field from the metadata VAL."
  (setf (ekg-note-properties ekg-note)
        (plist-put (ekg-note-properties ekg-note) :titled/title val)))

(defun ekg--metadata-update-resource (val)
  "Update the resource to the metadata VAL."
  (setf (ekg-note-id ekg-note) (or val (ekg--generate-id))))

(defun ekg--metadata-fields (expect-valid)
  "Return all metadata fields as a cons of labels and values.
If EXPECT-VALID is true, warn when we encounter an unparseable field."
  (save-excursion
    (let ((mo (ekg--metadata-overlay))
          (fields))
      (goto-char (overlay-start mo))
      (while (< (point) (overlay-end mo))
        (if-let (field (ekg--metadata-current-field))
            (push field fields)
          (when expect-valid
            (warn "EKG: No field could be parsed from metadata line at point %s" (point))))
        (forward-line))
      (append (reverse fields)
              ;; Add any original field that is no longer here, with an empty value.
              (mapcar #'list (seq-difference ekg-note-orig-fields
                                             (mapcar #'car fields)))))))

(defun ekg--update-from-metadata ()
  "Update the `ekg-note' object from the metadata."
  (cl-loop with values = (make-hash-table :test 'equal)
           for (field . value) in (ekg--metadata-fields t)
           do
           (pcase (assoc-default field ekg-property-multivalue-type)
             ('line (unless (string-empty-p value) (push value (gethash field values))))
             ('comma (mapc (lambda (elt) (push elt (gethash field values)))
                           (ekg--split-metadata-string value)))
             (_ (setf (gethash field values) value)))
           finally return
           (maphash (lambda (key val)
                      (if-let (func (assoc key ekg-metadata-parsers))
                          (funcall (cdr func) (if (listp val)
                                                  (nreverse (flatten-list val))
                                                val))
                        (warn "EKG: No function found for field %s" key)))
                    values)))

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
                          (or (seq-intersection (ekg-note-tags note) ekg-notes-tags)
                              (string-match-p
                               (rx (or "latest modified" "latest captured"))
                               (substring-no-properties
                                (ewoc--node-data
                                 (ewoc--header ekg-notes-ewoc))))))
                 (ewoc-enter-first ekg-notes-ewoc note))))))

(defun ekg-capture-abort ()
  "Abort the current capture.
Notes saved as drafts will be deleted."
  (interactive nil ekg-capture-mode)
  (ekg-connect)
  (let ((id (ekg-note-id ekg-note)))
    (when (ekg-note-with-id-exists-p id)
      (ekg-note-delete ekg-note)))
  (setq-local kill-buffer-query-functions
              (delq 'ekg--kill-buffer-query-function
                    kill-buffer-query-functions))
  (kill-buffer))

(defun ekg-note-active-tags (note)
  "Return the tags of NOTE that are considered normal tags."
  (seq-difference (ekg-note-tags note)
                  (list ekg-draft-tag ekg-trash-tag)))

(defun ekg-fix-renamed-dup-tags (id)
  "Fix duplicate tags in note with ID.
After a tag is renamed, it could become a duplicate of another
tag.  This defun will fix the problem for one note, only
executing a write if there is a problem."
  (let* ((tagged-plist (triples-get-type ekg-db id 'tagged))
         (tags (plist-get tagged-plist :tag))
         (uniq-tags (seq-uniq tags)))
    (when (> (length tags) (length uniq-tags))
      (apply #'triples-set-type ekg-db id 'tagged (plist-put tagged-plist :tag uniq-tags)))))

(defun ekg-clean-dup-tags ()
  "Fix all duplicate tags in the database."
  (ekg-connect)
  (let ((cleaned)
        (tags (triples-subjects-of-type ekg-db 'tag)))
    (cl-loop for tag in tags do
             (let ((tagged (plist-get (triples-get-type ekg-db tag 'tag) :tagged)))
               (when (> (length tagged) (length (seq-uniq tagged)))
                 ;; if there is duplication in the tag list then
                 ;; something must have duplicate tags.
                 (mapc #'ekg-fix-renamed-dup-tags tagged)
                 (push tag cleaned))))
    (when cleaned
      (message "%d cleaned tags that were duplicated: %s" (length cleaned)
               (mapconcat #'identity cleaned ", ")))))

(defun ekg-clean-leftover-types ()
  "Clean up any ekg types that are left over without ekg notes."
  (ekg-connect)
  (let ((cleaned)
        (notes (triples-subjects-of-type ekg-db 'text)))
    (cl-loop for type in ekg-schema-text-cotypes do
             (cl-loop for s in (seq-difference (triples-subjects-of-type ekg-db type) notes) do
                      (push s cleaned)
                      (triples-remove-type ekg-db s type)))
    (when cleaned
      (message "%d notes cleaned of leftover information: %s" (length cleaned)
               (mapconcat (lambda (id) (format "%s" id)) cleaned ", ")))))

;; In order for emacsql / sqlite to not give build warnings we need to declare
;; them. Because we only require one to be installed, following the
;; implementation in the triples library, we can't just require them.
(declare-function emacsql "ext:emacsql.el")
(declare-function sqlite-execute "ext:sqlite.c")

(defun ekg-global-rename-tag (from-tag to-tag)
  "Rename FROM-TAG to TO-TAG.
This can be done whether TO-TAG already exists or not.  This
renames all instances of the tag globally, and all notes with
FROM-TAG will use TO-TAG."
  (interactive (list (completing-read "From tag: " (ekg-tags) nil t)
                     (ekg--normalize-tag (completing-read "To tag: " (ekg-tags)))))
  (ekg-connect)
  (triples-with-transaction
    ekg-db
    (let ((old-tag-ids (plist-get (triples-get-type ekg-db from-tag 'tag) :tagged)))
      (pcase triples-sqlite-interface
        ('builtin (if (fboundp 'sqlite-execute)
                      (sqlite-execute
                       ekg-db
                       "UPDATE triples SET object = ? WHERE object = ? AND predicate = 'tagged/tag'"
                       (list (triples-standardize-val to-tag) (triples-standardize-val from-tag)))
                    (error "The triples library incorrectly configured to use sqlite in 29.1, which is not available")))
        ('emacsql (emacsql ekg-db [:update triples :set (= object $s1) :where (= object $s2) :and (= predicate 'tagged/tag)]
                           to-tag from-tag)))
      (triples-remove-type ekg-db from-tag 'tag)
      (triples-set-type ekg-db to-tag 'tag)
      (mapc #'ekg-fix-renamed-dup-tags old-tag-ids)))
  (ekg-backup))

(defun ekg-tags ()
  "Return a list of all tags.
Does not include any tags with special uses (e.g. trash and draft
tags)."
  (ekg-connect)
  (seq-difference (triples-subjects-of-type ekg-db 'tag)
                  (list ekg-draft-tag ekg-trash-tag)))

(defun ekg-tags-including (substring)
  "Return all tags including SUBSTRING."
  (ekg-connect)
  (seq-filter (lambda (tag) (string-match-p (rx (literal substring)) tag))
              (triples-subjects-of-type ekg-db 'tag)))

(defun ekg-tags-with-prefix (prefix)
  "Return all tags with PREFIX."
  (ekg-connect)
  (seq-filter (lambda (tag) (string-match-p (rx (seq line-start (literal prefix))) tag))
              (triples-subjects-of-type ekg-db 'tag)))

(defun ekg-tags-display (tags)
  "Return string representing a group of TAGS."
  (mapconcat #'identity
             (sort (seq-copy tags) #'string<) ", "))

(defun ekg-display-note (note template)
  "Return TEMPLATE formatted NOTE for display in a buffer."
  (ekg-connect)
  (let* ((ic (ekg-extract-inlines template))
         (template-types (mapcan (lambda (i)
                                   (when (eq 'note (ekg-inline-type i))
                                     (list (car (ekg-inline-command i)))))
                                 (cdr ic))))
    ;; If there is a command for the type of "other", then we need to add
    ;; in all types that are in the note properties, and have valid
    ;; functions.
    (when (memq 'other template-types)
      (setf (cdr ic)
            (mapcan (lambda (i)
                      (if (and (eq 'note (ekg-inline-type i))
                               (eq 'other (car (ekg-inline-command i))))
                          (cl-loop for type in
                                   (seq-uniq
                                    (seq-difference
                                     (mapcar (lambda (prop)
                                               (car (triples-combined-to-type-and-prop prop)))
                                             (map-keys (ekg-note-properties note)))
                                     template-types))
                                   when (fboundp (intern (format "ekg-display-note-%s" type)))
                                   collect (make-ekg-inline :type 'note
                                                            :command (list type)
                                                            :pos (ekg-inline-pos i)))
                        (list i)))
                    (cdr ic))))
    (ekg-insert-inlines-results (car ic) (cdr ic) note)))

(defun ekg-display-note-insert (note)
  "Insert the the display of NOTE into the buffer."
  (insert (ekg-display-note note ekg-display-note-template)))

(defun ekg--note-highlight ()
  "In the buffer, highlight the current note."
  (let ((node (ewoc-locate ekg-notes-ewoc)))
    (when (and node (ewoc-location node))
      (move-overlay ekg-notes-hl
                    (ewoc-location node)
                    (- (or (if-let (next (ewoc-next ekg-notes-ewoc node))
                               (ewoc-location next)
                             (point-max))) 1)))))

(defun ekg-current-note-or-error ()
  "Return the current `ekg-note'.
Raise an error if there is no current note."
  (unless (eq major-mode 'ekg-notes-mode)
    (error "This command can only be used in `ekg-notes-mode'"))
  (if-let (node (ewoc-locate ekg-notes-ewoc))
      (ewoc-data node)
    (error "No current note is available to act on!  Create a new note first with `ekg-capture'")))

(defun ekg-notes-tag (&optional tag)
  "Show notes associated with TAG.
If TAG is nil, it will be read, selecting from the list of the current note's
tags."
  (interactive (list (completing-read "Tag: " (ekg-note-tags (ekg-current-note-or-error))))
               ekg-notes-mode)
  (ekg-show-notes-with-tag tag))

(defun ekg-notes-open ()
  "Open the current note."
  (interactive nil ekg-notes-mode)
  (ekg-edit (ekg-current-note-or-error)))

(defun ekg-notes-kill ()
  "Kill (hide) the current note from the view.
Note is not deleted from the database and will reappear when the
view is refreshed."
  (interactive nil ekg-notes-mode)
  (let ((inhibit-read-only t))
    (ewoc-delete ekg-notes-ewoc (ewoc-locate ekg-notes-ewoc))
    (ekg--note-highlight)))

(defun ekg-notes-delete (arg)
  "Trash the current note.
With a `C-u' prefix or when ARG is non-nil, silently delete the
current note without a prompt."
  (interactive "P" ekg-notes-mode)
  (let ((note (ekg-current-note-or-error))
        (inhibit-read-only t))
    (when (or arg (y-or-n-p "Are you sure you want to delete this note?"))
      (ekg-note-trash note)
      (ewoc-delete ekg-notes-ewoc (ewoc-locate ekg-notes-ewoc))
      (ekg--note-highlight))))

(defun ekg-notes-browse ()
  "If the note is about a browseable resource, browse to it.

If the link is a web address, open in browser with `browse-url'.
Otherwise, open in Emacs with `find-file'."
  (interactive nil ekg-notes-mode)
  (let ((note (ekg-current-note-or-error)))
    (cond ((ffap-url-p (ekg-note-id note))
           (let* ((url (ekg-note-id note))
                  (struct (url-generic-parse-url url))
                  (full (url-fullness struct))
                  (file (car (url-path-and-query struct))))
             (if full
                 (browse-url url)
               (when (and file (> (length file) 0))
                 (find-file file))))))))

(defun ekg-notes-select-and-browse-url (title)
  "Browse one of the resources in the current buffer's notes.
TITLE is the title of the URL to browse to."
  (interactive (list
                (completing-read
                 "Doc: "
                 (mapcan (lambda (note)
                           (plist-get (ekg-note-properties note)
                                      :titled/title))
                         (ewoc-collect ekg-notes-ewoc #'identity)))) ekg-notes-mode)
  (when title (ekg-browse-url title)))

(defun ekg--show-notes (name notes-func tags)
  "Display notes from NOTES-FUNC in buffer.
New notes are created with additional tags TAGS.
NAME is displayed at the top of the buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((ewoc (ewoc-create #'ekg-display-note-insert
                           (propertize name 'face 'ekg-notes-mode-title))))
    (mapc (lambda (note) (ewoc-enter-last ewoc note)) (funcall notes-func))
    (ekg-notes-mode)
    (setq-local ekg-notes-ewoc ewoc
                ekg-notes-fetch-notes-function notes-func
                ekg-notes-name name
                ekg-notes-hl (make-overlay 1 1)
                ekg-notes-tags tags)
    (overlay-put ekg-notes-hl 'face hl-line-face)
    ;; Move past the title
    (forward-line 1)
    (ekg--note-highlight)
    (when (eq ekg-capture-default-mode 'org-mode)
      (ekg--notes-activate-links)
      (if ekg-notes-display-images (org-redisplay-inline-images))))
  (set-buffer-modified-p nil))

(defun ekg--notes-activate-links()
  "Activate the `org-mode' links so they can be seen and followed."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (org-activate-links (point-max))
        (goto-char (match-end 0)))
      ;; Go back and activate the first link again as it gets missed in the iteration
      (goto-char (point-min))
      (org-activate-links (point-max)))))

(defun ekg-notes-refresh ()
  "Refresh the current `ekg-notes' buffer."
  (interactive nil ekg-notes-mode)
  (ekg--show-notes
   ekg-notes-name
   ekg-notes-fetch-notes-function
   ekg-notes-tags))

(defun ekg-notes-create ()
  "Add a note that by default has all the tags in the buffer."
  (interactive nil ekg-notes-mode)
  (ekg-capture :tags ekg-notes-tags))

(defun ekg-notes-next ()
  "Move to the next note, if possible."
  (interactive nil ekg-notes-mode)
  (ewoc-goto-next ekg-notes-ewoc 1)
  (ekg--note-highlight))

(defun ekg-notes-previous ()
  "Move to the previous note, if possible."
  (interactive nil ekg-notes-mode)
  (ewoc-goto-prev ekg-notes-ewoc 1)
  (ekg--note-highlight))

(defun ekg-notes-any-note-tags ()
  "Show notes with any of the tags in the current note."
  (interactive nil ekg-notes-mode)
  (ekg-show-notes-with-any-tags (ekg-note-tags (ewoc-data (ewoc-locate ekg-notes-ewoc)))))

(defun ekg-notes-any-tags ()
  "Show notes with any of the tags in any of the notes in the buffer."
  (interactive nil ekg-notes-mode)
  (ekg-show-notes-with-any-tags
   (seq-uniq (flatten-list
              (mapcar (lambda (n) (ekg-note-tags n))
                      (ewoc-collect ekg-notes-ewoc #'identity))))))

(defun ekg-setup-notes-buffer (name notes-func tags)
  "Set up and display new buffer with NAME.
NAME is the base name, to which ekg will be prepended, and
asterisks will surround (to indicate a non-file-based buffer).
NOTES-FUNC is used to get the list of notes to display.  New
notes are created with additional tags TAGS."
  (let ((buf (get-buffer-create (format "*ekg %s*" name))))
    (set-buffer buf)
    (ekg--show-notes name notes-func tags)
    (switch-to-buffer buf)
    (setq-local default-directory (ekg--notes-directory))))

(defun ekg-sort-by-creation-time (a b)
  "Used to pass to `sort', which will supply A and B."
  (> (ekg-note-creation-time a)
     (ekg-note-creation-time b)))

;;;###autoload
(defun ekg-show-notes-with-any-tags (tags)
  "Show notes with any of TAGS."
  (interactive (list (completing-read-multiple "Tags: " (ekg-tags))))
  (ekg-setup-notes-buffer
   (format "tags (any): %s" (ekg-tags-display tags))
   (lambda () (ekg-get-notes-with-any-tags tags))
   tags))

;;;###autoload
(defun ekg-show-notes-with-all-tags (tags)
  "Show notes that contain all TAGS."
  (interactive (list (completing-read-multiple "Tags: " (ekg-tags))))
  (ekg-setup-notes-buffer
   (format "tags (all): %s" (ekg-tags-display tags))
   (lambda () (sort (ekg-get-notes-with-tags tags)
                    #'ekg-sort-by-creation-time))
   tags))

;;;###autoload
(defun ekg-show-notes-with-tag (tag)
  "Show notes that contain TAG."
  (interactive (list (completing-read "Tag: " (ekg-tags))))
  (ekg-setup-notes-buffer
   (format "tag: %s" (ekg-tags-display (list tag)))
   (lambda () (sort (ekg-get-notes-with-tag tag) #'ekg-sort-by-creation-time))
   (list tag)))

;;;###autoload
(defun ekg-show-notes-for-query (query)
  "Show notes matching QUERY.

This does a token-based search, so single letters or word fragments and
punctuation may not match.

This uses `ekg-query-pred-abbrevs' to provide abbreviations for querying
for special forms of note data.  For example, you can search for tags
with `tag:<term>' or `title:<term>', or `text:<term>'.  Anything not
prefixed will search any text associated with the subject.

Text included in inline templates is not searched."
  (interactive "sQuery: ")
  (ekg-connect)
  (ekg-setup-notes-buffer
   (format "query: %s" query)
   (lambda () (seq-filter #'ekg-note-active-p (mapcar #'ekg-get-note-with-id (triples-fts-query-subject ekg-db query ekg-query-pred-abbrevs)))) nil))

;;;###autoload
(defun ekg-show-notes-in-trash ()
  "Show notes that have only tags that are trashed."
  (interactive)
  (ekg-connect)
  (ekg-setup-notes-buffer
   "Trash"
   (lambda ()
     (sort
      (ekg-get-notes-with-tag ekg-trash-tag)
      #'ekg-sort-by-creation-time))
   nil))

(defun ekg-show-notes-with-tag-prefix (prefix)
  "Show notes that contain a tag with PREFIX."
  (interactive "Mtag prefix: ")
  (ekg-connect)
  (let ((tags (ekg-tags-with-prefix prefix)))
    (ekg-setup-notes-buffer
     (format "tag prefix: %s" prefix)
     (lambda ()
       (sort
        (seq-uniq (mapcan (lambda (tag) (ekg-get-notes-with-tag tag)) tags))
        #'ekg-sort-by-creation-time))
     tags)))

;;;###autoload
(defun ekg-show-notes-in-drafts ()
  "Show all notes in the draft state.
These notes have not yet been saved, and don't show up in most
other views."
  (interactive)
  (unless ekg-draft-tag
    (error "Variable ekg-draft-tag is nil, so drafts are not used so drafts can be shown"))
  (ekg-connect)
  (ekg-setup-notes-buffer
   "Drafts"
   (lambda ()
     (sort
      (seq-filter (lambda (note) (not (member ekg-trash-tag (ekg-note-tags note))))
                  (ekg-get-notes-with-tag ekg-draft-tag))
      #'ekg-sort-by-creation-time))
   nil))

;;;###autoload
(defun ekg-show-notes-for-today ()
  "Show all notes with today's date as a tag."
  (interactive)
  (ekg-show-notes-with-tag (car (ekg-date-tag))))

;;;###autoload
(defun ekg-show-notes-latest-captured (&optional num)
  "Show the last several notes taken.
NUM is by default `ekg-notes-size', which determines how many
notes to show.  But with a prefix ARG, ask the user."
  (interactive (list (if current-prefix-arg
                         (read-number "Number of notes to display: ")
                       ekg-notes-size)))
  (ekg-connect)
  (ekg-setup-notes-buffer
   "Latest captured notes"
   (lambda ()
     (cl-loop for id in (mapcar #'car (sort (triples-with-predicate
                                             ekg-db
                                             'time-tracked/creation-time)
                                            (lambda (trip1 trip2) (> (nth 2 trip1)
                                                                     (nth 2 trip2)))))
              until (= (length selected) (or num ekg-notes-size))
              when (ekg-active-id-p id)
              collect (ekg-get-note-with-id id) into selected
              finally return selected))
   nil))

;;;###autoload
(defun ekg-show-notes-latest-modified (&optional num)
  "Show the last several notes modified.
NUM is by default `ekg-notes-size', which determines how many
notes to show.  But with a prefix ARG, ask the user."
  (interactive (list (if current-prefix-arg
                         (read-number "Number of notes to display: ")
                       ekg-notes-size)))
  (ekg-connect)
  (ekg-setup-notes-buffer
   "Latest modified notes"
   (lambda ()
     (cl-loop for id in (mapcar #'car (sort (triples-with-predicate
                                             ekg-db
                                             'time-tracked/modified-time)
                                            (lambda (trip1 trip2) (> (nth 2 trip1)
                                                                     (nth 2 trip2)))))
              until (= (length selected) (or num ekg-notes-size))
              when (ekg-active-id-p id)
              collect (ekg-get-note-with-id id) into selected
              finally return selected))
   nil))

(defun ekg-document-titles ()
  "Return an alist of all titles.
The key is the subject and the value is the title."
  (ekg-connect)
  (mapcan
   (lambda (sub)
     (mapcar (lambda (title) (cons sub title))
             (plist-get (triples-get-type ekg-db sub 'titled) :title)))
   (seq-difference
    (triples-subjects-of-type ekg-db 'titled)
    (ekg-inactive-note-ids))))

(defun ekg-browse-url (title)
  "Browse the url corresponding to TITLE.
If no corresponding URL is found, an error is thrown."
  (interactive (list (completing-read "Doc: "
                                      (mapcan
                                       (lambda (tcons)
                                         (when (ffap-url-p (car tcons))
                                           (list (cdr tcons))))
                                       (ekg-document-titles)))))
  (ekg-connect)
  (let ((subjects (seq-filter #'ffap-url-p (triples-subjects-with-predicate-object ekg-db 'titled/title title))))
    (when (= 0 (length subjects)) (error "Could not fetch existing URL title: %s" title))
    (when (> (length subjects) 1) (warn "Multiple URLs with the same title exist: %s" title))
    (browse-url (car subjects))))

;;;###autoload
(defun ekg-search ()
  "Search notes by text content with live updating.
As you type, the list of matching notes is updated.  Press RET to
view the matching notes in the standard notes view."
  (interactive)
  (ekg-connect)
  (let* ((minibuffer-setup-hook
          (cons (lambda ()
                  (add-hook 'post-command-hook #'ekg--search-update nil t))
                minibuffer-setup-hook))
         (query (read-string "Search notes: " nil 'ekg-search-history)))
    (when (not (string-empty-p query))
      (ekg-show-notes-with-search-results query))))

(defun ekg--search-update ()
  "Update the search results as the user types in the minibuffer."
  (let* ((query (minibuffer-contents))
         (results (when (not (string-empty-p query))
                    (ekg--search-notes query ekg-search-max-results))))
    (with-current-buffer (get-buffer-create "*ekg search preview*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when results
          (insert (format "Matching notes for: %s\n\n" query))
          (dolist (note results)
            (insert (ekg-display-note note ekg-oneliner-note-template) "\n")))
        (when (> (length results) ekg-search-max-results)
          (insert (format "\n...and %d more matches." (- (length results) ekg-search-max-results))))
        (goto-char (point-min))
        (setq buffer-read-only t))
      (display-buffer (current-buffer)
                      '((display-buffer-in-side-window)
                        (side . bottom)
                        (window-height . 10))))))

(defun ekg--search-notes (query &optional limit)
  "Find notes containing QUERY in their text content.
Returns a list of matching notes, up to LIMIT if provided."
  (let ((plainquery (substring-no-properties query))
        (limit (or limit ekg-search-max-results)))
    (ekg-connect)
    (mapcar #'ekg-get-note-with-id
            (seq-difference
             (mapcar #'car (seq-union (triples-search ekg-db :text/text plainquery limit)
                                      (triples-search ekg-db :titled/title plainquery limit)))
             (ekg-inactive-note-ids)))))

(defun ekg-show-notes-with-search-results (query)
  "Show notes that match the search QUERY."
  (ekg-setup-notes-buffer
   (format "search: %s" query)
   (lambda () (ekg--search-notes query))
   nil))

(defun ekg-date-tag-p (tag)
  "Return non-nil if TAG is a date tag."
  (let ((prefix "date/"))
    (string= prefix (substring-no-properties tag 0 (min (length prefix) (length tag))))))

(defun ekg-inactive-note-ids ()
  "Get a list of ekg-note objects, representing all inactive notes.
Inactive in this context means trashed or draft note."
  (mapcan (lambda (tag)
            (plist-get (triples-get-type ekg-db tag 'tag) :tagged))
          (list ekg-draft-tag ekg-trash-tag)))

(defun ekg-active-note-ids ()
  "Get a list of ekg-note objects, representing all active notes.
Active in this context means non-trashed."
  (ekg-connect)
  (seq-difference
   (triples-subjects-of-type ekg-db 'text)
   (ekg-inactive-note-ids)))

(defun ekg-get-notes-cotagged-with-tags (tags cotag)
  "Return a list of all notes with one of TAGS and COTAG.
Parents of the tags in TAGS are also considered.  Specifically,
look at each tag in order, from ancestor to child (so if a tag
was a/b/c, we'd check a, then a/b, then a/b/c), and for each of
those look at all notes also tagged with COTAG.  We return a list
of the text of all these notes for all tags and tag parents, etc."
  (flatten-list (mapcar (lambda (tag)
                          (ekg-get-notes-with-tags (list tag cotag)))
                        (seq-mapcat #'ekg-tag-to-hierarchy tags))))

(defun ekg-on-add-tag-insert-template (tag)
  "Look for templates for TAG, and insert into current buffer.
This looks for notes with tags TAG and `template', and for any
found, insert, one by one, into the current note."
  (unless (equal tag ekg-template-tag)
    (mapc (lambda (template)
            (save-excursion (goto-char (point-max))
                            ;; Don't insert the same string twice, which is
                            ;; sometimes possible when templates have more than
                            ;; one tag overlapping with the current note.
                            (unless (string-match (rx (literal (ekg-note-text template)))
                                                  (buffer-substring-no-properties (+ 1 (overlay-end (ekg--metadata-overlay)))
                                                                                  (point-max)))
                              (unless (looking-at (rx (seq line-start line-end)))
                                (insert "\n"))
                              (insert (ekg-note-text template)))))
          (ekg-get-notes-cotagged-with-tags (list tag) ekg-template-tag))))

;; Auto-tag functions

(defun ekg-tag-for-date (&optional date)
  "Return standard tag for DATE.
This uses ISO 8601 format."
  (format-time-string "date/%F" date))

(defun ekg-date-tag ()
  "Get single tag representing the date as a ISO 8601 format."
  (list (ekg-tag-for-date)))

(defun ekg-tag-to-hierarchy (tag)
  "Given TAG, return a list of the hierarchy of tags.
This works with TAG having a hierarchy, such as \"foo/bar/baz\",
which would return `(\"foo\" \"foo/bar\" \"foo/bar/baz\")'."
  (let ((tags (split-string tag "/")))
    (cl-loop for i from 1 to (length tags)
             collect (mapconcat #'identity (seq-take tags i) "/"))))

(defun ekg-maybe-function-tag (tag)
  "Apply edit function for TAG, if it exists.
The tag value in `ekg-function-tag' is treated specially here -
it ensures the mode is `emacs-lisp-mode.' If TAG is a hierarchy,
we try applying functions from the top to the bottom of the
hierarchy, so the most general tag to the most specific tag."
  (mapc (lambda (tag)
          (if (equal tag ekg-function-tag)
              (unless (eq major-mode 'emacs-lisp-mode)
                (ekg-change-mode "emacs-lisp-mode"))
            (mapc #'eval
                  (mapcar #'read
                          (mapcar #'ekg-note-text (ekg-get-notes-with-tags (list tag ekg-function-tag)))))))
        (ekg-tag-to-hierarchy tag)))

(defun ekg-force-upgrade ()
  "Force an upgrade of the ekg database.
This calls `ekg-upgrade-db', but insures that it does all
necessary upgrades."
  (interactive)
  (ekg-upgrade-db nil))

(defun ekg-upgrade-db (from-version)
  "After updating, do any necessary upgrades.
This is designed so that it can be run an arbitrary number of
times.  If there's nothing to do, it won't have any effect.  If
an upgrade is needed, it will always make a backup, regardless of
backup settings, and will not delete any backups, regardless of
other settings.  FROM-VERSION is the version of the database
before the upgrade, in list form.  TO-VERSION is the version of
the database after the upgrade, in list form."
  (let ((need-fts-upgrade
         (or (null from-version)
             (version-list-< from-version '(0 7 0))))
        (need-trash-upgrade
         (or (null from-version)
             ;; Version 0.5.0 changed how trashed tags are handled.
             (version-list-< from-version '(0 5 0))))
        (need-triple-0.3-upgrade
         (or (null from-version)
             ;; We have done upgrades to 0.3.1, but we want to re-do them for
             ;; additional bugfixes. There should be no downside to doing the
             ;; upgrade many times.
             (version-list-< from-version '(0 3 2))))
        (need-type-removal-upgrade
         (or (null from-version)
             (version-list-< from-version '(0 6 3)))))
    (ekg-connect)
    (when (and (eq 'builtin triples-sqlite-interface) need-fts-upgrade)
      (triples-fts-setup ekg-db))
    (when need-fts-upgrade
      (triples-fts-setup ekg-db))
    ;; In the future, we can separate out the backup from the upgrades.
    (when need-type-removal-upgrade
      (ekg-backup t)
      (triples-remove-schema-type ekg-db 'person)
      (triples-remove-schema-type ekg-db 'email))
    (when need-triple-0.3-upgrade
      (ekg-backup t)
      ;; This converts all string integers in subjects and objects to real integers.
      (triples-upgrade-to-0.3 ekg-db)
      ;; The above may cause issues if there tags that are integers, since tags have
      ;; to be strings. So let's iterate through all tag subjects and re-convert
      ;; them to strings.
      (cl-loop for tag in (triples-subjects-of-type ekg-db 'tag) do
               (when (numberp tag)
                 (ekg-global-rename-tag tag (format "%d" tag))))
      ;; Also, we need to convert any text back to strings. We only need to do
      ;; this for the builtin sqlite, since that's the only case that
      ;; triples-upgrade-to-0.3 will do anything.
      )
    (when need-trash-upgrade
      (ekg-backup t)
      (let* ((trash-ids (ekg-tags-with-prefix "trash/"))
             (note-ids (mapcan (lambda (tag) (plist-get (triples-get-type ekg-db tag 'tag) :tagged))
                               trash-ids)))
        (triples-with-transaction
          ekg-db
          (cl-loop for id in note-ids do
                   (let ((note (ekg-get-note-with-id id)))
                     (cond
                      ((null note)
                       (message "ekg-upgrade-db: Note %s has a trashed tag but doesn't exist!  This is unusual and should not happen." id))
                      ((seq-some (lambda (tag)
                                   (not (string-match-p (rx (literal "trash/")) tag)))
                                 (ekg-note-tags note))
                       (message "ekg-upgrade-db: Note %s has some trash tags but not all trash tags,  you can run ekg-show-notes-with-tag-prefix to find all notes with tags with the 'trash/' prefix."
                                id))
                      (t
                       (message "ekg-upgrade-db: Moving note %s from trash tags to trash tag"
                                id)
                       (ekg-note-trash note)))))
          (cl-loop for tag in trash-ids do
                   (triples-remove-type ekg-db tag 'tag)
                   (triples-set-type ekg-db ekg-trash-tag 'tag)))))))

(defun ekg-tag-used-p (tag)
  "Return non-nil if TAG has useful information."
  (ekg-connect)
  (triples-get-type ekg-db tag 'tag))

(defun ekg-remove-unused-tags ()
  "Remove all tags that are not used and have no info."
  (ekg-connect)
  (cl-loop for tag in (seq-filter
                       (lambda (tag) (not (ekg-tag-used-p tag)))
                       (triples-subjects-of-type ekg-db 'tag))
           do
           (ekg-tag-delete tag)))

(defun ekg-clean-db ()
  "Clean all useless or malformed data from the database.
Some of this is tags which have no uses, which we consider
useless.  This will always make a backup, regardless of backup
settings, and will not delete any backups, regardless of other
settings.

In general, this isn't necessary to run, but it may help if you
have a lot of tags that you no longer use, or feel like your
database is bigger than it should be.

Specifically, this does a few things:

1) Calls `ekg-remove-unused-tags' to remove all tags that no note
is using.

2) Deletes any notes that have no content or almost no content,
as long as those notes aren't on resources that are interesting.

3) Deletes all trashed notes.

4) Fixes any duplicate tags."
  (interactive)
  (ekg-connect)
  (ekg-backup t)
  (ekg-remove-unused-tags)
  (cl-loop for id in (triples-subjects-of-type ekg-db 'text) do
           (let ((note (ekg-get-note-with-id id))
                 (deleted))
             (unless (ekg-note-text note)
               ;; As a heuristic, if this note is sufficiently weird that
               ;; there's no creation date, delete it, otherwise try to fix it.
               (if (null (ekg-note-creation-time note))
                   (progn
                     (message "ekg-clean-db: Deleting note %s, reason: no text or creation date" id)
                     (ekg-note-delete note)
                     (setq deleted t))
                 (message "ekg-clean-db: Fixed nil text for note %s" id)
                 (setf (ekg-note-text note) "")
                 (condition-case nil
                     (ekg-save-note note)
                   (error
                    (message "ekg-clean-db: Deleting note %s, reason: error saving note, too corrupted" id)
                    (ekg-note-delete note)
                    (setq deleted t)))))
             (unless deleted
               (let ((trashed-note (member ekg-trash-tag (ekg-note-tags note)))
                     (almost-empty-note (string= (string-trim (ekg-note-text note)) "*"))
                     (empty-note (string= (string-trim (ekg-note-text note)) ""))
                     (note-without-properties (null (ekg-note-properties note))))
                 (when (and
                        (not (ekg-should-show-id-p id))
                        (or trashed-note (and note-without-properties
                                              (or almost-empty-note empty-note))))
                   (message "ekg-clean-db: Deleting note %s, reason: %s" id
                            (mapconcat #'identity
                                       (seq-filter #'identity (list (when trashed-note "trashed")
                                                                    (when almost-empty-note "almost empty")
                                                                    (when empty-note "empty"))) ", "))
                   (ekg-note-delete note))))))
  (ekg-clean-dup-tags)
  (ekg-clean-leftover-types))

;; Links for org-mode
(require 'ol)

(defun ekg--link-for-tag (tag)
  "Return a link for a single TAG."
  (format "ekg-tag:%s" tag))

(defun ekg--store-any-tags-link ()
  "Store a link to an any-tags ekg page."
  (when (and (eq major-mode 'ekg-notes-mode) (> (length ekg-notes-tags) 1))
    ;; TODO: Stop assuming every notes mode with multiple tags is an any tags.
    (org-link-store-props :type "ekg-tags-any" :link (concat "ekg-tags-any:" (format "%S" ekg-notes-tags))
                          :description (format "EKG page for any of the tags: %s"
                                               (mapconcat #'identity ekg-notes-tags ", ")))))

(defun ekg-edit-note-display-text ()
  "From an edit or capture mode buffer, return display text.
The display text is the text with all inlines executed, without
any metadata text."
  (let* ((text (buffer-substring (+ 1 (overlay-end (ekg--metadata-overlay)))
                                 (point-max)))
         (ticons (ekg-extract-inlines text)))
    (ekg-insert-inlines-results (car ticons) (cdr ticons) nil)))

(defun ekg--open-any-tags-link (stags)
  "Open a link to an ekg page given by STAGS.
STAGS is a string version of a tag, as stored in a link."
  (let ((tags (read stags)))
    (if (= 1 (length tags))
        (ekg-show-notes-with-tag (car tags))
      (ekg-show-notes-with-any-tags tags))))

(defun ekg--store-note-link ()
  "Store a link to an individual note."
  (let ((id (when (member 'ekg-edit-mode local-minor-modes)
              (ekg-note-id ekg-note))))
    (when id
      (org-link-store-props :type "ekg-note"
                            :link (concat "ekg-note:" (format "%S" id))
                            :description (format "EKG note: %S" id)))))

(defun ekg--store-tag-link ()
  "Store a link to an any-tags ekg page."
  (when (and (eq major-mode 'ekg-notes-mode) (= (length ekg-notes-tags) 1))
    ;; TODO: Stop assuming every notes mode with multiple tags is an any tags.
    (org-link-store-props :type "ekg-tags-any" :link (ekg--link-for-tag (car ekg-notes-tags))
                          :description (format "EKG page for tag: %s" (car ekg-notes-tags)))))

(defun ekg--open-tag-link (tag)
  "Open a link to an ekg page given by TAG."
  (ekg-show-notes-with-tag tag))

(defun ekg--open-note-link (id)
  "Open a link to a note given its ID."
  (ekg-edit (ekg-get-note-with-id (read id))))

(org-link-set-parameters "ekg-tag" :follow #'ekg--open-tag-link
                         :store #'ekg--store-tag-link)

(org-link-set-parameters "ekg-tags-any" :follow #'ekg--open-any-tags-link
                         :store #'ekg--store-any-tags-link)

(org-link-set-parameters "ekg-note" :follow #'ekg--open-note-link
                         :store #'ekg--store-note-link)

(provide 'ekg)

;;; ekg.el ends here
