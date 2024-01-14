;;; ekg-denote.el --- ekg and denote integration -*- lexical-binding: t -*-

;; Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>

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
;; This package provides integration between ekg and denote.
;;
;; During export, for each ekg note, a denote file is created. Denote
;; does not allow creation time for two notes within a second whereas
;; ekg has no such restriction, so it is necessary to ensure that each
;; ekg note has a unique creation time for export. A helper function
;; is provided to update the ekg notes and give them unique creation
;; times which differ atleast by a second. Additionally, denote embeds
;; the title and tags in the filename, which is limited based on the
;; underlying operating system. The titles and tags of ekg notes are
;; trimmed to a configurable length before export. Ekg notes can have
;; creation time within a second when trying to bulk import org-roam
;; files to ekg.
;;
;; During import, for each denote file, an ekg note is created. Note
;; updated both in ekg and denote after last import cannot be
;; imported. Such notes has to be manually fixed for import to run.
;; Deleted denote files do not remove the corresponding ekg note. Such
;; ekg notes has to be manually deleted.

;; Eval to remove duplicates
;;
;; (ekg-denote-export-fix-duplicate-notes
;;  (ekg-denote-export--get-duplicate-notes       
;;   (ekg-denote-export-notes-modified-since 0))) ;; 0 means all notes

(require 'ekg)
(require 'denote)
(require 'triples)
(require 'seq)

(defcustom ekg-denote-title-max-len 50
  "Maximum length of the title to trim during export."
  :type 'integer
  :group 'ekg-denote)

(defcustom ekg-denote-combined-kws-len 150
  "Maximum length of the combined kws used for trimming kws when converting tags to kws during export."
  :type 'integer
  :group 'ekg-denote)

(defun ekg-denote-connect ()
  "Connect to ekg and ensure denote schema is set up."
  (ekg-connect)
  (ekg-denote-add-schema))

(defun ekg-denote-add-schema ()
  "Add the denote schema to the ekg database."
  (triples-add-schema ekg-db 'denote
		      '(last-export :base/unique t :base/type integer)
		      '(last-import :base/unique t :base/type integer)))

(defun ekg-denote-get-last-export ()
  "Get the last export time."
  (or (plist-get
       (triples-get-type ekg-db 'denote 'denote)
       :last-export) 0))

(defun ekg-denote-get-last-import ()
  "Get the last import time."
  (or (plist-get
       (triples-get-type ekg-db 'denote 'denote)
       :last-import) 0))

(defun ekg-denote-set-last-export (time)
  "Set the last export time to TIME."
  (let ((plist (triples-get-type ekg-db 'denote 'denote)))
    (apply #'triples-set-type ekg-db 'denote 'denote
	   (plist-put plist :last-export (floor (float-time time))))))

(defun ekg-denote-set-last-import (time)
  "Set the last import time to TIME."
  (let ((plist (triples-get-type ekg-db 'denote 'denote)))
    (apply #'triples-set-type ekg-db 'denote 'denote
	   (plist-put plist :last-import (floor (float-time time))))))

(defun ekg-denote-triples-get-rows-modified-since (time)
  "Return rows modified since TIME from ekg-db."
  (let ((pred (if (= 0 since-time) :time-tracked/creation-time :time-tracked/modified-time)))
    (triples-db-select-pred-op ekg-db pred '> since-time)))

(defun ekg-denote--notes-modified-since (time)
  "Return notes modified since TIME."
  (mapcar #'ekg-get-note-with-id
	  (mapcar #'car (ekg-denote-triples-get-rows-modified-since time))))

(defun ekg-denote--triples-update-creation-time ())

(defun ekg-denote-fix-duplicate-notes-creation-time ()
  
  )

(defun ekg-denote-export-fix-duplicate-notes (notes)
  "Fix duplicate notes out of the given NOTES list of list containing note-id and creation-time."
  (dolist (note (ekg-denote-export--get-duplicate-notes notes))
    (let ((note-id (car note))
	  (updated-creation-time (cdr note)))
      (message "Updating note:%s with creation-time:%s" note-id updated-creation-time)
      (triples-set-type ekg-db (ekg-note-id note) 'time-tracked
			:creation-time updated-creation-time))))

(defun ekg-denote-export--get-duplicate-notes (notes)
  "Return list of duplicate notes' id and creation-time out of the given ekg NOTES."
  (let* ((creation-times '())
	 (duplicates '()))
    (dolist (note notes)
      (let* ((creation-time (ekg-note-creation-time note))
	     (updated-creation-time creation-time))
	(while (cl-member updated-creation-time creation-times :test #'equal)
	  (setq updated-creation-time (time-add updated-creation-time (seconds-to-time 1))))
	(when (not (equal creation-time updated-creation-time))
	  (message "Found duplicate note:%s with creation-time:%s" note-id creation-time)
	  (push (cons note-id updated-creation-time) duplicates))
	(push updated-creation-time creation-times)))
    duplicates))

(defun ekg-denote--sublist-kws (kws combined-length)
  "Return the sublist for the given KWS list such that the
length of combined KWS is not more than the given COMBINED-LENGTH."
  (if (length< (denote--keywords-combine kws) combined-length) kws
    (ekg-denote-export--sublist-kws (butlast kws) combined-length)))

(cl-defstruct ekg-denote-file
  "Representation of denote file."
  id note-id text kws title path)

(defun ekg-denote--file-create (note)
  "Create a new `ekg-denote-file' from given NOTE."
  (let* ((id (format-time-string denote-id-format (ekg-note-creation-time note)))
	 (note-id (ekg-note-id note))
	 (text (or (ekg-note-text note) ""))
	 (ext (if (eq ekg-capture-default-mode 'org-mode) ".org" ".md"))
	 (kws (ekg-denote--sublist-kws
	       (denote-sluggify-keywords (ekg-note-tags note)) ekg-denote-combined-kws-len))
	 (ekg-title (or (car (plist-get (ekg-note-properties note) :titled/title)) ""))
	 (title (string-limit (denote-sluggify ekg-title) ekg-denote-title-max-len))
	 (path (denote-format-file-name (file-name-as-directory denote-directory) id kws title ext)))
    (make-ekg-denote-file :id id
			  :note-id note-id
			  :text text
			  :kws kws
			  :title title
			  :path path)))

(defun ekg-denote--file-rename-existing (file)
  (let* ((id (ekg-denote-file-id file))
	 (path (ekg-denote-file-path path))
	 (existing-path (denote-get-path-by-id id)))
    (when (and existing-path (not (string= existing-path path)))
      (denote-rename-file-and-buffer existing-path path))))

(defun ekg-denote--file-save (file)
  "Save the given FILE to the disk."
  (ekg-denote--file-rename-existing file)
  (let ((path (ekg-denote-file-path file))
	(text (ekg-denote-file-text file))
	(title (ekg-denote-file-title file))
	(kws (ekg-denote-file-kws file)))
    (with-temp-file path (insert text))
    (denote-add-front-matter path title kws)))

(defun ekg-denote--file-create-save (note)
  "Create file from NOTE and then save it."
  (ekg-denote--file-save (ekg-denote--file-create note)))

(defun ekg-denote--has-dups (sequence)
  "Returns t if SEQUENCE has duplicates."
  (not (eq sequence (seq-uniq sequence))))

(defun ekg-denote--assert-notes-have-unique-creation-time (notes)
  "Raise error if NOTES are using duplicate creation-time.

Unique creation time is needed as denote uses creation-time to
create note ID."
  (when (ekg-denote--has-dups (mapcar #'ekg-note-creation-time notes))
    (error "ekg-denote: Notes using same creation time.")))

(defun ekg-denote-export ()
  "Export the current ekg database to denote.

Because this overwrites denote data, running this by itself
should only be done if your denote is meant as a read-only copy
of your ekg database. If you intend to add to your denote, or you
have already have information in denote, you should run
`ekg-denote-sync' instead."
  (interactive)
  (ekg-denote-connect)
  (let* ((last-export-time (ekg-denote-get-last-export))
	 (notes (ekg-denote--notes-modified-since last-export-time)))
    (ekg-denote--assert-notes-have-unique-creation-time notes)
    (mapc #'ekg-denote--file-create-save notes)))

(defun ekg-denote-import ()
  "Import denote files to ekg database by creating/modifying ekg
notes, no deletions. Deletions has to be manually done."
  (interactive)
  ;; Force a backup pre-import.
  (triples-backup ekg-db ekg-db-file most-positive-fixnum)
  (let* ((last-import-time (ekg-denote-get-last-import))
	 (files (cl-remove-if-not
		 (lambda (file)
		   (time-less-p last-import-time) (file-attribute-modification-time (file-attributes file)))
		 (denote-directory-text-only-files))))
    (message "ekg-denote-import: Importing files since last-import-time: %s" last-import-time)
    (cl-loop
     for file in files do
     (let* ((creation-time (time-convert (encode-time (parse-time-string (denote-retrieve-filename-identifier file))) 'integer ))
	    (modified-time (time-convert (file-attribute-modification-time (file-attributes file)) 'integer))
	    (title (denote-retrieve-filename-title file))
	    (file-type (denote-filetype-heuristics file))
	    (kws (denote-retrieve-keywords-value file file-type))
	    (mode (if (equal 'org file-type) 'org-mode 'md-mode))
	    (content (with-temp-buffer
	       (insert-file-contents file)
	       (buffer-string)))
	    (triples (triples-db-select-pred-op
		      ekg-db :time-tracked/creation-time '=
		      creation-time))
	    (note (if (and triples (length= triples 1))
		      (ekg-get-note-with-id (car (car triples)))
		    (ekg-note-create content mode kws))))

       (prin1 triples)

       (message "ekg-denote-import: Importing file: %s, file-creation-time: %s, file-modification-time: %s, title: %s, kws: %s"
		file creation-time modified-time title kws)

       (if (and triples (length= triples 1))
	   (message "ekg-denote-import: Updating existing note. Details: note-id:%s for file:%s" (ekg-note-id note) file)
	 (message "ekg-denote-import: Creating new note for file: %s" file))

       (if (and triples (length> triples 1))
	   (error "ekg-denote-import: Found more than one note for the creation time: %s. Cannot continue.." creation-time))

       ;; for modification, make sure that ekg note does not have a latest update.
       (if (and triples (time-less-p modified-time (ekg-note-modified-time note)))
	   (progn
	     (message "ekg-denote-import: Ekg note is latest than denote file. Update denote file to the latest to continue with import. \
Details: note-id:%s, file:%s, creation-time:%s, note-modified-time:%s, file-modified-time:%s"
		    (ekg-note-id note) file creation-time (ekg-note-modified-time note) modified-time)
	     (error "ekg-denote-import: Note is latest than file.")))

       (if (and triples (time-equal-p modified-time (ekg-note-modified-time note)))
	   (message "ekg-denote-import: Skipping import as modification time is same. Details: note-id:%s, file:%s, note-modified-time:%s, file-modified-time:%s"
		    (ekg-note-id note) file (ekg-note-modified-time note) modified-time))

       ;; update note details
       (setf (ekg-note-tags note) kws
	     (ekg-note-text note) content
	     (ekg-note-modified-time note) modified-time
	     (ekg-note-creation-time note) creation-time)
       (when title
	 (setf (ekg-note-properties note)
	       (plist-put (ekg-note-properties note) :titled/title title)))
       

       
       (ekg-save-note note)
       ;; `ekg-save-note' updates the modification time to
       ;; current-time, so update the modification time using triples
       ;; with existing note id.

       ;; TODO: this is needed for new notes as well
       (if triples (triples-set-type ekg-db (ekg-note-id note) 'time-tracked
				     :modified-time modified-time))))))

(defun ekg-denote-sync ()
  "Sync ekg and denote.

This will import from denote to populate anything new into ekg,
then export ekg so that denote is up to date."
  (interactive)
  (ekg-denote-connect)
  (ekg-denote-import)
  (ekg-denote-export))

(provide 'ekg-denote)
;;; ekg-denote.el ends here.
