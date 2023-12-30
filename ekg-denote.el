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
  "Get the last export time."
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

(defun ekg-denote-export-notes-modified-since (time-threshold)
  "Return a list of notes modified since TIME-THRESHOLD."
  (let ((creation-predicate (if (= 0 time-threshold)
				:time-tracked/creation-time
			      :time-tracked/modified-time)))
    (let ((modified-notes (triples-db-select-pred-op ekg-db creation-predicate '> time-threshold)))
      (mapcar
       (lambda (item)
	 (ekg-get-note-with-id (car item)))
       modified-notes))))

(defun ekg-denote-export-fix-duplicate-notes (notes)
  "Fix duplicate notes out of the given NOTES."
  (dolist (note (ekg-denote-export--get-duplicate-notes notes))
    (let ((note-id (car note))
	  (updated-creation-time (cdr note)))
      (message "Updating note:%s with creation-time:%s" note-id updated-creation-time)
      (triples-set-type ekg-db (ekg-note-id note) 'time-tracked
			:creation-time updated-creation-time))))

(defun ekg-denote-export--get-duplicate-notes (notes)
  "Return duplicate notes out of the given NOTES."
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

(defun ekg-denote-export--sublist-kws (kws combined-length)
  "Return the sublist for the given KWS list such that the
length of combined KWS is not more than the given COMBINED-LENGTH."
  (if (length> (denote--keywords-combine kws) combined-length)
      (ekg-denote-export--sublist-kws (butlast kws) combined-length)
    kws))

(defun ekg-denote-export ()
  "Export the current ekg database to denote.

Because this overwrites denote data, running this by itself
should only be done if your denote is meant as a read-only copy
of your ekg database. If you intend to add to your denote, or you
have already have information in denote, you should run
`ekg-denote-sync' instead."
  (interactive)
  (ekg-denote-connect)
  (let* ((export-time (ekg-denote-get-last-export))
	 (notes (ekg-denote-export-notes-modified-since export-time))
	 (duplicates (ekg-denote-export--get-duplicate-notes notes))
	 (start-time (current-time)))
    (if duplicates
	(error "ekg-denote-export: Duplicate notes found. Fix duplicates to continue."))
    (cl-loop
     for note in notes do
     (let* ((note-id (ekg-note-id note))
	    (id (format-time-string denote-id-format (ekg-note-creation-time note)))
	    (tags (ekg-note-tags note))
	    (kws (ekg-denote-export--sublist-kws
		  (denote-sluggify-keywords tags) ekg-denote-combined-kws-len))
	    (title (string-limit (denote-sluggify
				  (or (car (plist-get (ekg-note-properties note) :titled/title)) ""))
				 ekg-denote-title-max-len))
	    (ext (if (eq ekg-capture-default-mode 'org-mode) ".org" ".md"))
	    (old-denote-file (cl-find id (denote-directory-text-only-files)
				      :test #'(lambda (substr str)
						(string-prefix-p substr (file-name-nondirectory str)))))
	    (filename (denote-format-file-name denote-directory id kws title ext)))
       (message "ekg-denote-export: Exporting note:%s with title:%s and tags:%S to file:%s"
		note-id title tags filename)
       (when old-denote-file (denote-rename-file-and-buffer old-denote-file filename))
       (with-temp-file filename
	 (insert (or (ekg-note-text note) "")))
       (denote-add-front-matter filename title kws)))))

(defun ekg-denote-import ()
  "Import denote files to ekg database by creating/modifying ekg notes."
  (interactive)
  ;; Force a backup pre-import.
  (triples-backup ekg-db ekg-db-file most-positive-fixnum)
  (let* ((last-import-time (ekg-denote-get-last-import))
	 (start-time (current-time))
	 (files (cl-remove-if-not
		 (lambda (file)
		   (time-less-p last-import-time (nth 5 (file-attributes file))))
		 (denote-directory-text-only-files))))
    (cl-loop
     for file in files do
     (let* ((creation-time (floor (float-time (apply #'encode-time (parse-time-string (denote-retrieve-filename-identifier file))))))
	    (modified-time (floor (float-time (nth 5 (file-attributes file)))))
	    (title (when (string-match denote-title-regexp file) (match-string 1 file)))
	    (file-type (denote-filetype-heuristics file))
	    (kws (denote-retrieve-keywords-value file file-type))
	    (mode (if (equal 'org file-type) 'org-mode 'md-mode))
	    (content
	     (with-temp-buffer
	       (insert-file-contents file)
	       (goto-char (point-min))
	       (when (re-search-forward "^$" nil 'noerror)
		 (delete-region (point-min) (1+ (point))))
	       (string-trim-left (buffer-string))))
	    (triples (triples-db-select-pred-op ekg-db :time-tracked/creation-time '=
						creation-time))
	    (note (if (and triples (= (length triples) 1))
		      (ekg-get-note-with-id (car (car triples)))
		    (ekg-note-create content mode kws))))

       (if (time-less-p modified-time (ekg-note-modified-time note))
	   (error "ekg-denote-import: Both ekg note and denote file found modified. Ekg note:%s, denote file:%s" (ekg-note-id note) file))
       
       (setf (ekg-note-tags note) kws
	     (ekg-note-text note) content
	     (ekg-note-modified-time note) modified-time
	     (ekg-note-creation-time note) creation-time)
       (when title
	 (setf (ekg-note-properties note)
	       (plist-put (ekg-note-properties note) :titled/title title)))
       (message "ekg-denote-import: Importing File:%s to Note:%s with title:%s tags:%s modified-time:%s creation-time:%s"
		file (ekg-note-id note) title kws modified-time creation-time)
       (ekg-save-note note)))))

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
