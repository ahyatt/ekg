;;; ekg-denote.el --- ekg and denote integration -*- lexical-binding: t -*-

;; Copyright (c) 2024  Jay Rajput <jayrajput@gmail.com>

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
;; During export, for each ekg note, a denote file is created.  Denote
;; does not allow creation time for two notes within a second whereas
;; ekg has no such restriction, so it is necessary to ensure that each
;; ekg note has a unique creation time for export.  Additionally,
;; denote embeds the title and tags in the filename, which is limited
;; based on the underlying operating system.  The titles and tags of
;; ekg notes are trimmed to a configurable length before export.  Ekg
;; notes can have creation time within a second when trying to bulk
;; import org-roam files to ekg.
;;


(require 'ekg)
(require 'denote)
(require 'triples)
(require 'seq)

;;; Code:

(defcustom ekg-denote-export-title-max-len 50
  "Maximum length of the title to trim to during export."
  :type 'integer
  :group 'ekg-denote-export)

(defcustom ekg-denote-export-combined-keywords-len 150
  "Maximum length of the combined keywords used for trimming kws when converting tags to keywords during export."
  :type 'integer
  :group 'ekg-denote-export)

(defcustom ekg-denote-export-add-front-matter nil
  "Whether front-matter is added or not to denotes during export."
  :type 'boolean
  :group 'ekg-denote-export)

(defcustom ekg-denote-export-backup-on-conflict t
  "Whether to backup denotes using `backup-buffer' on conflict during export.
You can choose not to backup in case denotes are already backed up using git or in other usecases."
  :type 'boolean
  :group 'ekg-denote-export)

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

(defun ekg-denote-set-last-export (time)
  "Set the last export time to TIME."
  (let ((plist (triples-get-type ekg-db 'denote 'denote)))
    (apply #'triples-set-type ekg-db 'denote 'denote
	   (plist-put plist :last-export (floor (float-time time))))))

(defun ekg-denote--triples-get-rows-modified-since (time)
  "Return rows modified since TIME."
  (let ((pred (if (= 0 time) :time-tracked/creation-time :time-tracked/modified-time)))
    (triples-db-select-pred-op ekg-db pred '>= time)))

(defun ekg-denote--notes-modified-since (time)
  "Return notes modified since TIME."
  (let* ((rows (ekg-denote--triples-get-rows-modified-since time))
	 (ids (mapcar #'car rows))
	 (notes (mapcar #'ekg-get-note-with-id ids)))
    (remove nil notes)))

(defun ekg-denote-sublist-keywords (kws combined-length)
  "Return the sublist for the given KWS list.
The length of combined KWS is not more than the given
COMBINED-LENGTH."
  (if (length< (denote-keywords-combine kws) combined-length) kws
    (ekg-denote-sublist-keywords (butlast kws) combined-length)))

(cl-defstruct ekg-denote
  "Representation of denote file."
  id note-id text kws title path)

(defun ekg-denote-create (note)
  "Create a new `ekg-denote' from given NOTE."
  (let* ((id (format-time-string denote-id-format (ekg-note-creation-time note)))
	 (note-id (ekg-note-id note))
	 (text (or (ekg-note-text note) ""))
	 (ext (if (eq ekg-capture-default-mode 'org-mode) ".org" ".md"))
	 ;; remove date tag as denote uses date in ID.
	 (tags (seq-filter (lambda (tag)
			     (not (string-prefix-p "date/" tag))) (ekg-note-tags note)))
	 (kws (ekg-denote-sublist-keywords
	       (denote-sluggify-keywords tags) ekg-denote-export-combined-keywords-len))
	 (ekg-title (plist-get (ekg-note-properties note) :titled/title))
	 (ekg-title (if (listp ekg-title) (car ekg-title) ekg-title))
	 (ekg-title (or ekg-title ""))
	 (title (string-limit (denote-sluggify ekg-title) ekg-denote-export-title-max-len))
	 (signature-slug "")
	 (path (denote-format-file-name (file-name-as-directory denote-directory) id kws title ext signature-slug)))
    (make-ekg-denote :id id
		     :note-id note-id
		     :text text
		     :kws kws
		     :title title
		     :path path)))

(defun ekg-denote--backup (denote)
  "Backup given DENOTE."
  (when ekg-denote-export-backup-on-conflict
    (message "ekg-denote: Taking backup of %s" (ekg-denote-path denote))
    (with-current-buffer (find-file-noselect (ekg-denote-path denote))
      (let ((make-backup-files t)
	    (backup-by-copying t)
	    (backup-inhibited nil))
	(backup-buffer)))))

(defun ekg-denote--rename-if-path-changed (denote)
  "Rename given DENOTE if path has changed.
Path can change due to title or tag changes."
  (let* ((id (ekg-denote-id denote))
	 (path (ekg-denote-path denote))
	 (existing-path (denote-get-path-by-id id)))
    (when (and existing-path (not (string= existing-path path)))
      (denote-rename-file-and-buffer existing-path path))))

(defun ekg-denote--text-save (denote)
  "Save the text from given DENOTE to the disk.
Optionally add front-matter."
  (let ((path (ekg-denote-path denote))
	(text (ekg-denote-text denote))
	(title (ekg-denote-title denote))
	(kws (ekg-denote-kws denote)))
    (with-temp-file path (insert text))
    (when ekg-denote-export-add-front-matter
      (denote-add-front-matter path title kws))))

(defun ekg-denote--modified-time-from-file (denote)
  "Return modified time for the DENOTE."
  (let ((path (ekg-denote-path denote)))
    (when (file-exists-p path)
      (time-convert
       (file-attribute-modification-time
	(file-attributes path))
       'integer))))

(defun ekg-denote--note-print (note)
  "Return string representation of NOTE for printing."
  (format "Note ID: %s, Modified: %s, Created: %s, Tags: %s, Title: %s, Text: %s"
	  (ekg-note-id note)
	  (ekg-note-modified-time note)
	  (ekg-note-creation-time note)
	  (ekg-note-tags note)
	  (plist-get (ekg-note-properties note) :titled/title)
	  (truncate-string-to-width (or (ekg-note-text note) "") 100 nil nil "...")))

(defun ekg-denote-assert-notes-have-creation-time (notes)
  "Raise error if NOTES are missing creation-time.
Denote uses creation-time as ID."
  (cl-loop for note in notes do
	   (when (not (ekg-note-creation-time note))
	     (error (format "ekg-denote: note missing creation time: %s" (ekg-denote--note-print note))))))

(defun ekg-denote-assert-notes-have-unique-creation-time (notes)
  "Raise error if NOTES are using duplicate creation-time.
Denote uses creation-time as ID and assume it to be unique."
  (let ((creation-times (mapcar #'ekg-note-creation-time notes)))
    (when (not (equal creation-times (seq-uniq creation-times)))
      (error "ekg-denote: Notes using same creation time"))))

(defun ekg-denote-export ()
  "Export the current ekg database to denote."
  (interactive)
  (ekg-denote-connect)
  (let* ((last-export-time (ekg-denote-get-last-export))
	 (start-time (current-time))
	 (notes (ekg-denote--notes-modified-since last-export-time)))
    (and (ekg-denote-assert-notes-have-creation-time notes)
	 (ekg-denote-assert-notes-have-unique-creation-time notes))
    (message "ekg-denote-export: exporting notes modified since epoch: %s, date-time: %s"
	     last-export-time
	     (format-time-string "%Y%m%dT%H%M%S" last-export-time))
    (cl-loop for note in notes do
	     (message "ekg-denote-export: exporting %s." (ekg-denote--note-print note))
	     (let* ((denote (ekg-denote-create note))
		    (modified-at (ekg-denote--modified-time-from-file denote)))
	       (when (and modified-at (time-less-p last-export-time modified-at))
		 (ekg-denote--backup denote))
	       (ekg-denote--rename-if-path-changed denote)
	       (ekg-denote--text-save denote)))
    (message "ekg-denote-export: completed.")
    (ekg-denote-set-last-export start-time)))

(provide 'ekg-denote)
;;; ekg-denote.el ends here.
