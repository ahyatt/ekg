;;; ekg-logseq.el --- ekg and logseq integration -*- lexical-binding: t -*-

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
;; This exports data to logseq. Because ekg and logseq have somewhat different
;; properties, the mapping process needs to be described. First, because notes
;; in ekg have no titles, they should not be separate logseq notes. Instead, a
;; tag is a logseq note, and contains all notes within it. This means that notes
;; are in multiple tags. We put the note in the first logseq page (tag) we find
;; it in, and then we tag all other tags, which will add it to those pages as
;; well.


(require 'ekg)
(require 'triples)
(require 'seq)
(require 'org nil t)
(require 'org-element nil t)

(declare-function org-element-map "ext:org-element.el")
(declare-function org-element-property "ext:org-element.el")
(declare-function org-element-put-property "ext:org-element.el")
(declare-function org-element-interpret-data "ext:org-element.el")
(declare-function org-element-parse-buffer "ext:org-element.el")
(declare-function org-element-create "ext:org-element.el")
(declare-function org-map-entries "ext:org.el")
(declare-function org-mode "ext:org.el")
(declare-function org-do-demote "ext:org.el")

(defgroup ekg-logseq nil
  "Customization for ekg's logseq integration."
  :group 'ekg)

(defcustom ekg-logseq-dir nil
  "Parent directory for logseq files, contains pages and diary directories."
  :type 'directory
  :group 'ekg-logseq)

(defun ekg-logseq-connect ()
  "Connect to ekg and ensure logseq schema is set up."
  (ekg-connect)
  (ekg-logseq-add-schema))

(defun ekg-logseq-add-schema ()
  "Add the logseq schema to the ekg database."
  (triples-add-schema ekg-db 'logseq
                      '(last-export :base/unique t :base/type integer)
                      '(last-import :base/unique t :base/type integer)))

(defun ekg-logseq-get-last-export ()
  "Get the last export time."
  (or (plist-get
       (triples-get-type ekg-db 'logseq 'logseq)
       :last-export) 0))

(defun ekg-logseq-get-last-import ()
  "Get the last export time."
  (or (plist-get
       (triples-get-type ekg-db 'logseq 'logseq)
       :last-import) 0))

(defun ekg-logseq-set-last-export (time)
  "Set the last export time to TIME."
  (let ((plist (triples-get-type ekg-db 'logseq 'logseq)))
    (apply #'triples-set-type ekg-db 'logseq 'logseq
           (plist-put plist :last-export (floor (float-time time))))))

(defun ekg-logseq-set-last-import (time)
  "Set the last import time to TIME."
  (let ((plist (triples-get-type ekg-db 'logseq 'logseq)))
    (apply #'triples-set-type ekg-db 'logseq 'logseq
           (plist-put plist :last-import (floor (float-time time))))))

(defun ekg-logseq-convert-ekg-tag (tag)
  "Convert an ekg TAG to a logseq tag."
  (if (ekg-date-tag-p tag)
      (replace-regexp-in-string "date/" "" tag) tag))

(defun ekg-logseq-property (name value org-mode-p)
  "Create a logseq property with NAME and VALUE.
If ORG-MODE-P is true, use org-mode syntax."
  (if org-mode-p (format "#+%s: %s\n" name value)
    (format "%s:: %s\n" name value)))

(defun ekg-logseq-primary-tag (tags)
  "From TAGS, return the primary tag.
The primary tag will be the tag in the note is exported in. The
others will have backreferences to the note in logseq.

We just use the first tag that is not a date tag, if it exists."
  (seq-find (lambda (tag) (not (ekg-date-tag-p tag)))
            tags
            (car tags)))

(defun ekg-logseq-hash (text)
  "Return the hash of TEXT."
  (secure-hash 'sha1 text))

(defun ekg-logseq-text-to-logseq (note)
  "Convert a note's text to logseq format.
This inserts the results of inlines, although for note
transclusion, it uses the logseq equivalent."
  (ekg-insert-inlines-and-process
   (ekg-note-text note)
   (ekg-note-inlines note)
   (lambda (i)
     (if (eq (car (ekg-inline-command i)) 'transclude-note)
         (format "{{embed ((%s))}}" (cadr (ekg-inline-command i)))
       (ekg-inline-to-result i note)))))

(defun ekg-logseq-note-to-logseq-org (note tag)
  "Return logseq text to store for NOTE in TAG.
This will store the note text as org-mode, regardless of the mode
of the note."
  (with-temp-buffer
    (org-mode)
    (when (ekg-should-show-id-p (ekg-note-id note))
      (insert (ekg-note-id note) "\n"))
    (insert (ekg-logseq-text-to-logseq note))
    ;; Demote all other headings to level 2.
    (org-map-entries (lambda () (org-do-demote)))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= "ekg" (org-element-property :type link))
          (let ((id (org-element-property :path link)))
            (org-element-put-property link :type "id")
            (org-element-put-property link :path id)))))
    (goto-char (point-min))
    (insert (org-element-interpret-data
             (org-element-create 'headline
                                 `(:level 1 :title ,(or (car (plist-get (ekg-note-properties note) :titled/title))
                                                        "Untitled Note")))))
    ;; Can't figure out how to get org-element to do this for me based off of
    ;; properties in the headline, so let's put the properties on here manually.
    (let ((tag-text (mapconcat (lambda (tag)
                         (format "#[[%s]]"
                                 (ekg-logseq-convert-ekg-tag tag)))
                               (seq-difference
                                (ekg-note-active-tags note) (list tag))
                               " ")))
      (insert (format ":PROPERTIES:\n:ID: %s\n:EKG_HASH: %s\n:END:\n%s%s"
                      (ekg-note-id note) (ekg-logseq-hash (ekg-note-text note))
                      tag-text (if (> (length tag-text) 0) "\n" ""))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ekg-logseq-note-to-logseq-md (note tag)
  "Return logseq text to store for NOTE in TAG.
This will store the note text as markdown, regardless of the mode
of the note."
  (with-temp-buffer
    (insert "- "
            (or (car (plist-get (ekg-note-properties note) :titled/title))
                "Untitled Note")
            "\n  "
            (format "id:: %s\n  ekg_hash:: %s\n  "
                    (ekg-note-id note) (ekg-logseq-hash (ekg-note-text note)))
            (mapconcat (lambda (tag)
                         (format "#[[%s]]"
                                 (ekg-logseq-convert-ekg-tag tag)))
                       (seq-difference (ekg-note-active-tags note) (list tag))
                       " ")
            "\n  "
            (if (ekg-should-show-id-p (ekg-note-id note))
                (format "%s\n  " (ekg-note-id note))
              "")
            (string-replace "\n" "\n  " (ekg-logseq-text-to-logseq note)))
    (string-trim (buffer-substring-no-properties (point-min) (point-max)) nil (rx (1+ blank)))))

(defun ekg-logseq-notes-to-logseq (notes tag org-mode-p)
  "Return logseq text to store for NOTES in TAG.
If ORG-MODE-P is non-nil, store the notes as org-mode, otherwise
store as markdown."
  (concat
   (ekg-logseq-property "title" (ekg-logseq-convert-ekg-tag tag)
                        org-mode-p)
   (ekg-logseq-property "ekg_export" "true" org-mode-p)
   "\n"
   (mapconcat (lambda (note)
               (if org-mode-p
                   (ekg-logseq-note-to-logseq-org note tag)
                 (ekg-logseq-note-to-logseq-md note tag)))
              (seq-filter (lambda (note)
                            (and (equal tag (ekg-logseq-primary-tag (ekg-note-tags note)))
                                 (not (equal "" (ekg-note-text note))))) notes)
             "\n")))

(defun ekg-logseq-tag-to-file (tag)
  "Return text to populate to a logseq file for TAG."
  (with-temp-buffer
    (when (eq ekg-capture-default-mode 'org-mode)
      (org-mode))
    (insert (ekg-logseq-notes-to-logseq
             (sort
              (ekg-get-notes-with-tag tag)
              (lambda (a b)
                (time-less-p (ekg-note-creation-time a) (ekg-note-creation-time b))))
             tag
             (eq major-mode 'org-mode)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ekg-logseq-tag-to-filename (tag)
  "Return the filename for TAG."
  (replace-regexp-in-string
                    "/" "$"
                    (format "%s.%s"
                            (ekg-logseq-convert-ekg-tag tag)
                            (if (eq ekg-capture-default-mode 'org-mode)
                                "org" "md"))))

(defun ekg-logseq-filename-to-tag (filename)
  "Return the tag for FILENAME."
  (replace-regexp-in-string
   (rx "$") "/"
   (file-name-sans-extension (file-name-nondirectory filename))))

(defun ekg-logseq-export-tag (tag)
  "Export TAG to logseq.
This may make files with no content if there are notes with no
backlinks.

Do not overwrite a file if nothing has changed.
Return non-nil if the file was written."
  (let ((contents (ekg-logseq-tag-to-file tag))
        (filename (expand-file-name
                   (ekg-logseq-tag-to-filename tag)
                   (file-name-concat ekg-logseq-dir
                                     (if (ekg-date-tag-p tag)
                                         "journals" "pages")))))
    (unless
        (and
         (file-exists-p filename)
         (string= contents (with-temp-buffer
                             (insert-file-contents filename)
                             (buffer-string))))
      (message "ekg-logseq-export: exporting to file %s" filename)
      (with-temp-file filename
        (insert contents)
        t))))

(defun ekg-logseq--to-import-text ()
  "Return a list of notes to the current buffer to import.
This is the text not marked as exported by ekg. If org-mode, the
buffer is divided into its top level subtrees. Any subtree that
isn't exported by EKG is included.

For markdown, it's the same thing except for list items, which
are always the top-level constructs in markdown mode.

For markdown, remove the leading dash. For org-mode, don't remove
the leading star, because a nested structure beneath seems to
make less sense without it."
  (save-excursion
    (if (eq major-mode 'org-mode)
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (headline)
            (unless (or (org-element-property :ekg_hash headline)
                        (org-element-property :EKG_HASH headline)
                        (> (org-element-property :level headline) 1))
              (buffer-substring-no-properties
               (org-element-property :begin headline)
               (org-element-property :end headline))))
          nil nil 'headline)
      ;; Not org-mode, it must be markdown. Iterate over the top-level list
      ;; items, keeping any that don't have an "ekg_id".
      (let ((pos (point-min))
            (items nil))
        (goto-char (point-min))
        (cl-loop
         do
         ;; According to the docs this should set the point to the end when
         ;; there is no match, but it doesn't seem to happen.
         (unless (re-search-forward "^- " nil t)
           (goto-char (point-max)))
         (let ((text
                (replace-regexp-in-string
                 (rx bol "- ") ""
                 (buffer-substring-no-properties
                  pos (point)))))
           (unless (or
                    ;; Don't take the text pre-first item.
                    (eq pos (point-min))
                    (equal text "")
                    (string-match-p "ekg_hash::" text))
             (push text items))
           (setq pos (match-end 0)))
         until (eq (point) (point-max)))
        (nreverse items)))))

(defun ekg-logseq--to-import-tags (text)
  "Return a list of tags in logseq format to import from TEXT.
We look for strings of the format #tag and #[[tag]]."
  (with-temp-buffer
    (insert text)
    (let ((tags ()))
      (goto-char (point-min))
      (while (re-search-forward
              (rx (or (seq "#" (group-n 1 (1+ alnum)))
                      (seq "#[[" (group-n 1 (1+ (or alnum space))) "]]"))) nil t)
        (when-let ((tag (match-string-no-properties 1)))
          (push tag tags)))
      (nreverse (seq-uniq tags)))))

(defun ekg-logseq--to-import-md-id (text)
  "Return the logseq id from markdown TEXT."
  (when (string-match (rx (seq "id:: " (group-n 1 (1+ (or "-" hex-digit))))) text)
    (match-string-no-properties 1 text)))

(defun ekg-logseq--to-import-org-id (text)
  "Return the logseq id from org TEXT."
    (with-temp-buffer
      (insert text)
      (org-mode)
      ;; There should just be one thing here.
      (car
       (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (org-element-property :ID headline))
        nil nil 'headline))))

(defun ekg-logseq--text-to-note (tag text)
  "Return the note to import from logseq TEXT.
TAG is the current tag being imported in logseq."
  (let* ((in-cons (ekg-extract-inlines
                   (replace-regexp-in-string
                    (rx (seq "{{embed" (* space) "(("
                             (group-n 1 (1+ (not ")"))) "))}}"))
                    "%(transclude-note \"\\1\")" text)))
         (note (ekg-note-create :text (car in-cons)
                                :mode (when (eq major-mode 'org-mode)
                                        'org-mode 'markdown-mode)
                                :tags (cons tag (ekg-logseq--to-import-tags text)))))
    (setf (ekg-note-inlines note)
          ;; These must all be transclusion commands, check each id (the first
          ;; argument) against the database to understand whether it should be
          ;; quoted or not.
          (mapcar (lambda (i)
                    (if (ekg-note-with-id-exists-p (read (nth 1 (ekg-inline-command i))))
                        (progn
                          (setf (ekg-inline-command i)
                                (list 'transclude-note (read (nth 1 (ekg-inline-command i)))))
                          i) i)) (cdr in-cons)))
    (when-let (id (if (eq major-mode 'org-mode)
                      (ekg-logseq--to-import-org-id text)
                    (ekg-logseq--to-import-md-id text)))
      (setf (ekg-note-id note) (if (ekg-note-with-id-exists-p (read id)) (read id) id)))
    note))

(defun ekg-logseq-import ()
  "Import from the current logseq directory.
This will create new nodes based on parts of the logseq pages
that were previously not exported. It will never re-import
something exported, so it's expected that changing things in
logseq will not result in those things showing up in ekg.

An import must be followed by an export, otherwise we can end up
importing the same thing multiple times. Because of this, if you
run this interactively, only run it once if you want an initial
population of data into ekg. Otherwise, run `ekg-logseq-sync',
which will import and re-export back to logseq."
  (unless ekg-logseq-dir
    (error "ekg-logseq-dir must be set"))
  ;; Force a backup pre-import.
  (triples-backup ekg-db ekg-db-file most-positive-fixnum)
  (let ((count 0)
        (last-import (ekg-logseq-get-last-import))
        (start-time (current-time)))
    (message "ekg-logseq-import: importing logseq files changed since %s"
             (if (= last-import 0) "the beginning of time itself"
               (format-time-string "%F %X" last-import)))
    (cl-loop for subdir in '("journals" "pages") do
           (cl-loop for file in
                    (directory-files
                     (file-name-concat ekg-logseq-dir subdir) t
                     (rx (seq "." (or "org" "md") eol))) do
                     ;; Only import files not modified since last-import
                     (when (time-less-p last-import
                                        (nth 5 (file-attributes file)))
                       (let ((tag (concat (if (equal subdir "journals")
                                              "date/" "")
                                          (ekg-logseq-filename-to-tag file))))
                         (with-temp-buffer
                           (insert-file-contents file)
                           (when (equal "org" (file-name-extension file))
                             (org-mode))
                           ;; No need to do the same for markdown, because we
                           ;; don't need any special parsing capabilities.
                           (let ((items (ekg-logseq--to-import-text)))
                             (cl-loop for text in items
                                      do
                                      (message "ekg-logseq-import: saving note from file %s" file)
                                        (cl-incf count)
                                        (ekg-save-note (ekg-logseq--text-to-note tag text)))))))))
    (message "ekg-logseq-import: imported %d notes" count)
    (ekg-logseq-set-last-import start-time)))

(defun ekg-logseq-tags-with-notes-modified-since (time)
  "Return a list of tags with notes modified since TIME."
  (if (= time 0)
      (ekg-tags)
    (seq-filter #'ekg-content-tag-p
     (seq-uniq
      (cl-loop for triples in
               (triples-db-select-pred-op ekg-db :time-tracked/modified-time '> time)
               nconc (ekg-note-tags (ekg-get-note-with-id (car triples))))))))

(defun ekg-logseq-export ()
  "Export the current ekg database to logseq.

Because this overwrites logseq data, running this by itself
should only be done if your logseq is meant as a read-only copy
of your ekg database. If you intend to add to your logseq, or you
have already have information in logseq, you should run
`ekg-logseq-sync' instead."
  (interactive)
  (unless ekg-logseq-dir
    (error "ekg-logseq-dir must be set"))
  (ekg-logseq-connect)
  (let* ((deleted 0)
         (modified 0)
         (export-time (ekg-logseq-get-last-export))
         (start-time (current-time))
         (tags (ekg-logseq-tags-with-notes-modified-since export-time)))
    (cl-loop for subdir in '("journals" "pages") do
             (cl-loop for file in
                      (seq-filter #'file-regular-p
                                  (directory-files
                                   (file-name-concat ekg-logseq-dir subdir) t))
                      do
                      (unless (member (concat (if (equal subdir "journals") "date/" "")
                                              (ekg-logseq-filename-to-tag file)) tags)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (when (string-match
                                 (rx (seq line-start "#+ekg-export: true" line-end))
                                 (buffer-substring-no-properties
                                  (point-min)
                                  (point-max)))
                            (delete-file file)
                            (message "ekg-logseq-export: deleting obsolete previously exported file %s" file)
                            (cl-incf deleted))))))
    (message "ekg-logseq-export: exporting all tags modified since %s: %S"
                      (if (= 0 export-time)
                          "the beginning of time itself"
                        (format-time-string "%F %X" export-time))
                      tags)
    (cl-loop for tag in tags do
             (when (ekg-logseq-export-tag tag)
               (cl-incf modified)))
    (message "ekg-logseq-export: deleted %d files, modified %d files" deleted modified)
    (ekg-logseq-set-last-export start-time)))

(defun ekg-logseq-sync ()
  "Sync ekg and logseq.
This will import from logseq to populate anything new into ekg,
then export ekg so that logseq is up to date, and information in
logseq is marked as being part of logseq.

All logic will be run in the background."
  (interactive)
  (ekg-logseq-connect)
  (message "ekg-logseq-sync: Starting in the background")
  (ekg-logseq-import)
  (ekg-logseq-export))

(provide 'ekg-logseq)
;;; ekg-logseq.el ends here
