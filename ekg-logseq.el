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
(require 'seq)
(require 'org nil t)

(defgroup ekg-logseq nil
  "Customization for ekg's logseq integration."
  :group 'ekg)

(defcustom ekg-logseq-dir nil
  "Parent directory for logseq files, contains pages and diary directories."
  :type 'directory
  :group 'ekg-logseq)

(defun ekg-logseq-convert-ekg-tag (tag)
  "Convert an ekg TAG to a logseq tag."
  (if (ekg-date-tag-p tag)
      (replace-regexp-in-string "date/" "" tag) tag))

(defun ekg-logseq-property (name value)
  "Create a logseq property with NAME and VALUE."
  (if (eq ekg-capture-default-mode 'org-mode)
      (format "#+%s: %s\n" name value)
    (format "%s:: %s\n" name value)))

(defun ekg-logseq-export-tag (tag &optional exported-note-ids)
  "Export TAG to logseq.
EXPORTED-NOTE-IDS is the list of note ids that have already been
exported, so shouldn't be exported again. It will be added to by
this function and returned."
  (with-temp-file (expand-file-name
                   (replace-regexp-in-string
                    "/" "$"
                    (format "%s.%s" (ekg-logseq-convert-ekg-tag tag)
                            (if (eq ekg-capture-default-mode 'org-mode)
                                "org" "md")))
                   (file-name-concat ekg-logseq-dir
                                     (if (ekg-date-tag-p tag)
                                         "journals" "pages")))
    (when (eq ekg-capture-default-mode 'org-mode)
      (org-mode))
    (insert (ekg-logseq-property "title" (ekg-logseq-convert-ekg-tag tag))
            (ekg-logseq-property "ekg-export" "true"))

    (let ((notes (ekg-get-notes-with-tag tag)))
      (cl-loop for note in notes do
               (when (not (member (ekg-note-id note) exported-note-ids))
                 (insert (format (if (eq ekg-capture-default-mode 'org-mode)
                                   "* %s\n" "- %s")
                           (or (plist-get (ekg-note-properties note) :titled/title)
                               "Untitled note")))
                 (if (eq ekg-capture-default-mode 'org-mode)
                     (org-set-property "EKG_ID" (format "%s" (ekg-note-id note)))
                   (insert (ekg-logseq-property "ekg-id" (ekg-note-id note))))
                 (let ((text (concat (string-trim (ekg-note-text note)) "\n"
                                     (mapconcat (lambda (tag)
                                                  (format "[[%s]]"
                                                          (ekg-logseq-convert-ekg-tag tag)))
                                                (seq-difference (ekg-note-tags note)
                                                                (list tag))
                                                " ")
                                     "\n")))
                   (if (and (eq ekg-capture-default-mode 'org-mode)
                            (org-kill-is-subtree-p text))
                       (org-paste-subtree nil text)
                     (insert "\n" text "\n")))
                 (push (ekg-note-id note) exported-note-ids)))))
  exported-note-ids)

(defun ekg-logseq-export ()
  "Export the current ekg database to logseq.
This is a one-way export, everything exported should never be
imported again, or else the ekg database will become corrupted
with duplicate data."
  (interactive)
  (unless ekg-logseq-dir
    (error "ekg-logseq-dir must be set"))
  (let ((date-tags)
        (normal-tags)
        (exported-note-ids))
    (cl-loop for tag in (ekg-tags) do
           (if (ekg-date-tag-p tag)
               (push tag date-tags)
             (push tag normal-tags)))
  ;; Export normal tags first, so date tags will have link to information
  ;; instead of containing information.
  (cl-loop for tag in normal-tags do
           (setq exported-note-ids (ekg-logseq-export-tag tag exported-note-ids)))
  (cl-loop for tag in date-tags do
           (setq exported-note-ids (ekg-logseq-export-tag tag exported-note-ids)))))

(provide 'ekg-logseq)
;;; ekg-logseq.el ends here
