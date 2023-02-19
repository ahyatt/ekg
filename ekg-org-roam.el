;;; ekg-org-roam.el --- Importing and exporting between org-roam and ekg  -*- lexical-binding: t -*-

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
;;
;;; Commentary:
;;
;; These utilities provide a method for handling the import and (eventually) the
;; export of data from org-roam into ekg.

(require 'ekg)
(require 'triples)
(require 'org nil t)
(require 'org-roam nil t)
(require 'org-roam-db nil t)
(require 'org-roam-utils nil t)
(require 'org-roam-dailies nil t)
(require 'rx)
(require 'url-handlers)

;;; Code:

(defvar ekg-org-roam-import-tag-to-prefix nil
  "Tags which need to be prefixed to the title.
For example, if you tag every idea with the `idea' tag, then I
think it's best to not bring that tag to ekg, but instead to
prefix the title with this tag name. This is a list of tags that
should operate like that - so, if one is found (at max one should
ever be found, these should be exclusionary), it turns into a
prefix on the title instead.")

(defvar ekg-org-roam-import-tag-to-ignore nil
  "Tags to ignore and not bring into ekg.")

;; Declarations to remove byte compilation warnings.
(declare-function org-roam-node-from-id "ext:org-roam-node.el")
(declare-function org-roam-node-title "ext:org-roam-node.el")
(declare-function org-roam-node-tags "ext:org-roam-node.el")
(declare-function org-roam-node-list "ext:org-roam-node.el")
(declare-function org-roam-node-file "ext:org-roam-node.el")
(declare-function org-roam-node-level "ext:org-roam-node.el")
(declare-function org-roam-node-point "ext:org-roam-node.el")
(declare-function org-roam-node-id "ext:org-roam-node.el")
(declare-function org-roam-node-refs "ext:org-roam-node.el")
(declare-function org-roam-with-file "ext:org-roam-utils.el")
(declare-function org-roam-db-query "ext:org-roam-db.el")
(declare-function org-narrow-to-element "ext:org.el")
(defvar org-roam-directory)
(defvar org-roam-dailies-directory)

(defun ekg-org-roam-import-title-to-tag (title tags)
  "From a TITLE and TAGS compute the new title according to prefix rules."
  (if (iso8601-valid-p title)
      (format "date/%s" title)
    (let ((tags (append tags)))
      (if-let (diff (seq-intersection tags
                                      ekg-org-roam-import-tag-to-prefix))
          (if (= 1 (length diff))
              (format "%s/%s" (car diff) title)
                                      (warn "Unexpectedly found more than one tag in `ekg-org-roam-import-tag-to-prefix' in tags for node %s.  Tags: %s."
                                            title tags)
                                      title)
        title))))

(defun ekg-org-roam-import--tags-from-links ()
  "From the current buffer, return tags from the links found.
The links are turned into tags, without regards for ignored tags.
However, we do pay attention to
`ekg-org-roam-import-tag-to-prefix'."
  (let ((tags-from-links))
    (while (re-search-forward org-link-bracket-re nil t)
      (let* ((type-val (split-string (substring-no-properties (match-string 1)) ":")))
        (when (string-equal (downcase (car type-val)) "file")
          (when-let ((node (org-roam-node-from-id (caar (org-roam-db-query [:select [id] :from nodes
                                                                                    :where (= file $s1)
                                                                                    :and (= level 0)]
                                                                           (cadr type-val))))))
            (cl-pushnew (ekg-org-roam-import-title-to-tag (org-roam-node-title node) (org-roam-node-tags node)) tags-from-links)))
        (when (string-equal (downcase (car type-val)) "id")
          (when-let (linked-node (org-roam-node-from-id (cadr type-val)))
            (cl-pushnew (ekg-org-roam-import-title-to-tag (org-roam-node-title linked-node) (org-roam-node-tags linked-node)) tags-from-links)))))
    tags-from-links))

(defun ekg-org-roam-import-logseq (pages-dir journal-dir)
  "Import Logseq data from PAGES-DIR and JOURNAL-DIR.
This assumes that org-roam is also being used alongside logseq.
Only data that org-roam doesn't have in its cache is imported,
the rest should have been imported with `ekg-org-roam-import'.
JOURNAL-DIR should be relative to PAGES-DIR."
  (triples-add-schema ekg-db 'logseq '(file (:base/unique t :base/type string)))
  (cl-loop for dir in (list pages-dir (concat (file-name-as-directory pages-dir) journal-dir))
           do
           (cl-loop for file in (directory-files dir) do
                    (unless (or
                             (string-match (rx (seq string-start ?\.)) file)
                             (not (member (file-name-extension file) '("org" "txt" "md")))
                             (org-roam-db-query [:select [id] :from nodes :where (= file $s1)] (concat (file-name-as-directory pages-dir) file))
                             (org-roam-db-query [:select [id] :from nodes :where (= file $s1)] (concat (file-name-as-directory journal-dir) file)))
                      (let ((title
                             (if (string-match-p "journal" dir) (string-replace "_" "-" (file-name-base file))
                               (replace-regexp-in-string (rx (seq string-start (one-or-more digit) ?\-)) "" (file-name-base file))))
                            (filename (concat (file-name-as-directory dir) file)))
                        (save-excursion
                          (find-file filename)
                          (unless (= 0 (length (string-trim (buffer-string))))
                            (let ((tags))
                              (when (re-search-forward (rx (seq line-start "TAGS:" (group (zero-or-more not-newline)) line-end)) nil t)
                                (setq tags (split-string (match-string 1))))
                              (font-lock-ensure)
                              (triples-with-transaction ekg-db
                                (let* ((note (ekg-note-create
                                              (buffer-string)
                                              major-mode
                                              (seq-difference (cons (ekg-org-roam-import-title-to-tag title tags) tags)
                                                              (seq-union ekg-org-roam-import-tag-to-ignore
                                                                         ekg-org-roam-import-tag-to-prefix
                                                                         #'equal)
                                                              #'equal))))
                                  (setf (ekg-note-id note) filename)
                                  (ekg-save-note note)
                                  (triples-set-type ekg-db (ekg-note-id note) 'logseq `(:file ,filename))))))))))))

(defun ekg-org-roam-import ()
  "Import all data from org-roam into ekg."
  (ekg--connect)
  (triples-add-schema ekg-db 'org-roam '(id (:base/unique t)))
  (dolist (node (org-roam-node-list))
    (save-excursion
      (org-roam-with-file (org-roam-node-file node) nil
        (goto-char (org-roam-node-point node))
        (when (> (org-roam-node-level node) 0)
          (condition-case nil
              (org-narrow-to-element)
            (error nil)))
        (let ((tags-from-links)
              (text (buffer-substring (save-excursion
                                        (goto-char (point-min))
                                        (font-lock-ensure)
                                        (point)) (point-max))))
          (unless (triples-subjects-with-predicate-object ekg-db 'org-roam/id (org-roam-node-id node))
            (setq tags-from-links (ekg-org-roam-import--tags-from-links))
            (triples-with-transaction ekg-db
              (let* ((note (ekg-note-create
                           text
                           'org-mode
                           (seq-difference (seq-uniq
                                            (cons
                                             (ekg-org-roam-import-title-to-tag (org-roam-node-title node) (org-roam-node-tags node))
                                             tags-from-links))
                                           (seq-union ekg-org-roam-import-tag-to-ignore
                                                      ekg-org-roam-import-tag-to-prefix
                                                      #'equal)
                                           #'equal))))
                (setf (ekg-note-id note) (org-roam-node-id node))
                (when (org-roam-node-refs node)
                  (setf (ekg-note-properties note) `(:reference/url ,(org-roam-node-refs node))))
                (ekg-save-note note)
                (triples-set-type ekg-db (ekg-note-id note) 'org-roam `(:id ,(org-roam-node-id node))))))))))
  (ekg-org-roam-import-logseq org-roam-directory org-roam-dailies-directory))

(provide 'ekg-org-roam)

;;; ekg-org-roam.el ends here
