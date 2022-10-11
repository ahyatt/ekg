;;; ekg-org-roam-import.el --- Function to import all data from org-roam into ekg

;;; Commentary:
;; 

(require 'org-roam)
(require 'org-roam-utils)
(require 'url-handlers)

(defvar ekg-org-roam-import-tag-to-prefix nil
  "Tags, whose presence indicates that they should be prefixes to
the title. For example, if you tag every idea with the 'idea'
tag, then I think it's best to not bring that tag to ekg, but
instead to prefix the title with this tag name. This is a list of
tags that should operate like that - so, if one is found (at max
one should ever be found, these should be exclusionary), it turns
into a prefix on the title instead.")

(defvar ekg-org-roam-import-tag-to-ignore nil
  "Tags to ignore and not bring into ekg.")

(defun ekg-org-roam-import-title-to-tag (node)
  "Turn NODE's title into tag according to prefix rules."
  (if-let (diff (seq-intersection (org-roam-node-tags node)
                                  ekg-org-roam-import-tag-to-prefix))
      (if (= 1 (length diff))
          (format "%s/%s" (car diff) (org-roam-node-title node))
                                  (warn "Unexpectedly found more than one tag in `ekg-org-roam-import-tag-to-prefix' in tags for node %s.  Tags: %s."
                                        (org-roam-node-title node) (org-roam-node-tags node))
                                  (org-roam-node-title node))
    (org-roam-node-title node)))

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
            (while (re-search-forward org-link-bracket-re nil t)
              (let* ((mdata (match-data))
                     (type-val (split-string (substring-no-properties (match-string 1)) ":")))
                (when (string-equal-ignore-case (car type-val) "file")
                  (when-let ((node (org-roam-node-from-id (caar (org-roam-db-query [:select [id] :from nodes
                                                                                    :where (= file $s1)
                                                                                    :and (= level 0)]
                                                                                    (cadr type-val))))))
                    (add-to-list 'tags-from-links (ekg-org-roam-import-title-to-tag node))))
                (when (string-equal-ignore-case (car type-val) "id")
                  (when-let (linked-node (org-roam-node-from-id (cadr type-val)))
                    (add-to-list 'tags-from-links (ekg-org-roam-import-title-to-tag linked-node))))))
            (emacsql-with-transaction ekg-db
              (let* ((note (ekg-note-create
                           text
                           'org-mode 
                           (seq-difference (seq-uniq
                                            (cons
                                             (ekg-org-roam-import-title-to-tag node)
                                             tags-from-links))
                                           (seq-union ekg-org-roam-import-tag-to-ignore
                                                      ekg-org-roam-import-tag-to-prefix
                                                      #'equal)
                                           #'equal))))
                (setf (ekg-note-id note) (org-roam-node-id node))
                (when (org-roam-node-refs node)
                  (setf (ekg-note-properties note) `(:reference/url ,(org-roam-node-refs node))))
                (ekg-save-note note)
                (triples-set-type ekg-db (ekg-note-id note) 'org-roam `(:id ,(org-roam-node-id node)))))))))))

(provide 'ekg-org-roam-import)
