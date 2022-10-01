;;; ekg-org-roam-import.el --- Function to import all data from org-roam into ekg

;;; Commentary:
;; 

(require 'org-roam)
(require 'org-roam-utils)

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
                     (path (match-string 1)))
                (when (string-prefix-p "file:" path)
                  (setq path (expand-file-name (substring path 5)))
                  (when-let ((node-title (caar (org-roam-db-query [:select [title] :from nodes
                                                                           :where (= file $s1)
                                                                           :and (= level 0)] path))))
                    (set-match-data mdata)
                    (add-to-list 'tags-from-links node-title)))))
            (emacsql-with-transaction ekg-db
              (let ((note (ekg-note-create
                           text
                           'org-mode 
                           (seq-uniq
                            (cons (org-roam-node-title node)
                                  (append
                                   tags-from-links
                                   (mapcar #'org-roam-node-title
                                                  (mapcar #'org-roam-backlink-source-node (org-roam-backlinks-get node)))
                                          (org-roam-node-tags node)))))))
                (setf (ekg-note-id note) (org-roam-node-id node))
                (ekg-save-note note)
                (triples-set-type ekg-db (ekg-note-id note) 'org-roam `(:id ,(org-roam-node-id node)))))))))))

(provide 'ekg-org-roam-import)
