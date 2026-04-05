(
 :group "file resource notes"
 :tasks
 (
  (
   :name "find-and-follow-file-note"
   :description "Create a file with a resource note containing specific instructions. Ask the agent to modify the file. Verify the agent found and followed the file-specific instructions.
"
   :setup
   (progn
;; Create the target file.
(with-temp-file "/tmp/ekg-bench-resource.el"
  (insert ";;; ekg-bench-resource.el -*- lexical-binding: t -*-\n\n")
  (insert ";;; Code:\n\n")
  (insert "(provide 'ekg-bench-resource)\n"))
;; Create a resource note about this file with specific conventions.
(let ((note (ekg-note-create
              :id (format "file:%s" (file-truename "/tmp/ekg-bench-resource.el"))
              :text "This file follows a specific pattern: all functions must return a plist with :status (either :ok or :error) and :value keys. Example: (list :status :ok :value result). Never raise errors directly; always return the error via the plist."
              :tags (list (concat "doc/" (downcase (file-name-nondirectory "/tmp/ekg-bench-resource.el")))
                          "elisp" "conventions")
              :mode 'org-mode
              :properties '(:titled/title ("ekg-bench-resource.el")))))
  (ekg-save-note note))
)
   :trigger
   (progn
(ekg-agent-ask
  "Add a function `ekg-bench-resource-divide` to /tmp/ekg-bench-resource.el
   that divides two numbers. Check if there are any notes about this file
   in ekg (look for doc/ tags or file resource notes) and follow any
   conventions you find.")
)
   :verify-task
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-resource.el")
  (string-match-p "ekg-bench-resource-divide" (buffer-string)))
)
   :verify-skill
   (progn
;; Should follow the plist return convention from the file note.
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-resource.el")
  (let ((content (buffer-string)))
    (and (string-match-p "ekg-bench-resource-divide" content)
         ;; Should return a plist with :status and :value.
         (or (string-match-p ":status" content)
             (string-match-p ":ok\\|:error" content)))))
)
   :verify-memory
   (progn
;; The agent should have created a task note referencing the file.
(let* ((ekg-hidden-tags nil)
       (all-notes (ekg-get-notes-with-tag "agent")))
  (and (> (length all-notes) 0)
       (cl-some (lambda (n)
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "resource" text)
                        (string-match-p "ekg-bench-resource" text)
                        (string-match-p "plist\\|:status\\|convention" text))))
                all-notes)))
)
   :max-iterations 20
   :timeout 180
  )
 )
)
