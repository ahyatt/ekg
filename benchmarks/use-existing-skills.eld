(
 :group "use existing skills"
 :setup
 (progn
;; A skill note about error handling conventions.
(ekg-agent-add-note
  "Error handling convention for this project:\n- All public functions must validate their inputs.\n- Use `cl-check-type` for type assertions.\n- For recoverable errors, return nil and log with `message`.\n- For unrecoverable errors, use `user-error` (not `error`)."
  '("prompt" "elisp" "error-handling") "org-mode")
)
 :tasks
 (
  (
   :name "apply-error-handling-skill"
   :description "A skill note defines error-handling conventions.  Ask the agent to write a function that processes input.  Verify it follows the conventions from the skill note.
"
   :setup
   (progn
(with-temp-file "/tmp/ekg-bench-errconv.el"
  (insert ";;; ekg-bench-errconv.el --- Error conventions test -*- lexical-binding: t -*-\n\n")
  (insert ";;; Code:\n\n")
  (insert "(provide 'ekg-bench-errconv)\n")
  (insert ";;; ekg-bench-errconv.el ends here\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Add a function `ekg-bench-safe-divide` to /tmp/ekg-bench-errconv.el
   that divides two numbers. Follow any error handling conventions
   you find in my notes.")
)
   :verify-task
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-errconv.el")
  (string-match-p "ekg-bench-safe-divide" (buffer-string)))
)
   :verify-skill
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-errconv.el")
  (let ((content (buffer-string)))
    (and
      (string-match-p "ekg-bench-safe-divide" content)
      ;; Should follow at least some of the conventions:
      ;; input validation, cl-check-type, user-error, etc.
      (or (string-match-p "cl-check-type\\|check-type" content)
          (string-match-p "numberp\\|integerp\\|floatp" content))
      (or (string-match-p "user-error" content)
          ;; At least some error handling for division by zero.
          (string-match-p "zerop\\|= 0" content)))))
)
   :verify-memory
   (progn
;; Agent should have documented applying the error handling conventions.
(let* ((ekg-hidden-tags nil)
       (all-notes (ekg-get-notes-with-tag "agent")))
  (and (> (length all-notes) 0)
       (cl-some (lambda (n)
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "error.*handling\\|convention\\|safe-divide" text)
                        (string-match-p "user-error\\|check-type\\|validation" text))))
                all-notes)))
)
   :max-iterations 20
   :timeout 180
  )
 )
)
