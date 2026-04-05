(
 :group "coding with skills"
 :setup
 (progn
;; Create a skill note: when writing elisp, use docstrings and cl-defun.
(ekg-agent-add-note
  "When writing Emacs Lisp functions:\n- Always include a docstring.\n- Use cl-defun with keyword arguments for public API functions.\n- Prefer descriptive names with a package prefix."
  '("prompt" "elisp") "org-mode")
)
 :tasks
 (
  (
   :name "write-function-following-skill"
   :description "Ask the agent to write an elisp function in a file. Verify the function exists and follows the skill note conventions (has a docstring).
"
   :setup
   (progn
(with-temp-file "/tmp/ekg-bench-code.el"
  (insert ";;; ekg-bench-code.el --- Bench test -*- lexical-binding: t -*-\n\n;;; Code:\n\n(provide 'ekg-bench-code)\n;;; ekg-bench-code.el ends here\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Add a function called ekg-bench-greet to the file /tmp/ekg-bench-code.el.
   The function should take a name as an argument and return a greeting
   string like 'Hello, NAME!'. Follow any relevant conventions you find
   in my notes.")
)
   :verify-task
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-code.el")
  (let ((content (buffer-string)))
    (and (string-match-p "ekg-bench-greet" content)
         (string-match-p "defun\\|cl-defun" content))))
)
   :verify-skill
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-code.el")
  (let ((content (buffer-string)))
    ;; Skill says: always include a docstring.
    ;; Look for a quoted string (docstring) after the arglist.
    (and (string-match-p "ekg-bench-greet" content)
         (string-match-p "\"[^\"]+\"" content))))
)
   :verify-memory
   (progn
;; Agent should have documented the task with relevant details.
(let* ((ekg-hidden-tags nil)
       (all-notes (ekg-get-notes-with-tag "agent")))
  (and (> (length all-notes) 0)
       (cl-some (lambda (n)
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "greet\\|ekg-bench-code" text)
                        (string-match-p "function\\|docstring\\|skill" text))))
                all-notes)))
)
   :max-iterations 20
   :timeout 180
  )
 )
)
