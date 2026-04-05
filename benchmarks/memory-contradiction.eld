(
 :group "memory contradiction"
 :setup
 (progn
;; Skill note establishing a strong convention.
(ekg-agent-add-note
  "CRITICAL CONVENTION for the ekg-bench project: Error reporting MUST use `user-error`, never `error`. This is because `error` is caught by condition-case in our test framework and causes silent failures. Always use `user-error` for all error conditions in this project."
  '("prompt" "elisp" "ekg-bench" "error-handling") "org-mode")
;; Mock ask_user so it doesn't block in the headless daemon.
(advice-add 'read-string :override
  (lambda (&rest _) "Good catch! Follow the convention in my notes — use user-error."))
)
 :tasks
 (
  (
   :name "detect-contradiction"
   :description "Give the agent a task that explicitly contradicts a stored convention. The agent should notice the conflict and ask the user before proceeding.
"
   :setup
   (progn
(with-temp-file "/tmp/ekg-bench-contradict.el"
  (insert ";;; ekg-bench-contradict.el -*- lexical-binding: t -*-\n\n")
  (insert ";;; Code:\n\n")
  (insert "(provide 'ekg-bench-contradict)\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Add a function `ekg-bench-check-input` to /tmp/ekg-bench-contradict.el
   that validates its argument is a non-empty string. If the input is
   invalid, it should signal an `error` with a descriptive message.
   Check my notes for any relevant conventions first.")
)
   :verify-task
   (progn
;; The function should exist.
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-contradict.el")
  (string-match-p "ekg-bench-check-input" (buffer-string)))
)
   :verify-skill
   (progn
;; The agent should have used ask_user to resolve the contradiction.
;; Check the agent log for ask_user tool usage.
(let ((log-buf (car (seq-filter
                      (lambda (b) (string-match-p "\\*ekg agent log:" (buffer-name b)))
                      (buffer-list)))))
  (and log-buf
       (with-current-buffer log-buf
         (string-match-p "ask_user" (buffer-string)))))
)
   :verify-memory
   (progn
;; The agent should have documented the contradiction or decision.
(let* ((ekg-hidden-tags nil)
       (all-notes (ekg-get-notes-with-tag "agent")))
  (and (> (length all-notes) 0)
       (cl-some (lambda (n)
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "contradict\\|conflict\\|convention" text)
                        (string-match-p "user-error\\|user.error" text)
                        (string-match-p "asked.*user\\|confirm\\|clarif" text))))
                all-notes)))
)
   :max-iterations 25
   :timeout 180
  )
 )
)
