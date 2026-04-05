(
 :group "elisp debugging"
 :setup
 (progn
;; Make run_elisp available so the agent can test elisp code.
(setq ekg-agent-extra-tools (list ekg-agent-tool-run-elisp))
)
 :tasks
 (
  (
   :name "fix-elisp-bug"
   :description "Create a file with a buggy elisp function. Ask the agent to find and fix the bug. The bug is a wrong variable name in the body. This tests the agent's ability to understand elisp and use tools like run_elisp to verify the fix.
"
   :setup
   (progn
(with-temp-file "/tmp/ekg-bench-buggy.el"
  (insert ";;; ekg-bench-buggy.el --- File with a bug -*- lexical-binding: t -*-\n\n")
  (insert "(defun ekg-bench-sum-list (numbers)\n")
  (insert "  \"Return the sum of NUMBERS, a list of integers.\"\n")
  (insert "  (let ((total 0))\n")
  (insert "    (dolist (n numbrs)\n")
  (insert "      (setq total (+ total n)))\n")
  (insert "    total))\n\n")
  (insert "(provide 'ekg-bench-buggy)\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "The file /tmp/ekg-bench-buggy.el has a bug. Find the bug,
   fix it, and verify that the function ekg-bench-sum-list works
   correctly (e.g. (ekg-bench-sum-list '(1 2 3)) should return 6).
   Use the run_elisp tool to test your fix.")
)
   :verify-task
   (progn
;; The file should be fixed: 'numbrs' -> 'numbers'
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-buggy.el")
  (let ((content (buffer-string)))
    (and
      ;; The typo should be fixed.
      (not (string-match-p "numbrs" content))
      ;; The correct variable should be there.
      (string-match-p "numbers" content)
      ;; The function should still exist.
      (string-match-p "ekg-bench-sum-list" content))))
)
   :verify-memory
   (progn
;; Agent should have documented finding and fixing the bug.
(let* ((ekg-hidden-tags nil)
       (all-notes (ekg-get-notes-with-tag "agent")))
  (and (> (length all-notes) 0)
       (cl-some (lambda (n)
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "bug\\|typo\\|numbrs\\|numbers" text)
                        (string-match-p "fix\\|debug\\|ekg-bench-buggy" text))))
                all-notes)))
)
   :max-iterations 20
   :timeout 120
  )
 )
)
