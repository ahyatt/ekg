(
 :group "memory continuity"
 :tasks
 (
  (
   :name "continue-without-redoing"
   :description "Seed detailed progress notes from a \"prior session\" showing files 1 and 2 are already done. Give the agent a continuation task. Verify it picks up from file 3 without redoing 1 and 2.
"
   :setup
   (progn
;; Files from the "prior session" — already modified correctly.
(with-temp-file "/tmp/ekg-bench-cont-1.el"
  (insert ";;; ekg-bench-cont-1.el -*- lexical-binding: t -*-\n\n")
  (insert "(cl-defun ekg-bench-cont-greet (&key name)\n")
  (insert "  \"Greet the user by NAME.\"\n")
  (insert "  (format \"Hello, %s!\" name))\n\n")
  (insert "(provide 'ekg-bench-cont-1)\n"))
(with-temp-file "/tmp/ekg-bench-cont-2.el"
  (insert ";;; ekg-bench-cont-2.el -*- lexical-binding: t -*-\n\n")
  (insert "(cl-defun ekg-bench-cont-farewell (&key name)\n")
  (insert "  \"Say farewell to NAME.\"\n")
  (insert "  (format \"Goodbye, %s!\" name))\n\n")
  (insert "(provide 'ekg-bench-cont-2)\n"))
;; File 3 — NOT yet modified (still has the raw stub).
(with-temp-file "/tmp/ekg-bench-cont-3.el"
  (insert ";;; ekg-bench-cont-3.el -*- lexical-binding: t -*-\n\n")
  (insert "(defun ekg-bench-cont-thanks (x)\n  x)\n\n")
  (insert "(provide 'ekg-bench-cont-3)\n"))
;; Prior session's progress note with specific details.
(ekg-agent-add-note
  "Task: Refactor ekg-bench-cont files to use cl-defun with &key args and add docstrings.\n\nProgress:\n- File 1 (/tmp/ekg-bench-cont-1.el): DONE — converted ekg-bench-cont-greet to cl-defun with &key name, added docstring.\n- File 2 (/tmp/ekg-bench-cont-2.el): DONE — converted ekg-bench-cont-farewell to cl-defun with &key name, added docstring.\n- File 3 (/tmp/ekg-bench-cont-3.el): NOT STARTED — need to convert ekg-bench-cont-thanks to cl-defun with &key arg and add docstring.\n\nNext: Work on file 3."
  '("agent-task" "ekg-bench-cont" "agent/self-info") "org-mode")
)
   :trigger
   (progn
(ekg-agent-ask
  "Continue the refactoring task for the ekg-bench-cont files.
   Check my notes for previous progress on this task and pick up
   where we left off. Don't redo work that's already done.")
)
   :verify-task
   (progn
;; File 3 should now have a docstring and cl-defun.
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-cont-3.el")
  (let ((content (buffer-string)))
    (and (or (string-match-p "cl-defun" content)
             (string-match-p "&key" content))
         (string-match-p "\"[^\"]+\"" content))))
)
   :verify-skill
   (progn
;; Files 1 and 2 should NOT have been re-modified.
;; Check that read_file was not called on files 1 and 2, OR
;; that the files remain unchanged. We check file contents.
(and
  (with-temp-buffer
    (insert-file-contents "/tmp/ekg-bench-cont-1.el")
    ;; Should still have the original content.
    (and (string-match-p "ekg-bench-cont-greet" (buffer-string))
         (string-match-p "cl-defun" (buffer-string))))
  (with-temp-buffer
    (insert-file-contents "/tmp/ekg-bench-cont-2.el")
    (and (string-match-p "ekg-bench-cont-farewell" (buffer-string))
         (string-match-p "cl-defun" (buffer-string)))))
)
   :verify-memory
   (progn
;; The agent should have updated or created a note indicating
;; file 3 is now done.  The seeded note says file 3 is "NOT STARTED",
;; so we need a note that says it's done/complete/finished.
(let* ((ekg-hidden-tags nil)
       (task-notes (ekg-get-notes-with-any-tags
                     '("agent-task" "ekg-bench-cont" "agent/self-info"))))
  (cl-some (lambda (n)
             (let ((text (downcase (ekg-note-text n))))
               (and (string-match-p "cont-3\\|file.3\\|thanks" text)
                    (string-match-p "done\\|complete\\|finish" text))))
           task-notes))
)
   :max-iterations 25
   :timeout 180
  )
 )
)
