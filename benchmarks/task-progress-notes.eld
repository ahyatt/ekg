(
 :group "task progress notes"
 :tasks
 (
  (
   :name "multi-step-task-journaling"
   :description "Give the agent a multi-step task (refactor three functions). Verify the agent creates an agent-task note at the start and appends progress entries as it works.
"
   :setup
   (progn
(with-temp-file "/tmp/ekg-bench-refactor.el"
  (insert ";;; ekg-bench-refactor.el --- Needs refactoring -*- lexical-binding: t -*-\n\n")
  (insert ";;; Code:\n\n")
  (insert "(defun ekg-bench-add (a b)\n  (+ a b))\n\n")
  (insert "(defun ekg-bench-sub (a b)\n  (- a b))\n\n")
  (insert "(defun ekg-bench-mul (a b)\n  (* a b))\n\n")
  (insert "(provide 'ekg-bench-refactor)\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Refactor /tmp/ekg-bench-refactor.el: each of the three functions
   (ekg-bench-add, ekg-bench-sub, ekg-bench-mul) needs a docstring
   and should use cl-defun with keyword arguments instead of positional
   args.  This is a multi-step task — document your progress in ekg
   as you go.")
)
   :verify-task
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-refactor.el")
  (let ((content (buffer-string)))
    (and
      ;; All three functions should have docstrings.
      (string-match-p "ekg-bench-add" content)
      (string-match-p "ekg-bench-sub" content)
      (string-match-p "ekg-bench-mul" content)
      ;; At least one should use cl-defun or have a docstring.
      (or (string-match-p "cl-defun" content)
          ;; Three docstrings (one per function).
          (>= (length (split-string content "\"[^\"]+\"" t)) 3)))))
)
   :verify-memory
   (progn
;; Should have an agent-task note that was updated during work.
(let ((task-notes (ekg-get-notes-with-tag "agent-task")))
  (and (> (length task-notes) 0)
       ;; The note should have substantial content (progress entries).
       (cl-some (lambda (n) (> (length (ekg-note-text n)) 100))
                task-notes)))
)
   :max-iterations 25
   :timeout 180
  )
  (
   :name "task-note-survives-restart"
   :description "Pre-seed a partial task note (simulating a restart). The agent should find the existing note and continue from where it left off rather than starting over.
"
   :setup
   (progn
;; Simulate a prior session's task note.
(ekg-agent-add-note
  "Task: Add error handling to /tmp/ekg-bench-errors.el\nProgress:\n- Read the file\n- Found that ekg-bench-divide has no division-by-zero check\nNext: Add the check and test it"
  '("agent-task" "ekg-bench-errors") "org-mode")
(with-temp-file "/tmp/ekg-bench-errors.el"
  (insert ";;; ekg-bench-errors.el --- Needs error handling -*- lexical-binding: t -*-\n\n")
  (insert "(defun ekg-bench-divide (a b)\n")
  (insert "  \"Divide A by B.\"\n")
  (insert "  (/ a b))\n\n")
  (insert "(provide 'ekg-bench-errors)\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Continue the task of adding error handling to /tmp/ekg-bench-errors.el.
   Check my notes — there should be a task note from a previous session
   describing progress so far. Pick up where it left off.")
)
   :verify-task
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-errors.el")
  (let ((content (buffer-string)))
    (and
      ;; Should have error handling for division by zero.
      (or (string-match-p "zerop\\|= 0\\|equal.*0\\|zero" content)
          (string-match-p "error\\|signal\\|condition-case" content))
      ;; Original function should still be there.
      (string-match-p "ekg-bench-divide" content))))
)
   :verify-memory
   (progn
;; The agent should have found the existing note and either
;; appended to it or created a new one referencing the prior work.
(let ((task-notes (ekg-get-notes-with-tag "agent-task")))
  ;; Started with 1 note; agent should have updated or added.
  (and (>= (length task-notes) 1)
       (cl-some (lambda (n)
                  ;; The note should mention the completion or the fix.
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "division.by.zero\\|zerop\\|error.handling" text)
                        (string-match-p "done\\|complete\\|fixed" text))))
                task-notes)))
)
   :max-iterations 20
   :timeout 180
  )
 )
)
