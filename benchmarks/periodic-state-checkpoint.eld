(
 :group "periodic state checkpoint"
 :tasks
 (
  (
   :name "multi-file-with-checkpoints"
   :description "Give the agent a task spanning 4 files. Verify that the agent creates progress notes as it works, not just one at the end.
"
   :setup
   (progn
(dotimes (i 4)
  (let ((file (format "/tmp/ekg-bench-checkpoint-%d.el" (1+ i))))
    (with-temp-file file
      (insert (format ";;; ekg-bench-checkpoint-%d.el -*- lexical-binding: t -*-\n\n" (1+ i)))
      (insert (format "(defun ekg-bench-func-%d (x)\n  (* x %d))\n\n" (1+ i) (1+ i)))
      (insert (format "(provide 'ekg-bench-checkpoint-%d)\n" (1+ i))))))
)
   :trigger
   (progn
(ekg-agent-ask
  "I need you to modify 4 files: /tmp/ekg-bench-checkpoint-1.el through
   /tmp/ekg-bench-checkpoint-4.el. For each file:
   1. Add a docstring to the existing function
   2. Add an input validation check (ensure x is a number)
   3. Add a second function that does the inverse operation

   This is a multi-step task. Document your progress in ekg notes
   as you complete each file.")
)
   :verify-task
   (progn
;; All 4 files should be modified.
(cl-every
  (lambda (i)
    (let ((file (format "/tmp/ekg-bench-checkpoint-%d.el" i)))
      (and (file-exists-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (let ((content (buffer-string)))
               ;; Should have a docstring and some validation.
               (and (string-match-p "\"[^\"]+\"" content)
                    (or (string-match-p "numberp" content)
                        (string-match-p "check-type\\|cl-check-type" content)
                        (string-match-p "unless\\|when\\|if" content))))))))
  '(1 2 3 4))
)
   :verify-memory
   (progn
;; Should have multiple notes showing progressive work.
;; Bind ekg-hidden-tags to nil so verify finds notes with agent/self-info.
(let* ((ekg-hidden-tags nil)
       (all-agent-notes (ekg-get-notes-with-tag "agent")))
  ;; Need at least 2 agent-created notes (task + progress/skill).
  (and (>= (length all-agent-notes) 2)
       ;; At least one note should reference specific file work.
       (cl-some (lambda (n)
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "checkpoint" text)
                        (string-match-p "func-[1-4]\\|bench-func" text)
                        (string-match-p "docstring\\|validation\\|inverse" text))))
                all-agent-notes)))
)
   :max-iterations 35
   :timeout 300
  )
 )
)
