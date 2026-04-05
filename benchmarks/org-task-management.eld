(
 :group "org task management"
 :setup
 (progn
;; Make the org tools available for these benchmarks.
(setq ekg-agent-extra-tools
      (list ekg-agent-org-tool-add-task
            ekg-agent-org-tool-set-status
            ekg-agent-org-tool-list-items))
)
 :tasks
 (
  (
   :name "create-org-task-from-scratch"
   :description "No org task exists. Give the agent a complex goal. It should create an org task for the high-level goal, break it into subtasks, execute them, and mark them done.
"
   :setup
   (progn
(with-temp-file "/tmp/ekg-bench-org-project.el"
  (insert ";;; ekg-bench-org-project.el --- Needs work -*- lexical-binding: t -*-\n\n")
  (insert ";;; Code:\n\n")
  (insert "(provide 'ekg-bench-org-project)\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "I need you to build out /tmp/ekg-bench-org-project.el with two
   utility functions: ekg-bench-capitalize-words (takes a string,
   capitalizes each word) and ekg-bench-reverse-words (takes a string,
   reverses word order).  Both need docstrings.

   Create an org task in ekg for this goal, break it into subtasks,
   execute them, and mark each subtask done as you complete it.
   Mark the parent task done when everything is finished.")
)
   :verify-task
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-org-project.el")
  (let ((content (buffer-string)))
    (and (string-match-p "ekg-bench-capitalize-words" content)
         (string-match-p "ekg-bench-reverse-words" content))))
)
   :verify-skill
   (progn
;; Verify the agent used the org task system.
(let ((tasks (ekg-org-get-tasks)))
  ;; Should have at least one top-level task.
  (and (> (length tasks) 0)
       (cl-some
         (lambda (task)
           (let ((title (downcase
                         (or (car (plist-get
                                    (ekg-note-properties task)
                                    :titled/title))
                             ""))))
             ;; Title should relate to the project.
             (or (string-match-p "org-project\\|capitalize\\|utility\\|bench" title)
                 (string-match-p "function" title))))
         tasks)))
)
   :verify-memory
   (progn
;; At least one org task should be marked DONE.
(let ((all-notes (ekg-get-notes-with-parent-tag "org/task")))
  (cl-some
    (lambda (n)
      (member "org/state/done" (ekg-note-tags n)))
    all-notes))
)
   :max-iterations 30
   :timeout 300
  )
  (
   :name "continue-existing-org-task"
   :description "An org task already exists with a title but no subtasks. The agent should find it, add subtasks, execute, and mark done.
"
   :setup
   (progn
;; Create an existing org task.
(let ((note (ekg-note-create
              :text "Add input validation to the calculator functions."
              :tags (list "org/task" "org/state/todo" "ekg-bench-calc")
              :properties (list :titled/title
                                (list "Add validation to calculator")))))
  (ekg-save-note note)
  ;; Save the ID so we can reference it.
  (setq ekg-bench--org-task-id (ekg-note-id note)))
(with-temp-file "/tmp/ekg-bench-calc.el"
  (insert ";;; ekg-bench-calc.el --- Calculator -*- lexical-binding: t -*-\n\n")
  (insert "(defun ekg-bench-calc-add (a b)\n")
  (insert "  \"Add A and B.\"\n")
  (insert "  (+ a b))\n\n")
  (insert "(defun ekg-bench-calc-div (a b)\n")
  (insert "  \"Divide A by B.\"\n")
  (insert "  (/ a b))\n\n")
  (insert "(provide 'ekg-bench-calc)\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "There's an existing org task about adding validation to the
   calculator in /tmp/ekg-bench-calc.el. Find it in my org tasks,
   add subtasks for each function that needs validation, then do
   the work and mark tasks done as you complete them.")
)
   :verify-task
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-calc.el")
  (let ((content (buffer-string)))
    ;; Should have some form of validation (type check, error, etc.)
    (and (string-match-p "ekg-bench-calc-div" content)
         (or (string-match-p "numberp\\|integerp\\|floatp" content)
             (string-match-p "zerop\\|= 0" content)
             (string-match-p "error\\|signal\\|check-type\\|cl-check-type" content)
             (string-match-p "unless\\|when\\|if.*0" content)))))
)
   :verify-skill
   (progn
;; The existing task should have child tasks (subtasks).
(let ((children (ekg-org-get-child-notes-of-id ekg-bench--org-task-id)))
  (> (length children) 0))
)
   :verify-memory
   (progn
;; The parent task or its children should be marked DONE.
(let ((parent (ekg-get-note-with-id ekg-bench--org-task-id)))
  (or (member "org/state/done" (ekg-note-tags parent))
      (let ((children (ekg-org-get-child-notes-of-id ekg-bench--org-task-id)))
        (cl-some (lambda (c) (member "org/state/done" (ekg-note-tags c)))
                 children))))
)
   :max-iterations 30
   :timeout 300
  )
 )
)
