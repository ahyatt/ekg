(
 :group "subagent memory handoff"
 :setup
 (progn
;; Seed project context that the parent agent should find and share.
(ekg-agent-add-note
  "Project widget-maker conventions:\n- Language: Emacs Lisp\n- All functions must have docstrings\n- Use cl-defun with &key arguments for public API\n- File location: /tmp/ekg-bench-widget/"
  '("widget-maker" "conventions" "prompt") "org-mode")
)
 :tasks
 (
  (
   :name "delegate-to-subagent"
   :description "Ask the agent to delegate file creation to a subagent. Verify the parent stored context in ekg for the subagent to find, and the subagent produced output following conventions.
"
   :setup
   (progn
(make-directory "/tmp/ekg-bench-widget" t)
)
   :trigger
   (progn
(ekg-agent-ask
  "I need two utility functions for the widget-maker project:
   ekg-bench-widget-create (takes keyword args name and type, returns a plist)
   and ekg-bench-widget-validate (takes a widget plist, returns t or nil).

   Use a subagent to write these functions into /tmp/ekg-bench-widget/utils.el.
   Make sure to check my notes for any project conventions first and pass
   relevant context to the subagent.")
)
   :verify-task
   (progn
(and (file-exists-p "/tmp/ekg-bench-widget/utils.el")
     (with-temp-buffer
       (insert-file-contents "/tmp/ekg-bench-widget/utils.el")
       (let ((content (buffer-string)))
         (and (string-match-p "ekg-bench-widget-create" content)
              (string-match-p "ekg-bench-widget-validate" content)))))
)
   :verify-skill
   (progn
;; The subagent should have followed conventions from ekg notes.
(and (file-exists-p "/tmp/ekg-bench-widget/utils.el")
     (with-temp-buffer
       (insert-file-contents "/tmp/ekg-bench-widget/utils.el")
       (let ((content (buffer-string)))
         ;; Should have docstrings (from convention).
         (and (string-match-p "\"[^\"]+\"" content)
              ;; Should use cl-defun or &key (from convention).
              (or (string-match-p "cl-defun" content)
                  (string-match-p "&key" content))))))
)
   :verify-memory
   (progn
;; The parent agent should have created notes about the task,
;; and/or the subagent should also have stored results.
(let* ((ekg-hidden-tags nil)
       (all-notes (ekg-get-notes-with-any-tags
                    '("agent-task" "widget-maker" "agent/self-info" "agent"))))
  ;; Should have more than the 1 setup note (which has "conventions" tag).
  (and (> (length all-notes) 1)
       ;; At least one note should reference the work.
       (cl-some (lambda (n)
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "subagent\\|delegat\\|widget" text)
                        (string-match-p "utils\\.el\\|widget-create\\|widget-validate" text))))
                all-notes)))
)
   :max-iterations 35
   :timeout 360
  )
 )
)
