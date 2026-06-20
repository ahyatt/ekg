(
 :group "status update gap"
 :tasks
 (
  (
   :name "long-multi-file-status-gap"
   :description "Run a deliberately long multi-file task and verify that explicit summarize_state calls keep the user updated at least once per minute.
"
   :setup
   (progn
(dotimes (i 4)
  (let ((file (format "/tmp/ekg-bench-status-gap-%d.el" (1+ i))))
    (with-temp-file file
      (insert (format ";;; ekg-bench-status-gap-%d.el -*- lexical-binding: t -*-\n\n"
                      (1+ i)))
      (insert (format "(defun ekg-bench-status-gap-%d (value)\n  value)\n\n"
                      (1+ i)))
      (insert (format "(provide 'ekg-bench-status-gap-%d)\n" (1+ i))))))
(defun ekg-bench--slow-file-update (orig-fn &rest args)
  (sleep-for 15)
  (apply orig-fn args))
(advice-add 'ekg-agent--edit-file :around #'ekg-bench--slow-file-update)
(advice-add 'ekg-agent--write-file :around #'ekg-bench--slow-file-update)
)
   :trigger
   (progn
(ekg-agent-ask
  "Please update these four files:
   /tmp/ekg-bench-status-gap-1.el
   /tmp/ekg-bench-status-gap-2.el
   /tmp/ekg-bench-status-gap-3.el
   /tmp/ekg-bench-status-gap-4.el

   Each file has the same simple structure. Do not read all four
   files first, and do not use edit_file. For each file, use write_file
   once with the complete corrected file content:
   - keep the existing header and provide form for that file number,
   - make the function body include a docstring,
   - validate VALUE with numberp and error if it is not a number,
   - keep returning VALUE unchanged.

   This is a long task. Use `summarize_state` explicitly with
   meaningful updates at least once a minute. Each update should say
   what you finished, what you are doing now, and what comes next.
   Do not batch all of the edits together. Work one file at a time:
   write one file, call summarize_state, then move to the next file.")
)
   :verify-task
   (progn
(cl-every
 (lambda (i)
   (with-temp-buffer
     (insert-file-contents
      (format "/tmp/ekg-bench-status-gap-%d.el" i))
     (let ((content (buffer-string)))
       (and (string-match-p
             (format "defun ekg-bench-status-gap-%d" i)
             content)
            (string-match-p "\"[^\"]+\"" content)
            (or (string-match-p "numberp" content)
                (string-match-p "cl-check-type\\|check-type" content))))))
 '(1 2 3 4))
)
   :timeout 300
   :max-status-update-gap 60
  )
 )
)
