(
 :group "memory retrieval"
 :setup
 (progn
;; Simulate a previous session's finding stored in ekg.
(ekg-agent-add-note
  "Decision: For the data-export project, we chose CSV format over JSON
   because the downstream system only accepts CSV. The delimiter should
   be a semicolon (;) not a comma, because the data contains commas in
   text fields."
  '("data-export" "decisions" "agent-task") "org-mode")
;; Another related note.
(ekg-agent-add-note
  "The data-export output file should be written to /tmp/export-output/.
   The filename format is export-YYYY-MM-DD.csv."
  '("data-export" "conventions") "org-mode")
)
 :tasks
 (
  (
   :name "retrieve-memory-and-continue"
   :description "Give the agent a vague follow-up task about the data-export project. The agent should find the stored decision notes and apply them.
"
   :setup
   (progn
(make-directory "/tmp/export-output" t)
(with-temp-file "/tmp/ekg-bench-data.txt"
  (insert "name;value;description\nalpha;100;first item\nbeta;200;second item\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Continue the data-export project. We have a data file at
   /tmp/ekg-bench-data.txt that needs to be exported. Check my notes
   for the decisions we made about format and conventions, then
   produce the export file.")
)
   :verify-task
   (progn
;; There should be a CSV file in the output directory.
(let ((files (directory-files "/tmp/export-output" nil "\\.csv\\'")))
  (and (> (length files) 0)
       (with-temp-buffer
         (insert-file-contents
           (expand-file-name (car files) "/tmp/export-output"))
         ;; Should use semicolons as decided.
         (string-match-p ";" (buffer-string)))))
)
   :verify-memory
   (progn
;; The agent should have read from ekg (task or self-info notes exist,
;; or more notes were created during the run).
(let ((all-notes (ekg-get-notes-with-any-tags
                   '("data-export" "agent-task" "agent/self-info"))))
  ;; Should have more than the 2 we started with.
  (> (length all-notes) 2))
)
   :max-iterations 25
   :timeout 180
  )
 )
)
