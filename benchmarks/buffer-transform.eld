(
 :group "buffer transform"
 :tasks
 (
  (
   :name "csv-to-org-table"
   :description "Create a CSV data file. Ask the agent to read it and produce an org-mode file with the data as a sorted org table. This is an emacs-y workflow: reading structured data and producing formatted output.
"
   :setup
   (progn
(with-temp-file "/tmp/ekg-bench-data.csv"
  (insert "Name,Score,Grade\n")
  (insert "Charlie,78,C\n")
  (insert "Alice,95,A\n")
  (insert "Bob,87,B\n")
  (insert "Diana,92,A\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Read the CSV file at /tmp/ekg-bench-data.csv and create an
   org-mode file at /tmp/ekg-bench-report.org containing:
   1. A title '#+TITLE: Student Report'
   2. An org table with the data sorted by Score descending
   3. A summary line below the table showing the average score.
   Use elisp or shell commands as needed to accomplish this.")
)
   :verify-task
   (progn
(and (file-exists-p "/tmp/ekg-bench-report.org")
     (with-temp-buffer
       (insert-file-contents "/tmp/ekg-bench-report.org")
       (let ((content (buffer-string)))
         (and
           ;; Has a title.
           (string-match-p "TITLE" content)
           ;; Has table markers.
           (string-match-p "|" content)
           ;; Contains all student names.
           (string-match-p "Alice" content)
           (string-match-p "Bob" content)
           (string-match-p "Charlie" content)
           (string-match-p "Diana" content)))))
)
   :verify-memory
   (progn
;; Agent should have documented the CSV-to-org conversion task.
(let* ((ekg-hidden-tags nil)
       (all-notes (ekg-get-notes-with-tag "agent")))
  (and (> (length all-notes) 0)
       (cl-some (lambda (n)
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "csv\\|org.*table\\|report" text)
                        (string-match-p "convert\\|transform\\|student" text)
                        (string-match-p "bench-data\\|bench-report" text))))
                all-notes)))
)
   :max-iterations 20
   :timeout 120
  )
 )
)
