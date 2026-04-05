(
 :group "store findings"
 :tasks
 (
  (
   :name "investigate-and-document"
   :description "Ask the agent to investigate a code file and document its findings in ekg notes. Verify the agent created notes with useful content and appropriate tags.
"
   :setup
   (progn
(with-temp-file "/tmp/ekg-bench-analyze.el"
  (insert ";;; ekg-bench-analyze.el --- Sample package -*- lexical-binding: t -*-\n\n")
  (insert ";; This package provides utilities for text analysis.\n\n")
  (insert "(defgroup ekg-bench-analyze nil\n  \"Text analysis utilities.\"\n  :group 'tools)\n\n")
  (insert "(defcustom ekg-bench-analyze-max-words 1000\n")
  (insert "  \"Maximum number of words to analyze.\"\n  :type 'integer\n  :group 'ekg-bench-analyze)\n\n")
  (insert "(defun ekg-bench-analyze-word-count (text)\n")
  (insert "  \"Count words in TEXT.\"\n")
  (insert "  (length (split-string text \"[[:space:]]+\" t)))\n\n")
  (insert "(defun ekg-bench-analyze-char-freq (text)\n")
  (insert "  \"Return an alist of (CHAR . COUNT) for TEXT.\"\n")
  (insert "  (let ((freq nil))\n")
  (insert "    (seq-doseq (c text)\n")
  (insert "      (if-let ((entry (assq c freq)))\n")
  (insert "          (setcdr entry (1+ (cdr entry)))\n")
  (insert "        (push (cons c 1) freq)))\n")
  (insert "    (sort freq (lambda (a b) (> (cdr a) (cdr b))))))\n\n")
  (insert "(provide 'ekg-bench-analyze)\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Analyze the elisp package at /tmp/ekg-bench-analyze.el.
   Document your findings as ekg notes: describe what the package does,
   its public API, any potential issues or improvements you notice.
   Tag the notes with 'code-review' and 'ekg-bench-analyze'.")
)
   :verify-task
   (progn
;; Agent should have created at least one note tagged code-review.
(let ((notes (ekg-get-notes-with-tag "code-review")))
  (and (> (length notes) 0)
       (cl-some (lambda (n)
                  (> (length (ekg-note-text n)) 50))
                notes)))
)
   :verify-memory
   (progn
;; The notes should also be tagged with the package name.
(let ((notes (ekg-get-notes-with-any-tags
               '("code-review" "ekg-bench-analyze"))))
  (> (length notes) 0))
)
   :max-iterations 20
   :timeout 180
  )
 )
)
