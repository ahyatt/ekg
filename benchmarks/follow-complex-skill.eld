(
 :group "follow complex skill"
 :setup
 (progn
;; A skill note with multi-step instructions for code changes.
(ekg-agent-add-note
  "When modifying elisp files, follow this checklist:\n1. Read the entire file first before making changes.\n2. Ensure lexical-binding is set in the first line.\n3. After making changes, verify the file compiles without warnings by evaluating it.\n4. Add a comment with the date and description of the change at the top of the file.\n5. Create a note documenting what was changed and why."
  '("prompt" "elisp" "code-changes") "org-mode")
)
 :tasks
 (
  (
   :name "follow-checklist-skill"
   :description "Ask the agent to make a code change. A skill note defines a multi-step checklist. Verify the agent followed the key steps.
"
   :setup
   (progn
(with-temp-file "/tmp/ekg-bench-checklist.el"
  (insert ";;; ekg-bench-checklist.el --- Checklist test -*- lexical-binding: t -*-\n\n")
  (insert ";;; Code:\n\n")
  (insert "(defun ekg-bench-double (n)\n")
  (insert "  \"Double the value of N.\"\n")
  (insert "  (* n 2))\n\n")
  (insert "(provide 'ekg-bench-checklist)\n")
  (insert ";;; ekg-bench-checklist.el ends here\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Add a function ekg-bench-triple to /tmp/ekg-bench-checklist.el
   that triples a number. Follow any relevant conventions from my
   notes about modifying elisp files.")
)
   :verify-task
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-checklist.el")
  (let ((content (buffer-string)))
    (and (string-match-p "ekg-bench-triple" content)
         ;; Original function should still be there.
         (string-match-p "ekg-bench-double" content))))
)
   :verify-skill
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-checklist.el")
  (let ((content (buffer-string)))
    (and
      ;; Step 2: lexical-binding should still be there.
      (string-match-p "lexical-binding: t" content)
      ;; Step 4: Should have a comment about the change.
      ;; Look for a comment line that mentions the change.
      (string-match-p ";;.*\\(add\\|change\\|triple\\|modif\\)" content)
      ;; Step 5: Should have created a note about the change.
      (let ((notes (ekg-get-notes-with-any-tags
                     '("agent-task" "code-changes" "ekg-bench-checklist"))))
        (> (length notes) 0)))))
)
   :verify-memory
   (progn
;; Agent should have created notes documenting the change (step 5).
(let ((all-notes (ekg-get-notes-with-any-tags
                   '("agent-task" "code-changes" "agent/self-info"))))
  (> (length all-notes) 0))
)
   :max-iterations 25
   :timeout 180
  )
 )
)
