(
 :group "auto create skills"
 :tasks
 (
  (
   :name "learn-and-store-skill"
   :description "Give the agent a task where it encounters a non-obvious pattern. After completing the task, verify it created a skill note (tagged with 'prompt') summarizing what it learned.
"
   :setup
   (progn
;; A file with a subtle issue: using `nth` in a loop is O(n^2).
(with-temp-file "/tmp/ekg-bench-perf.el"
  (insert ";;; ekg-bench-perf.el --- Performance issue -*- lexical-binding: t -*-\n\n")
  (insert "(defun ekg-bench-sum-nth (lst)\n")
  (insert "  \"Sum elements of LST using nth.\"\n")
  (insert "  (let ((total 0))\n")
  (insert "    (dotimes (i (length lst))\n")
  (insert "      (setq total (+ total (nth i lst))))\n")
  (insert "    total))\n\n")
  (insert "(defun ekg-bench-find-nth (lst target)\n")
  (insert "  \"Find TARGET in LST using nth.\"\n")
  (insert "  (let ((result nil))\n")
  (insert "    (dotimes (i (length lst))\n")
  (insert "      (when (equal (nth i lst) target)\n")
  (insert "        (setq result i)))\n")
  (insert "    result))\n\n")
  (insert "(provide 'ekg-bench-perf)\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "Review /tmp/ekg-bench-perf.el for performance issues. Fix any
   problems you find. After you're done, if you learned something
   generally useful from this review, store it as a skill in ekg
   (tag it with 'prompt' and an appropriate topic tag) so it can
   help with future code reviews.")
)
   :verify-task
   (progn
(with-temp-buffer
  (insert-file-contents "/tmp/ekg-bench-perf.el")
  (let ((content (buffer-string)))
    ;; The O(n^2) nth-in-loop pattern should be fixed.
    ;; Either nth is removed, or the loop is restructured.
    (or (not (string-match-p "nth i" content))
        ;; Or there's a comment explaining why it's OK.
        (string-match-p "O(n" content))))
)
   :verify-skill
   (progn
;; A new prompt-tagged skill note should have been created.
(let ((prompt-notes (ekg-get-notes-with-tag "prompt")))
  (cl-some
    (lambda (n)
      (let ((text (downcase (ekg-note-text n))))
        ;; Should mention performance, nth, O(n^2), or similar.
        (or (string-match-p "nth" text)
            (string-match-p "performance" text)
            (string-match-p "o(n" text)
            (string-match-p "quadratic" text)
            (string-match-p "dolist\\|seq-" text))))
    prompt-notes))
)
   :verify-memory
   (progn
;; The skill note should be co-tagged with prompt and something
;; useful for future lookups (elisp, performance, code-review, etc.).
(let ((prompt-notes (ekg-get-notes-with-tag "prompt")))
  (cl-some
    (lambda (n)
      ;; Should have at least one tag besides 'prompt', date, and agent.
      (let ((tags (seq-remove
                    (lambda (tag)
                      (or (string= tag "prompt")
                          (string-prefix-p "date/" tag)
                          (string= tag "agent")))
                    (ekg-note-tags n))))
        (> (length tags) 0)))
    prompt-notes))
)
   :max-iterations 20
   :timeout 180
  )
  (
   :name "discover-and-document-convention"
   :description "The agent reads multiple files in a project and should notice a consistent convention, then store it as a reusable skill.
"
   :setup
   (progn
;; Three files that all follow the same convention: error-first returns.
(with-temp-file "/tmp/ekg-bench-conv-a.el"
  (insert ";;; ekg-bench-conv-a.el -*- lexical-binding: t -*-\n")
  (insert ";; Convention: all public functions return (cons :ok result) or (cons :error msg)\n\n")
  (insert "(defun ekg-bench-parse-int (s)\n")
  (insert "  \"Parse string S as integer.\"\n")
  (insert "  (condition-case nil\n")
  (insert "      (cons :ok (string-to-number s))\n")
  (insert "    (error (cons :error (format \"Cannot parse: %s\" s)))))\n\n")
  (insert "(provide 'ekg-bench-conv-a)\n"))
(with-temp-file "/tmp/ekg-bench-conv-b.el"
  (insert ";;; ekg-bench-conv-b.el -*- lexical-binding: t -*-\n\n")
  (insert "(defun ekg-bench-read-config (path)\n")
  (insert "  \"Read config from PATH.\"\n")
  (insert "  (if (file-exists-p path)\n")
  (insert "      (cons :ok (with-temp-buffer\n")
  (insert "                  (insert-file-contents path)\n")
  (insert "                  (buffer-string)))\n")
  (insert "    (cons :error (format \"Config not found: %s\" path))))\n\n")
  (insert "(provide 'ekg-bench-conv-b)\n"))
(with-temp-file "/tmp/ekg-bench-conv-c.el"
  (insert ";;; ekg-bench-conv-c.el -*- lexical-binding: t -*-\n\n")
  (insert "(defun ekg-bench-validate-email (s)\n")
  (insert "  \"Check if S is a valid email.\"\n")
  (insert "  (if (string-match-p \"@\" s)\n")
  (insert "      (cons :ok s)\n")
  (insert "    (cons :error \"No @ in email\")))\n\n")
  (insert "(provide 'ekg-bench-conv-c)\n"))
)
   :trigger
   (progn
(ekg-agent-ask
  "I have a small project with files: /tmp/ekg-bench-conv-a.el,
   /tmp/ekg-bench-conv-b.el, and /tmp/ekg-bench-conv-c.el.  Read
   all three files, identify any consistent conventions or patterns
   across them, and store what you find as a skill note in ekg
   (tagged with 'prompt' and relevant topic tags) so future code
   in this project follows the same conventions.")
)
   :verify-task
   (progn
;; The agent should have read all three files.
t
)
   :verify-skill
   (progn
;; A prompt note should exist describing the error-first pattern.
(let ((prompt-notes (ekg-get-notes-with-tag "prompt")))
  (cl-some
    (lambda (n)
      (let ((text (downcase (ekg-note-text n))))
        (or (string-match-p ":ok\\|:error" text)
            (string-match-p "result.*cons\\|cons.*result" text)
            (string-match-p "error.first\\|error.*return" text)
            (string-match-p "convention\\|pattern" text))))
    prompt-notes))
)
   :verify-memory
   (progn
;; The skill should be tagged for future retrieval.
(let ((prompt-notes (ekg-get-notes-with-tag "prompt")))
  (cl-some
    (lambda (n)
      (let ((tags (ekg-note-tags n)))
        ;; Should have tags that help retrieve it for this project.
        (or (cl-some (lambda (tag) (string-match-p "conv\\|bench\\|elisp\\|pattern" tag)) tags)
            ;; Or at least have more than just prompt + date + agent.
            (> (length (seq-remove
                         (lambda (tag)
                           (or (string= tag "prompt")
                               (string-prefix-p "date/" tag)
                               (string= tag "agent")))
                         tags))
               0))))
    prompt-notes))
)
   :max-iterations 20
   :timeout 180
  )
 )
)
