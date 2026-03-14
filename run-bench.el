;;; run-bench.el --- Script to run ekg agent benchmarks in batch -*- lexical-binding: t -*-

;;; Commentary:
;; Run with: emacs --batch -l run-bench.el
;; Or run a single task: EKG_BENCH_TASK=fix-elisp-bug emacs --batch -l run-bench.el
;;
;; Configure the provider by setting environment variables or editing
;; the provider form below.

;;; Code:

;; Add load paths.
(add-to-list 'load-path (expand-file-name "~/src/ekg"))
(add-to-list 'load-path (expand-file-name "~/src/llm-test"))

;; Add all elpa dirs.
(let ((elpa-dir (expand-file-name "~/.emacs.d/elpa")))
  (when (file-directory-p elpa-dir)
    (dolist (dir (directory-files elpa-dir t "\\`[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(require 'ekg-agent-bench)

;; Configure the LLM provider for the subprocess.
;; Edit this form to match your provider.  It will be evaluated inside
;; the test Emacs subprocess.
;;
;; Examples:
;;   (make-llm-claude :key "sk-..." :chat-model "claude-sonnet-4-20250514")
;;   (make-llm-openai :key "sk-..." :chat-model "gpt-4o")
;;   (progn (require 'llm-vertex) (make-llm-vertex :project "my-project" :chat-model "gemini-2.0-flash"))
(setq ekg-agent-bench-provider-form
      (or (when (getenv "EKG_BENCH_PROVIDER_FORM")
            (read (getenv "EKG_BENCH_PROVIDER_FORM")))
          (error "Set EKG_BENCH_PROVIDER_FORM env var or edit run-bench.el")))

;; Sanity check: verify the subprocess can start and load ekg.
(message "=== Sanity check: starting test Emacs ===")
(let ((info (ekg-agent-bench--start-emacs)))
  (unwind-protect
      (progn
        (message "ekg: %s" (llm-test--eval-in-emacs info "(featurep 'ekg)"))
        (message "ekg-agent: %s" (llm-test--eval-in-emacs info "(featurep 'ekg-agent)"))
        (message "ekg-db: %s" (llm-test--eval-in-emacs info "ekg-db-file"))
        (message "provider: %s" (llm-test--eval-in-emacs info "(type-of ekg-llm-provider)")))
    (llm-test--stop-emacs info)))
(message "=== Sanity OK ===\n")

;; Run benchmarks.
(let ((task-name (getenv "EKG_BENCH_TASK")))
  (if task-name
      (progn
        (message "Running single task: %s" task-name)
        (let ((result (ekg-agent-bench-run-one task-name)))
          (message "\nResult: %s — task:%s skill:%s memory:%s iters:%d time:%.0fs"
                   (ekg-agent-bench-result-name result)
                   (ekg-agent-bench--format-pass (ekg-agent-bench-result-task-passed result))
                   (ekg-agent-bench--format-pass (ekg-agent-bench-result-skill-passed result))
                   (ekg-agent-bench--format-pass (ekg-agent-bench-result-memory-passed result))
                   (ekg-agent-bench-result-iterations result)
                   (ekg-agent-bench-result-wall-time result))
          (unless (ekg-agent-bench-result-task-passed result)
            (kill-emacs 1))))
    (progn
      (message "Running all benchmarks...")
      (let ((results (ekg-agent-bench-run)))
        (let ((failures (seq-filter
                         (lambda (r) (not (ekg-agent-bench-result-task-passed r)))
                         results)))
          (when failures
            (message "\n%d task(s) FAILED" (length failures))
            (kill-emacs 1)))))))

;;; run-bench.el ends here
