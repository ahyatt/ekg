;;; run-bench.el --- Script to run ekg agent benchmarks in batch -*- lexical-binding: t -*-

;;; Commentary:
;; Run with: emacs --batch -l run-bench.el
;; Or run a single task: EKG_BENCH_TASK=fix-elisp-bug emacs --batch -l run-bench.el
;;
;; Configure the environment by setting the following variables:
;;
;; EKG_BENCH_PROVIDER_FORM: A sexp that will be evalled in the subprocess to
;;   create the LLM provider to run the agent.  It must support tool calling.
;;   Example: "(make-llm-openai :key \"sk-...\")"
;;
;; LLM_TEST_PATH: Path to the llm-test source directory. Defaults to
;;   ../llm-test relative to this script.
;;
;; ELPA_PATH: Path to the ELPA directory containing installed packages.
;;   Defaults to `package-user-dir` (usually ~/.emacs.d/elpa).
;;
;; EKG_BENCH_TASK: Optional. The name of a single benchmark task to run.
;;   If unset, all benchmarks are run.

;;; Code:

(defvar ekg-bench-ekg-path
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Path to the ekg source directory, determined from this script's location.")

(defvar ekg-bench-llm-test-path
  (or (getenv "LLM_TEST_PATH")
      (expand-file-name "../llm-test" ekg-bench-ekg-path))
  "Path to the llm-test source directory.  Override with LLM_TEST_PATH env var.")

(require 'package)

(defvar ekg-bench-elpa-path
  (or (getenv "ELPA_PATH")
      (expand-file-name package-user-dir))
  "Path to the ELPA directory.  Override with ELPA_PATH env var.")

;; Add all elpa dirs to the end of load-path.
(when (file-directory-p ekg-bench-elpa-path)
  (dolist (dir (directory-files ekg-bench-elpa-path t "\\`[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir t))))

;; Add source load paths to the front.
(add-to-list 'load-path ekg-bench-ekg-path)
(when (file-directory-p ekg-bench-llm-test-path)
  (add-to-list 'load-path ekg-bench-llm-test-path))

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
          (message "\nResult: %s — task:%s skill:%s memory:%s iters:%d time:%.0fs status-updates:%d max-gap:%s"
                   (ekg-agent-bench-result-name result)
                   (ekg-agent-bench--format-pass (ekg-agent-bench-result-task-passed result))
                   (ekg-agent-bench--format-pass (ekg-agent-bench-result-skill-passed result))
                   (ekg-agent-bench--format-pass (ekg-agent-bench-result-memory-passed result))
                   (ekg-agent-bench-result-iterations result)
                   (ekg-agent-bench-result-wall-time result)
                   (or (ekg-agent-bench-result-status-update-count result) 0)
                   (let ((gap (ekg-agent-bench-result-max-status-update-gap result)))
                     (if gap
                         (format "%.1fs" gap)
                       "n/a")))
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
