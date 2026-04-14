;;; run-bench.el --- Script to run ekg agent benchmarks in batch -*- lexical-binding: t -*-

;;; Commentary:
;; Run with: Emacs --batch -l run-bench.el
;; Or run a single task: EKG_BENCH_TASK=fix-elisp-bug Emacs --batch -l run-bench.el
;;
;; Configure the environment by setting the following variables:
;;
;; LLM_TEST_PROVIDER_ELISP: A sexp that will be evalled in the subprocess to
;;   create the LLM provider to run the agent.  It must support tool calling.
;;   Example: "(make-llm-openai :key \"sk-...\")"
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

(require 'ekg-agent-bench)

(unless (or (bound-and-true-p byte-compile-current-file)
            (boundp 'eldev-project-dir))
  ;; Sanity check: verify the subprocess can start and load ekg.
  (message "=== Sanity check: starting test Emacs ===")
  (let ((info (ekg-agent-bench--start-emacs)))
    (unwind-protect
        (progn
          (message "ekg: %s" (ekg-agent-bench--eval-in-emacs info "(featurep 'ekg)"))
          (message "ekg-agent: %s" (ekg-agent-bench--eval-in-emacs info "(featurep 'ekg-agent)"))
          (message "ekg-db: %s" (ekg-agent-bench--eval-in-emacs info "ekg-db-file"))
          (message "provider: %s" (ekg-agent-bench--eval-in-emacs info "(if ekg-llm-provider (format \"%s\" (type-of ekg-llm-provider)) \"UNCONFIGURED\")")))
      (ekg-agent-bench--stop-emacs info)))
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
            (when-let ((err (ekg-agent-bench-result-error-message result)))
              (message "Error: %s" err))
            (when-let ((log (ekg-agent-bench-result-agent-log result)))
              (unless (string-empty-p log)
                (message "\n--- agent log ---\n%s\n--- end agent log ---" log)))
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
              (kill-emacs 1))))))))

;;; run-bench.el ends here
