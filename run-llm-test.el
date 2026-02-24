;;; run-llm-test.el --- Script to run ekg LLM tests in batch -*- lexical-binding: t -*-
;;; Commentary:
;; Run with: emacs --batch -l run-llm-test.el
;;; Code:

;; Add needed load paths
(add-to-list 'load-path (expand-file-name "~/src/ekg"))
(add-to-list 'load-path (expand-file-name "~/src/llm-test"))

;; Add all elpa dirs
(let ((elpa-dir (expand-file-name "~/.emacs.d/elpa")))
  (when (file-directory-p elpa-dir)
    (dolist (dir (directory-files elpa-dir t "\\`[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; Add claude-oauth
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(require 'llm-test)
(require 'claude-oauth)
(require 'ekg-agent-llm-test)

(setq llm-test-provider (make-llm-claude-oauth :chat-model "claude-sonnet-4-6"))
(setq llm-test-max-iterations 20)
(setq llm-test-timeout 30)

;; Quick sanity check
(message "=== Sanity: testing subprocess with load-path ===")
(require 'ekg-agent-llm-test)
(let* ((lp (ekg-agent-llm-test--package-load-paths))
       (init (ekg-agent-llm-test--init-forms))
       (info (llm-test--start-emacs :load-path lp :init-forms init)))
  (unwind-protect
      (progn
        (message "ekg available: %s"
                 (llm-test--eval-in-emacs info "(require 'ekg nil t)"))
        (message "ekg-agent available: %s"
                 (llm-test--eval-in-emacs info "(require 'ekg-agent nil t)")))
    (llm-test--stop-emacs info)))
(message "=== Sanity OK, running test ===")

;; Register tests
(ekg-agent-llm-test-register)

;; Run a specific test
(let ((test-name (intern (or (getenv "EKG_LLM_TEST") "llm-test/ekg-agent-basics/1"))))
  (message "Running test: %s" test-name)
  (ert-run-tests-batch test-name))

;;; run-llm-test.el ends here
