;;; ekg-agent-llm-test.el --- LLM-driven integration tests for ekg-agent -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Andrew Hyatt <ahyatt@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; LLM-driven integration tests for ekg-agent, using the llm-test framework.
;; These tests launch a fresh Emacs subprocess and use an LLM agent to
;; interpret natural-language test specifications from YAML files in the
;; llm-tests/ directory.
;;
;; Tests are skipped unless an LLM provider is configured via the
;; LLM_TEST_PROVIDER_ELISP environment variable.

;;; Code:

(require 'ert)
(require 'subr-x)

;; Guard: llm-test may not be installed (it's a local dependency).
(unless (require 'llm-test nil t)
  ;; When llm-test is unavailable, define a single skipped test so eldev does
  ;; not error on loading this file.
  (ert-deftest ekg-agent-llm-test/unavailable ()
    "Placeholder — llm-test library not found."
    (ert-skip "llm-test library not available")))

(when (and (featurep 'llm-test)
           (let ((p (getenv "LLM_TEST_PROVIDER_ELISP")))
             (and p (not (string-empty-p p)))))
  (require 'ekg-agent-bench)

  (defun ekg-agent-llm-test--init-forms ()
    "Return init forms for the llm-test subprocess.
Sets up a temp ekg database, requires the relevant ekg modules,
connects, and configures `ekg-llm-provider' from the
LLM_TEST_PROVIDER_ELISP environment variable.  Wrapped in a
`condition-case' so init failures don't hang the daemon.
Diagnostics are written to /tmp/ekg-llm-test-init-diag.log so
provider-setup failures and silent agent errors are visible
post-hoc."
    `((require 'cl-lib)
      (let ((diag-file "/tmp/ekg-llm-test-init-diag.log"))
        (cl-flet ((diag (fmt &rest args)
                    (ignore-errors
                      (with-temp-buffer
                        (insert (apply #'format fmt args))
                        (append-to-file (point-min) (point-max)
                                        diag-file)))))
          (condition-case err
              (progn
                (setq load-prefer-newer t)
                (setq ekg-db-file (make-temp-file "ekg-llm-test-db"))
                (require 'ekg)
                (require 'ekg-agent)
                (require 'ekg-llm)
                (require 'ekg-org)
                (ekg-connect)
                (let ((provider-elisp (getenv "LLM_TEST_PROVIDER_ELISP")))
                  (diag "[%s] env-set:%s env-len:%d load-path-len:%d\n"
                        (format-time-string "%F %T")
                        (if provider-elisp "yes" "no")
                        (length (or provider-elisp ""))
                        (length load-path))
                  (diag "  locate llm-openai: %s\n"
                        (locate-library "llm-openai"))
                  (diag "  featurep llm-openai (before require): %s\n"
                        (featurep 'llm-openai))
                  ;; Force-require llm-openai at init time so struct
                  ;; constructors like `make-llm-openrouter' are
                  ;; defined before the provider eval reaches them.
                  (ignore-errors (require 'llm-openai))
                  (diag "  featurep llm-openai (after require): %s\n"
                        (featurep 'llm-openai))
                  (diag "  fboundp make-llm-openrouter: %s\n"
                        (fboundp 'make-llm-openrouter))
                  (diag "  fboundp make-llm-openai: %s\n"
                        (fboundp 'make-llm-openai))
                  (when (and provider-elisp (not (string-empty-p provider-elisp)))
                    (condition-case provider-err
                        (progn
                          (setq ekg-llm-provider
                                (eval (read provider-elisp)))
                          (diag "[%s] provider-set:%s type:%s\n"
                                (format-time-string "%F %T")
                                (if ekg-llm-provider "yes" "no")
                                (type-of ekg-llm-provider)))
                      (error
                       (diag "[%s] provider-eval-error: %S\n"
                             (format-time-string "%F %T")
                             provider-err)))))
                ;; In daemon mode, (current-message) does not retain
                ;; transient messages after the command that produced
                ;; them returns, so the frame-state capture's
                ;; `message' field is always nil.  Intercept `message'
                ;; to store the most recent non-empty call and have
                ;; `current-message' return it while it's "fresh".
                (defvar ekg-agent-llm-test--last-message nil
                  "Pair of (TIMESTAMP . MESSAGE) for the most recent `message' call.")
                (advice-add
                 'message :after
                 (lambda (fmt &rest args)
                   (ignore-errors
                     (when (stringp fmt)
                       (let ((text (apply #'format fmt args)))
                         (unless (string-empty-p text)
                           (setq ekg-agent-llm-test--last-message
                                 (cons (float-time) text)))))))
                 '((name . ekg-llm-test-last-message)))
                (advice-add
                 'current-message :around
                 (lambda (orig &rest args)
                   (or (apply orig args)
                       (and ekg-agent-llm-test--last-message
                            (< (- (float-time)
                                  (car ekg-agent-llm-test--last-message))
                               30)
                            (cdr ekg-agent-llm-test--last-message))))
                 '((name . ekg-llm-test-last-message)))
                (diag "[%s] advices attached\n"
                      (format-time-string "%F %T")))
            (error
             (message "ekg-agent-llm-test init error: %S" err)
             (diag "[%s] init-error: %S\n"
                   (format-time-string "%F %T") err)))))))

  (defun ekg-agent-llm-test--register ()
    "Register llm-test YAML specs from llm-tests/ as ERT tests."
    ;; Use the same Emacs binary as the host, to avoid version/build
    ;; mismatches between host and daemon (e.g. Homebrew vs Emacs.app).
    (setq llm-test-emacs-executable
          (ekg-agent-bench--emacs-executable))
    (let ((dir (expand-file-name
                "llm-tests"
                (file-name-directory
                 (or load-file-name
                     (locate-library "ekg-agent-llm-test")))))
          (load-paths (ekg-agent-bench--compute-load-paths))
          (init-forms (ekg-agent-llm-test--init-forms)))
      (llm-test-register-tests dir
                               :extra-load-path load-paths
                               :init-forms init-forms)))

  (ekg-agent-llm-test--register))

(provide 'ekg-agent-llm-test)

;;; ekg-agent-llm-test.el ends here
