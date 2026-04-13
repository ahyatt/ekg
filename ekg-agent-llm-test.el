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
Writes a diagnostic line to /tmp/ekg-llm-test-init-diag.log so
provider-setup failures are visible post-hoc."
    `((let ((diag-file "/tmp/ekg-llm-test-init-diag.log"))
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
                (with-temp-buffer
                  (insert (format
                           "[%s] env-set:%s env-len:%d load-path-len:%d\n"
                           (format-time-string "%F %T")
                           (if provider-elisp "yes" "no")
                           (length (or provider-elisp ""))
                           (length load-path)))
                  (append-to-file (point-min) (point-max) diag-file))
                (when (and provider-elisp (not (string-empty-p provider-elisp)))
                  (condition-case provider-err
                      (progn
                        (setq ekg-llm-provider
                              (eval (read provider-elisp)))
                        (with-temp-buffer
                          (insert (format
                                   "[%s] provider-set:%s type:%s\n"
                                   (format-time-string "%F %T")
                                   (if ekg-llm-provider "yes" "no")
                                   (type-of ekg-llm-provider)))
                          (append-to-file (point-min) (point-max) diag-file)))
                    (error
                     (with-temp-buffer
                       (insert (format "[%s] provider-eval-error: %S\n"
                                       (format-time-string "%F %T")
                                       provider-err))
                       (append-to-file (point-min) (point-max)
                                       diag-file)))))))
          (error
           (message "ekg-agent-llm-test init error: %S" err)
           (ignore-errors
             (with-temp-buffer
               (insert (format "[%s] init-error: %S\n"
                               (format-time-string "%F %T") err))
               (append-to-file (point-min) (point-max) diag-file))))))))

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
