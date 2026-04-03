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
;; EKG_BENCH_PROVIDER_FORM environment variable.

;;; Code:

(require 'ert)

;; Guard: llm-test may not be installed (it's a local dependency).
(unless (require 'llm-test nil t)
  ;; When llm-test is unavailable, define a single skipped test so eldev does
  ;; not error on loading this file.
  (ert-deftest ekg-agent-llm-test/unavailable ()
    "Placeholder — llm-test library not found."
    (ert-skip "llm-test library not available")))

(when (featurep 'llm-test)
  (require 'ekg-agent-bench)

  (defvar ekg-agent-llm-test--provider-form
    (when (getenv "EKG_BENCH_PROVIDER_FORM")
      (read (getenv "EKG_BENCH_PROVIDER_FORM")))
    "Provider form read from the environment, or nil.")

  (defun ekg-agent-llm-test--register ()
    "Register llm-test YAML specs from llm-tests/ as ERT tests."
    (let* ((dir (expand-file-name
                 "llm-tests"
                 (file-name-directory
                  (or load-file-name
                      (locate-library "ekg-agent-llm-test")))))
           (groups (llm-test-load-directory dir))
           (load-paths (ekg-agent-bench--compute-load-paths))
           (init-forms '((setq load-prefer-newer t))))
      (dolist (group groups)
        (let ((group-slug (llm-test--slugify (llm-test-group-name group)))
              (setup (llm-test-group-setup group)))
          (cl-loop
           for spec in (llm-test-group-tests group)
           for idx from 1
           for test-name = (intern (format "ekg-agent-llm-test/%s/%d"
                                           group-slug idx))
           do (let ((the-spec spec)
                    (the-setup setup)
                    (the-load-paths load-paths)
                    (the-init-forms init-forms))
                (ert-set-test
                 test-name
                 (make-ert-test
                  :name test-name
                  :documentation (format "LLM test: %s (test %d)\n%s"
                                         (llm-test-group-name group)
                                         idx
                                         (llm-test-spec-description the-spec))
                  :body (lambda ()
                          (unless ekg-agent-llm-test--provider-form
                            (ert-skip "EKG_BENCH_PROVIDER_FORM not set"))
                          (let* ((provider (eval ekg-agent-llm-test--provider-form t))
                                 (emacs-info (llm-test--start-emacs
                                              :extra-load-path the-load-paths
                                              :init-forms the-init-forms)))
                            (unwind-protect
                                (let ((result (llm-test--run-test
                                               provider emacs-info
                                               the-setup the-spec)))
                                  (llm-test--report-result result))
                              (llm-test--stop-emacs emacs-info))))))))))))

  (ekg-agent-llm-test--register))

(provide 'ekg-agent-llm-test)

;;; ekg-agent-llm-test.el ends here
