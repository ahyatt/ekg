;;; ekg-agent-llm-test.el --- LLM-driven integration tests for ekg-agent -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; LLM-driven integration tests for the ekg-agent, using the llm-test
;; framework.  These tests start a fresh Emacs subprocess, set up ekg
;; with a temporary database, and use an LLM agent to execute and
;; verify test scenarios described in YAML.
;;
;; To run these tests:
;;   1. Set `llm-test-provider' (the LLM that drives the test agent).
;;   2. Set `ekg-agent-llm-test-provider-form' to a form that creates
;;      the LLM provider for the ekg-agent subprocess.
;;   3. M-x eval-buffer on this file.
;;   4. M-x ert RET llm-test/ RET
;;
;; The tests require a working LLM provider both for the test agent
;; (llm-test) and for the ekg-agent inside the test Emacs subprocess.

;;; Code:

(require 'llm-test)

(defcustom ekg-agent-llm-test-provider-form nil
  "A form that constructs an LLM provider in the test subprocess.
This form will be evaluated in a fresh Emacs subprocess to create
the `ekg-llm-provider' used by ekg-agent during tests.

Example values:
  (make-llm-claude :key \"sk-...\" :chat-model \"claude-sonnet-4-20250514\")
  (progn (require \\='llm-openai)
         (make-llm-openai :key \"sk-...\" :chat-model \"gpt-4o\"))"
  :type 'sexp
  :group 'ekg-agent)

(defconst ekg-agent-llm-test--required-libraries
  '("ekg" "ekg-agent" "llm-test" "triples" "llm" "plz" "plz-media-type"
    "plz-event-source" "websocket" "async" "compat" "yaml")
  "Libraries whose load-path directories are needed by the test subprocess.
Libraries not found on `load-path' are silently skipped.

You may need to add more directories if your LLM provider has additional dependencies not in this list.")

(defun ekg-agent-llm-test--package-load-paths ()
  "Compute the load-path entries needed by the test Emacs subprocess.
Returns only directories for ekg and its actual dependencies, avoiding
unrelated packages that could cause conflicts."
  (let ((paths nil))
    (dolist (lib ekg-agent-llm-test--required-libraries)
      (let ((file (locate-library lib)))
        (when file
          (push (file-name-directory file) paths))))
    (delete-dups paths)))

(defun ekg-agent-llm-test--init-forms ()
  "Return the init forms for the test Emacs subprocess.
This configures the LLM provider for ekg-agent to use."
  `((require 'ekg)
    (require 'ekg-agent)
    ,@(when ekg-agent-llm-test-provider-form
        `((setq ekg-llm-provider ,ekg-agent-llm-test-provider-form)))))

;;;###autoload
(defun ekg-agent-llm-test-register ()
  "Register the ekg-agent LLM integration tests.
Passes load-path and init-forms to `llm-test-register-tests' for the
test Emacs subprocess, then registers all YAML test specs from
the testscripts/ directory."
  (interactive)
  (unless llm-test-provider
    (user-error "Set `llm-test-provider' before registering tests"))
  (unless ekg-agent-llm-test-provider-form
    (user-error "Set `ekg-agent-llm-test-provider-form' before registering tests"))
  (let ((testscripts-dir (expand-file-name
                          "testscripts"
                          (file-name-directory
                           (locate-library "ekg-agent")))))
    (llm-test-register-tests testscripts-dir
                             :load-path (ekg-agent-llm-test--package-load-paths)
                             :init-forms (ekg-agent-llm-test--init-forms))))

(provide 'ekg-agent-llm-test)

;;; ekg-agent-llm-test.el ends here
