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
;;   1. Ensure `llm-test-provider' is configured (or set it below).
;;   2. M-x eval-buffer on this file.
;;   3. M-x ert RET llm-test/ RET
;;
;; The tests require a working LLM provider both for the test agent
;; (llm-test) and for the ekg-agent inside the test Emacs subprocess.

;;; Code:

(require 'llm-test)

(defconst ekg-agent-llm-test--required-libraries
  '("ekg" "ekg-agent" "llm-test" "triples" "llm" "plz" "plz-media-type"
    "plz-event-source" "websocket" "async" "compat" "yaml"
    "claude-oauth")
  "Libraries whose load-path directories are needed by the test subprocess.
Libraries not found on `load-path' are silently skipped.")

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
  ;; The test Emacs needs an LLM provider for ekg-agent.
  ;; We use the same provider as llm-test-provider, reconstructed
  ;; from its printed representation.
  (let ((provider-form (ekg-agent-llm-test--provider-form)))
    `((require 'ekg)
      (require 'ekg-agent)
      ,@(when provider-form
          `((setq ekg-llm-provider ,provider-form))))))

(defun ekg-agent-llm-test--provider-form ()
  "Return a form that reconstructs `llm-test-provider' in the subprocess.
Returns nil if the provider type is not recognized."
  (when llm-test-provider
    (cond
     ;; Claude OAuth — requires claude-oauth.el on load-path
     ((and (fboundp 'llm-claude-oauth-p)
           (llm-claude-oauth-p llm-test-provider))
      `(progn
         (require 'claude-oauth)
         (make-llm-claude-oauth
          :chat-model ,(llm-claude-chat-model llm-test-provider))))
     ;; Standard Claude with API key
     ((and (fboundp 'llm-claude-p) (llm-claude-p llm-test-provider))
      `(make-llm-claude
        :key ,(llm-claude-key llm-test-provider)
        :chat-model ,(llm-claude-chat-model llm-test-provider)))
     ;; OpenAI
     ((and (fboundp 'llm-openai-p) (llm-openai-p llm-test-provider))
      `(progn
         (require 'llm-openai)
         (make-llm-openai
          :key ,(llm-openai-key llm-test-provider)
          :chat-model ,(llm-openai-chat-model llm-test-provider))))
     (t
      (warn "ekg-agent-llm-test: unrecognized provider type %S, \
test Emacs will not have an LLM provider"
            (type-of llm-test-provider))
      nil))))

;;;###autoload
(defun ekg-agent-llm-test-register ()
  "Register the ekg-agent LLM integration tests.
Passes load-path and init-forms to `llm-test-register-tests' for the
test Emacs subprocess, then registers all YAML test specs from
the testscripts/ directory."
  (interactive)
  (unless llm-test-provider
    (user-error "Set `llm-test-provider' before registering tests"))
  (let ((testscripts-dir (expand-file-name
                          "testscripts"
                          (file-name-directory
                           (locate-library "ekg-agent")))))
    (llm-test-register-tests testscripts-dir
                             :load-path (ekg-agent-llm-test--package-load-paths)
                             :init-forms (ekg-agent-llm-test--init-forms))))

(provide 'ekg-agent-llm-test)

;;; ekg-agent-llm-test.el ends here
