;;; ekg-llm.el --- Using LLMs within, or via, ekg -*- lexical-binding: t -*-

;; Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Keywords: outlines, hypermedia
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
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
;; ekg-llm provides a way to interact with a language model using prompts that
;; are stored in ekg, and able to provide output to ekg notes. Notes can have
;; certain prompts associated with them by using "magic tags".
;;
;; This currently only works with Open AI's API, but could be extended to work
;; with others that also offer an API and a way to have structured return
;; values.

(require 'ekg)
(require 'ekg-embedding)
(require 'json)
(require 'org nil t)

;;; Code:

(cl-defstruct ekg-llm-prompt
  interactions temperature)

(cl-defstruct ekg-llm-prompt-interaction
  role content)

(defcustom ekg-llm-format-output '((org-mode . ekg-llm-format-output-org)
                                   (markdown-mode . ekg-llm-format-output-markdown)
                                   (text-mode . ekg-llm-format-output-text))
  "Alist of functions to format LLM output for different modes."
  :type '(alist :key-type symbol :value-type function)
  :group 'ekg-llm)

(defconst ekg-llm-trace-buffer "*ekg llm trace*"
  "Buffer to use for tracing the LLM interactions.")

(defvar ekg-llm-default-prompt "You are an all-around expert, and are providing helpful addendums to notes the user is writing.  The addendums could be insights from other fields, advice, quotations, or pointing out any issues you may find. The text of the note follows."
  "Default prompt to use for LLMs, if no other is found.")

(defun ekg-llm-prompt-prelude ()
  "Output a prelude to the prompt that mentions the mode."
  ;; Text mode doesn't really need anything.
  (concat
   (unless (eq major-mode 'text-mode)
    (format "All input in this prompt is in %s. "
            (pcase major-mode
              ('org-mode "emacs org-mode")
              ('markdown-mode "markdown")
              (_ (format "emacs %s" (symbol-name major-mode))))))
   "Anything inside an LLM_OUTPUT block is previous output you have given."))

(defun ekg-llm-prompt-to-text (prompt)
  "Convert PROMPT struct to a simple text structure."
  (mapconcat (lambda (i)
               (format "%s: %s"
                       (pcase (ekg-llm-prompt-interaction-role i)
                         ('user "User")
                         ('system "System")
                         ('assistant "Assistant"))
                       (ekg-llm-prompt-interaction-content i)))
             (ekg-llm-prompt-interactions prompt)
             "\n"))

(defun ekg-llm-send-prompt (prompt &optional return-json-spec)
  "Send PROMPT to the Open AI, and return the result.
If RETURN-JSON-SPEC is passed in, force the return to compliant
with that JSON spec, meaning the return value will be a JSON
struct represented in elisp."
  (unless ekg-embedding-api-key
    (error "To call Open AI API, provide the ekg-embedding-api-key"))
  (with-current-buffer (get-buffer-create ekg-llm-trace-buffer)
        (goto-char (point-max))
        (insert (format "PROMPT: %s\n" (ekg-llm-prompt-to-text prompt))))
  (let* ((resp (request "https://api.openai.com/v1/chat/completions"
                :type "POST"
                :headers `(("Authorization" . ,(format "Bearer %s" ekg-embedding-api-key))
                           ("Content-Type" . "application/json"))
                :data (json-encode `(("messages" . ,(mapcar (lambda (p) `(("role" . ,(pcase (ekg-llm-prompt-interaction-role p)
                                                                                       ('user "user")
                                                                                       ('system "system")
                                                                                       ('assistant "assistant")))
                                                                          ("content" . ,(ekg-llm-prompt-interaction-content p))))
                                                            (ekg-llm-prompt-interactions prompt)))
                                     ("model" . "gpt-3.5-turbo-0613")
                                     ;; Removed ability to set the temperature -
                                     ;; Open AI's model seems to go crazy when I
                                     ;; set it, and their documentation is
                                     ;; pretty inconsistent about temperature
                                     ;; values.
                                     ,@(when return-json-spec
                                         `(("functions" . ((("name" . "output")
                                                           ("parameters" .
                                                            ,return-json-spec))))
                                           ("function_call" . (("name" . "output")))))
                                     ))
                :parser 'json-read
                :error (cl-function (lambda (&key error-thrown data &allow-other-keys)
                                      (error (format "Problem calling Open AI: %s, type: %s message: %s"
                                                     (cdr error-thrown)
                                                     (assoc-default 'type (cdar data))
                                                     (assoc-default 'message (cdar data))))))
                :sync t)))
    (let ((result (cdr (assoc 'content (cdr (assoc 'message (aref (cdr (assoc 'choices (request-response-data resp))) 0))))))
          (func-result (cdr (assoc 'arguments (cdr (assoc 'function_call (cdr (assoc 'message (aref (cdr (assoc 'choices (request-response-data resp))) 0)))))))))
      (with-current-buffer (get-buffer-create ekg-llm-trace-buffer)
        (goto-char (point-max))
        (insert (format "RESULT: %s\n" (or func-result result))))
      (or func-result result))))

(defvar-local ekg-llm-interaction-func
    (lambda ()
      (interactive)
      (ekg-llm-send-and-use (ekg-llm-interaction-func 'append)
                            ekg-llm-default-prompt nil))
  "Function to call for an LLM to interact with the note.
If nil, then the LLM will not be used.")

(keymap-set ekg-capture-mode-map "C-c ."
            (lambda ()
              (interactive)
              (when ekg-llm-interaction-func
                (funcall ekg-llm-interaction-func))))
(keymap-set ekg-edit-mode-map "C-c ."
            (lambda ()
              (interactive)
              (when ekg-llm-interaction-func
                (funcall ekg-llm-interaction-func))))

(defun ekg-llm-structured-call (prompt arglist)
  "Produces programmatic output with PROMPT producing ARGLIST.
ARGLIST is in the form of the arglist of `cl-defmethod, but where
all arguments have to be a tuple of argname and one of `string',
`integer', `float', `boolean', or a list of the element `list'
and the type of list. All arguments are required at the moment.

The return value is a alist where each element is a key with the
respective argname and the value of the returned value, if any."
  (ekg-llm-structured-output-to-plist
   (json-read-from-string (ekg-llm-send-prompt
                           prompt
                           (ekg-llm-structured-call-arglist-to-schema arglist)))))

(defun ekg-llm-type-to-json-type (type)
  "Convert TYPE to a JSON type."
  (pcase type
    ('string "string")
    ('integer "integer")
    ('float "float")
    ('list "array")
    ('boolean "boolean")))

(defun ekg-llm-structured-call-arglist-to-schema (arglist)
  "Convert ARGLIST to a function schema for the OpenAI API.
ARGLIST is mostly in the form a `cl-defmethod' arglist, but where
lists also need to be tuples of the form `(list element-type)'."
  (let ((required)
        (properties))
    (cl-loop for req-or-opt in '(required optional) do
             (mapc (lambda (arg)
                     (when (eq req-or-opt 'required) (push (symbol-name (car arg)) required))
                     (push (cons (symbol-name (car arg))
                                 (cons
                                  (cons "type"
                                        (if (eq 'cons (type-of (nth 1 arg)))
                                            (ekg-llm-type-to-json-type (car (nth 1 arg)))
                                          (ekg-llm-type-to-json-type (nth 1 arg))))
                                  (when (eq (type-of (nth 1 arg)) 'cons)
                                    (list (cons
                                           "items"
                                           (list
                                            (cons "type"
                                                  (ekg-llm-type-to-json-type (nth 1 (nth 1 arg))))))))))
                           properties))
                   (let ((optional-marker (seq-position arglist '&optional)))
                     (if optional-marker
                         (if (eq req-or-opt 'required)
                             (seq-subseq arglist 0 optional-marker)
                           (seq-subseq arglist (1+ optional-marker)))
                       (when (eq req-or-opt 'required)
                         arglist)))))
    (list
     (cons "type" "object")
     (cons "required" (nreverse required))
     (cons "properties" (nreverse properties)))))

(defun ekg-llm-structured-output-to-plist (output)
  "Convert structured OUTPUT given by the OpenAI API to a plist."
  (let ((plist))
    (cl-loop for (key . value) in output do
             (setq plist
                   (plist-put plist (intern (format ":%s" key))
                              ;; Transform vectors into lists if encountered.
                              (if (eq (type-of value) 'vector)
                                  (append value nil)
                                value))))
    plist))

(defun ekg-llm-format-output-org (s)
  "Format S in org mode to denote it as LLM created."
  (format "#+BEGIN_LLM_OUTPUT\n%s\n#+END_LLM_OUTPUT\n" s))

(defun ekg-llm-format-output-markdown (s)
  "Format S in markdown mode to denote it as LLM created."
  (format "<!-- BEGIN_LLM_OUTPUT -->\n%s\n<!-- END_LLM_OUTPUT -->\n" s))

(defun ekg-llm-format-output-text (s)
  "Format S in text mode to denote it as LLM created."
  (format "BEGIN_LLM_OUTPUT\n%s\nEND_LLM_OUTPUT\n" s))

(defun ekg-llm-note-interactions (&optional prompt-prefix)
  "From an ekg note buffer, create the prompt for the LLM.
The return value is a list of `ekg-llm-prompt-interaction'
structs. PROMPT-PREFIX, when exists, will be used as a prefix to
the note text. If it is nil, use `ekg-llm-default-prompt'. This
will also add a standard "
  (list
   (make-ekg-llm-prompt-interaction
    :role 'system
    :content (ekg-llm-prompt-prelude))
   (make-ekg-llm-prompt-interaction
    :role 'system
    :content (or prompt-prefix ekg-llm-default-prompt))
   (make-ekg-llm-prompt-interaction
    :role 'user
    :content (substring-no-properties (ekg-edit-note-display-text)))))

(defun ekg-llm-send-and-use (consume-func &optional prompt temperature)
  "Run the LLM and pass response to CONSUME-FUNC.
If PROMPT is nil, use `ekg-llm-default-prompt'. TEMPERATURE is a
float between 0 and 1, controlling the randomness and creativity
of the response."
  (let ((output (ekg-llm-send-prompt
                   (make-ekg-llm-prompt
                    :temperature temperature
                    :interactions (ekg-llm-note-interactions prompt)))))
    (funcall consume-func output)))

(defun ekg-llm-interaction-func (interaction-type)
  "Return a function for each valid INTERACTION-TYPE.
The valid interaction types are `'append' and `'replace'."
  (pcase interaction-type
    ('append (lambda (output)
               (let ((formatter (cdr (assoc major-mode ekg-llm-format-output))))
                 (save-excursion
                   (goto-char (point-max))
                   (insert (if formatter
                               (funcall formatter output)
                             (format "\n%s\n" output)))))))
    ('replace (lambda (output) (save-excursion
                                 (goto-char (+ 1 (overlay-end (ekg--metadata-overlay))))
                                 (delete-region (point) (point-max))
                                 (insert output))))
    (_ (error "Invalid interaction type %s" interaction-type))))

(defun ekg-llm-set-interaction (prompt-title &optional interaction-type temperature)
  "Set prompt to the text of the note with PROMPT-TITLE.

INTERACTION-TYPE is one of `'append' or `'replace', to either append
to the text or replace it entirely.

If TEMPERATURE is non-nil, use it as the temperature for the LLM.
It should be a floating point number between 0 and 1."
  (setq-local ekg-llm-interaction-func
              (lambda ()
                (interactive)
                (let ((prompts (ekg-get-notes-with-title prompt-title)))
                  (unless (= 1 (length prompts))
                    (error "Should have exactly one note with title %s, instead there are %d" prompt-title (length prompts)))
                  (ekg-llm-send-and-use
                   (ekg-llm-interaction-func (or interaction-type 'append))
                   (substring-no-properties (ekg-display-note-text (car prompts)))
                   temperature)))))

(provide 'ekg-llm)

;;; ekg-llm.el ends here
