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
(require 'llm)
(require 'llm-fake)
(require 'json)
(require 'org nil t)

;;; Code:

(defcustom ekg-llm-format-output '((org-mode . ekg-llm-format-output-org)
                                   (markdown-mode . ekg-llm-format-output-markdown)
                                   (text-mode . ekg-llm-format-output-text))
  "Alist of functions to format LLM output for different modes."
  :type '(alist :key-type symbol :value-type function)
  :group 'ekg-llm)

(defcustom ekg-llm-query-num-notes 5
  "Number of notes to retrieve and send in a query prompt."
  :type 'integer
  :group 'ekg-llm)

(defcustom ekg-llm-query-prompt-intro
  "Given the following notes taken by the user, and your own knowledge, create a final answer that may, if needed, quote from the notes.  If you don't know the answer, tell the user that.  Never try to make up an answer."
  "Introductory text to use for the query prompt."
  :type 'string
  :group 'ekg-llm)

(defconst ekg-llm-provider nil
  "The provider of the embedding.
This is a struct representing a provider in the `llm' package.
The type and contents of the struct vary by provider.")

(defconst ekg-llm-trace-buffer "*ekg llm trace*"
  "Buffer to use for tracing the LLM interactions.")

(defconst ekg-llm-default-prompt "You are an all-around expert, and are providing helpful addendums to notes the user is writing.  The addendums could be insights from other fields, advice, quotations, or pointing out any issues you may find. The text of the note follows."
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

(defun ekg-llm-format-output-org (s)
  "Format S in org mode to denote it as LLM created."
  (format "#+BEGIN_LLM_OUTPUT\n%s\n#+END_LLM_OUTPUT\n" s))

(defun ekg-llm-format-output-markdown (s)
  "Format S in markdown mode to denote it as LLM created."
  (format "<!-- BEGIN_LLM_OUTPUT -->\n%s\n<!-- END_LLM_OUTPUT -->\n" s))

(defun ekg-llm-format-output-text (s)
  "Format S in text mode to denote it as LLM created."
  (format "BEGIN_LLM_OUTPUT\n%s\nEND_LLM_OUTPUT\n" s))

(defun ekg-llm-note-interactions ()
  "From an ekg note buffer, create the prompt for the LLM.
The return value is a list of `ekg-llm-prompt-interaction'
structs."
  (list
   (make-llm-chat-prompt-interaction
    :role 'system
    :content (ekg-llm-prompt-prelude))
   (make-llm-chat-prompt-interaction
    :role 'user
    :content (substring-no-properties (ekg-edit-note-display-text)))))

(defun ekg-llm-send-and-use (consume-func &optional prompt temperature)
  "Run the LLM and pass response to CONSUME-FUNC.
If PROMPT is nil, use `ekg-llm-default-prompt'. TEMPERATURE is a
float between 0 and 1, controlling the randomness and creativity
of the response."
  (funcall consume-func
           (llm-chat
            ekg-llm-provider
            (make-llm-chat-prompt
             :temperature temperature
             :context (or prompt ekg-llm-default-prompt)
             :interactions (ekg-llm-note-interactions)))))

(defun ekg-llm-interaction-func (interaction-type)
  "Return a function for each valid INTERACTION-TYPE.
The valid interaction types are `'append' and `'replace'."
  (pcase interaction-type
    ('append (lambda (output)
               (let ((formatter (cdr (assoc major-mode ekg-llm-format-output))))
                 (save-excursion
                   (goto-char (point-max))
                   (insert "\n"
                           (if formatter
                               (funcall formatter output)
                             (format "\n%s\n" output)))))))
    ('replace (lambda (output) (save-excursion
                                 (goto-char (+ 1 (overlay-end (ekg--metadata-overlay))))
                                 (delete-region (point) (point-max))
                                 (insert output))))
    (_ (error "Invalid interaction type %s" interaction-type))))

(defun ekg-llm-debug-query ()
  "Instead of sending the query to the LLM, just display it."
  (interactive)
  (let ((ekg-llm-provider (make-llm-fake :output-to-buffer "*ekg llm trace*")))
    (funcall ekg-llm-interaction-func)))

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

(defun ekg-llm-note-metadata-for-input (note)
  "Return a brief description of the metdata of NOTE.
The description is appropriate for input to a LLM. This is
designed to be on a line of its own. It does not return a
newline."
  (let ((title (plist-get (ekg-note-properties note) :titled/title))
        (tags (ekg-note-tags note))
        (created (ekg-note-creation-time note))
        (modified (ekg-note-modified-time note)))
    (format "Note: %s"
            (string-join
             (remove
              "" (list
                  (if title (format "Title: %s" title) "")
                  (if tags (format "Tags: %s" (mapconcat 'identity tags ", ")) "")
                  (if created (format "Created: %s" (format-time-string "%Y-%m-%d" created)) "")
                  (if modified (format "Modified: %s" (format-time-string "%Y-%m-%d" modified)) "")))
             ", "))))

(defun ekg-llm-query-with-notes (query)
  "Query the LLM with QUERY, including relevant notes in the prompt.
The answer will appear in a new buffer"
  (interactive "sQuery: ")
  (let ((notes (mapcar #'ekg-get-note-with-id
                       (ekg-embedding-n-most-similar-notes (llm-embedding ekg-embedding-provider query)
                                                           ekg-llm-query-num-notes)))
        (buf (get-buffer-create
              (format "*ekg llm query '%s'*" (ekg-truncate-at query 5)))))
    (with-current-buffer buf
      (erase-buffer)
      (funcall
       (ekg-llm-interaction-func 'append)
       (llm-chat ekg-llm-provider
        (make-llm-chat-prompt
         :context ekg-llm-query-prompt-intro
         :interactions
         (append
          (mapcar
           (lambda (note)
             (make-llm-chat-prompt-interaction
              :role 'user
              :content
              (format "%s\n%s" (ekg-llm-note-metadata-for-input note)
                      (substring-no-properties (ekg-display-note-text note)))))
           notes)
          (list (make-llm-chat-prompt-interaction
                 :role 'user
                 :content (format "Query: %s" query))))))))
    (pop-to-buffer buf)))

(provide 'ekg-llm)

;;; ekg-llm.el ends here
