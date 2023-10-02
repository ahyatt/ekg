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

(defcustom ekg-llm-format-output '((org-mode . ("#+BEGIN_LLM_OUTPUT" . "#+END_LLM_OUTPUT"))
                                   (markdown-mode . ("<!-- BEGIN_LLM_OUTPUT -->" . "<!-- END_LLM_OUTPUT -->"))
                                   (text-mode . ("BEGIN_LLM_OUTPUT" . "END_LLM_OUTPUT")))
  "Alist of functions to format LLM output for different modes."
  :type '(alist :key-type symbol :value-type (cons string string))
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

(defcustom ekg-llm-prompt-tag "prompt"
  "The tag to use to denote a prompt. Notes tagged with this and
other tags will be used as prompts for those other tags."
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

(defvar ekg-llm-prompt-history nil
  "History of prompts used in the LLM.")

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

(defun ekg-llm-prompt-for-note (note)
  "Return the prompt for NOTE, using the tags on the note.
Return value is a string. This is calculated by looking at the
tags on the note, and finding the ones that are co-occuring with
the ekg-llm-prompt-tag. The prompt will be built up from
appending the prompts together, in the order of the tags in the
note.

If there are no prompts on any of the note tags, use
`ekg-llm-default-prompt'."
  (ekg--update-from-metadata)  ;; so we can get the latest tags
  (let ((prompt-notes (ekg-get-notes-cotagged-with-tags
                       (ekg-note-tags note) ekg-llm-prompt-tag)))
    (if prompt-notes
        (mapconcat (lambda (prompt-note)
                     (string-trim
                      (substring-no-properties (ekg-display-note-text prompt-note))))
                   prompt-notes "\n")
      ekg-llm-default-prompt)))

(defun ekg-llm--send-and-process-note (arg interaction-type)
  "Resolve the note prompt and send to LLM with the INTERACTION-TYPE.
ARG comes from the calling function's prefix arg."
  (interactive)
  (let* ((prompt-initial (ekg-llm-prompt-for-note ekg-note))
         (prompt-for-use (if arg
                             ;; The documentation is clear this isn't correct -
                             ;; the INITIAL-CONTENTS variable is deprecated.
                             ;; However, it's the only way I know to prepopulate
                             ;; the minibuffer, which is important because the
                             ;; whole idea is that the user can edit the prompt
                             ;; this way.
                             (read-string "Prompt: " prompt-initial 'ekg-llm-prompt-history prompt-initial t)
                           prompt-initial)))
    (ekg-llm-send-and-use (ekg-llm-interaction-func interaction-type) prompt-for-use)))

(defun ekg-llm-send-and-append-note (&optional arg)
  "Send the note text to the LLM, appending the result.
The prompt text is defined by the set of tags and their
co-occurence with a prompt tag.

ARG, if nonzero and nonnil, will let the user edit the prompt
sent before it goes to the LLM.

The text will be appended to the end of the note."
  (interactive "P")
  (ekg-llm--send-and-process-note arg 'append))

(defun ekg-llm-send-and-replace-note (&optional arg)
  "Replace note text with the result of sending the text to an LLM.
The prompt text is defined by the set of tags and their
co-occurence with a prompt tag.

ARG, if nonzero and nonnil, will let the user edit the prompt
sent before it goes to the LLM.

The note text will be replaced by the result of the LLM."
  (interactive "P")
  (ekg-llm--send-and-process-note arg 'replace))

(keymap-set ekg-capture-mode-map "C-c ." #'ekg-llm-send-and-append-note)
(keymap-set ekg-edit-mode-map "C-c ." #'ekg-llm-send-and-append-note)
(keymap-set ekg-capture-mode-map "C-c ," #'ekg-llm-send-and-replace-note)
(keymap-set ekg-edit-mode-map "C-c ," #'ekg-llm-send-and-replace-note)

(defun ekg-llm-create-output-holder (prefix suffix)
  "Create a marker pair for the output of the LLM.
PREFIX and SUFFIX surround the marker, which are inserted into
the current buffer."
  (save-excursion
   (insert prefix "\n")
   (let ((start (make-marker))
         (end (make-marker)))
     (set-marker start (point))
     (set-marker end (point))
     (set-marker-insertion-type start nil)
     (insert "\n")
     (set-marker-insertion-type end t)
     (insert suffix "\n")
     (cons start end))))

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

(defun ekg-llm-send-and-use (marker-func &optional prompt temperature)
  "Run the LLM and replace markers supplied by MARKER-FUNC.
If PROMPT is nil, use `ekg-llm-default-prompt'. TEMPERATURE is a
float between 0 and 1, controlling the randomness and creativity
of the response."
  (let ((markers (funcall marker-func))
        (prompt (make-llm-chat-prompt
                 :temperature temperature
                 :context (or prompt ekg-llm-default-prompt)
                 :interactions (ekg-llm-note-interactions))))
    (cl-flet ((insert-text (text)
                ;; Erase and insert the new text between the marker cons.
                (let ((start-marker (car markers))
                      (end-marker (cdr markers)))
                  (with-current-buffer (marker-buffer start-marker)
                    (save-excursion
                      (goto-char start-marker)
                      (delete-region start-marker end-marker)
                      (insert text))))))
      (condition-case nil
          (llm-chat-streaming ekg-llm-provider
                              prompt
                              (lambda (partial-response)
                                (insert-text partial-response))
                              (lambda (final-response)
                                (insert-text final-response))
                              (lambda (_ msg)
                                (error "LLM error: %s" msg)))
        (not-implemented
         ;; Fallback to synchronous chat if streaming isn't supported.
         (message "Streaming not supported, falling back to synchronous chat, which may take around 10 seconds.")
         (insert-text (llm-chat ekg-llm-provider prompt)))))))

(defun ekg-llm-interaction-func (interaction-type)
  "Return a function for each valid INTERACTION-TYPE.
The valid interaction types are `'append' and `'replace'."
  (pcase interaction-type
    ('append (lambda ()
               (let ((enclosure (assoc-default major-mode ekg-llm-format-output nil '("_BEGIN_" . "_END_"))))
                 (save-excursion
                   (goto-char (point-max))
                   (insert "\n")
                   (ekg-llm-create-output-holder (car enclosure) (cdr enclosure))))))
    ('replace (lambda ()
                (save-excursion
                  (goto-char (+ 1 (overlay-end (ekg--metadata-overlay))))
                  (let ((start (make-marker))
                        (end (make-marker)))
                    (set-marker start (point))
                    (set-marker end (point-max))
                    (set-marker-insertion-type end t)
                    (cons start end)))))
    (_ (error "Invalid interaction type %s" interaction-type))))

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
      (let ((prompt (make-llm-chat-prompt
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
                             :content (format "Query: %s" query)))))))
        (condition-case nil
            (llm-chat-streaming ekg-llm-provider
                                prompt
                                (lambda (text)
                                  (with-current-buffer buf
                                    (erase-buffer)
                                    (insert text)))
                                (lambda (text)
                                  (with-current-buffer buf
                                    (erase-buffer)
                                    (insert text)))
                                (lambda (_ msg)
                                  (error "Could not call LLM: %s" msg)))
          (not-implemented (llm-chat ekg-llm-provider prompt)))))
    (pop-to-buffer buf)))

(provide 'ekg-llm)

;;; ekg-llm.el ends here
