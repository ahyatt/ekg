;;; ekg-llm.el --- Using LLMs within, or via, ekg -*- lexical-binding: t -*-

;; Copyright (c) 2023-2026  Andrew Hyatt <ahyatt@gmail.com>

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
;; are stored in ekg, and able to provide output to ekg notes.  Notes can have
;; certain prompts associated with them by using "magic tags".
;;
;; This currently only works with Open AI's API, but could be extended to work
;; with others that also offer an API and a way to have structured return
;; values.

(require 'ekg)
(require 'ekg-embedding)
(require 'llm)
(require 'llm-prompt)
(require 'json)
(require 'map)
(require 'seq)
(require 'org nil t)

;;; Code:

(defcustom ekg-llm-generated-tag "llm-generated"
  "Tag applied to notes containing LLM-generated responses.
Set this to nil to create generated responses without a special tag."
  :type '(choice (const :tag "Do not tag generated responses" nil)
                 string)
  :group 'ekg-llm)

(defcustom ekg-llm-query-num-notes 5
  "Number of notes to retrieve and send in a query prompt."
  :type 'integer
  :group 'ekg-llm)

(llm-defprompt ekg-llm-note-query-prompt
  "Given the following notes taken by the user, and your own
knowledge, create a final answer that may, if needed, quote from
the notes.  If you don't know the answer, tell the user that.
Never try to make up an answer.

{{notes}}
")

(defcustom ekg-llm-prompt-tag "prompt"
  "The tag to use to denote a prompt.
Notes tagged with this and other tags will be used as prompts for
those other tags."
  :type 'string
  :group 'ekg-llm)

(defun ekg-llm--response-inheritable-tag-p (tag _note)
  "Return non-nil when TAG is topical rather than LLM provenance."
  (not (member tag (delq nil (list ekg-llm-generated-tag
                                   ekg-llm-prompt-tag)))))

(add-hook 'ekg-response-tag-filter-functions
          #'ekg-llm--response-inheritable-tag-p)

(defconst ekg-llm-provider nil
  "The provider of the embedding.
This is a struct representing a provider in the `llm' package.
The type and contents of the struct vary by provider.

It can also be a list of providers, in which case the first one is the
default.")

(defconst ekg-llm-trace-buffer "*ekg llm trace*"
  "Buffer to use for tracing the LLM interactions.")

(defconst ekg-llm-default-instructions
  "You are an all-around expert, and are providing helpful addendums
to notes the user is writing.  The addendums could be insights
from other fields, advice, quotations, pointing out any issues
you may find in the text of the note, or answering direct
questions posed in the notes.")

(llm-defprompt ekg-llm-fill-prompt
  "The user has written a note, and would like you to append to it,
to make it more useful.  This is important: only output your
additions, and do not repeat anything in the user's note.  Write
as a third party adding information to a note, so do not use the
first person.

First, I'll give you information about the note, then similar
other notes that user has written, in JSON.  Finally, I'll give
you instructions.  The user's note will be your input, all the
rest, including this, is just context for it.  The notes given
are to be used as background material, which can be referenced in
your answer.

The user's note uses tags: {{tags}}.  The notes with the same
tags, listed here in reverse date order: {{tag-notes:10}}

These are similar notes in general, which may have duplicates
from the ones above: {{similar-notes:1}}

The hierarchy around the note follows as JSON.  The
`ancestor_chain' runs from the root to the immediate parent.  The
`current_note' is the note to answer, and its nested `children'
contain every recursive descendant.  Each entry states its
relationship, signed depth relative to the current note, note ID,
and parent ID.  Sibling order is not meaningful.  The current
entry omits `note.text' because its exact text is the user message.

{{note-hierarchy}}

This ends the section on useful notes as a background for the
note in question.

Your instructions on what content to add to the note:

{{instructions}}
")

(defvar ekg-llm-prompt-history nil
  "History of prompts used in the LLM.")

(defvar ekg-llm-ignored-props-for-json '(:embedding/embedding))

(defvar ekg-llm-note-numwords 500000
  "The maximum number of words to include from the note in the prompt.
This is a safeguard against sending too much data to the LLM, however,
we want to try to include as much information as possible.")

(defun ekg-llm-prompt-prelude ()
  "Output a prelude describing the input and output formats."
  ;; Text mode doesn't really need anything.
  (concat
   (unless (eq major-mode 'text-mode)
     (format "All input in this prompt is in %s. "
             (pcase major-mode
               ('org-mode "emacs org-mode")
               ('markdown-mode "markdown")
               (_ (format "emacs %s" (symbol-name major-mode))))))
   "Return only the response to the note, formatted as Markdown."))

(defun ekg-llm--context-tags (note)
  "Return the tags relevant to prompting from NOTE and its ancestors."
  (seq-uniq
   (seq-remove
    (lambda (tag) (equal tag ekg-llm-generated-tag))
    (mapcan (lambda (entry) (copy-sequence (ekg-note-tags entry)))
            (append (ekg-note-ancestors note) (list note))))))

(defun ekg-llm-instructions-for-note (note)
  "Return the prompt for NOTE, using the tags on the note.
Return value is a string.  This is calculated by looking at the
tags on the note, and finding the ones that are co-occuring with
the ekg-llm-prompt-tag.  The instructions will be built up from
appending the prompts together, in the order of the tags in the
note.

If there are no prompts on any of the note tags, use
`ekg-llm-default-instructions'."
  (let ((prompt-notes (ekg-get-notes-cotagged-with-tags
                       (ekg-llm--context-tags note)
                       ekg-llm-prompt-tag)))
    (if prompt-notes
        (mapconcat
         (lambda (prompt-note)
           (string-trim
            (substring-no-properties (ekg-display-note-text prompt-note ekg-llm-note-numwords))))
         prompt-notes "\n")
      ekg-llm-default-instructions)))

(defun ekg-llm--send-and-process-note (arg)
  "Resolve note instructions and create an LLM response note.
ARG comes from the calling function's prefix argument."
  (interactive)
  (let* ((instructions-initial (ekg-llm-instructions-for-note ekg-note))
         (instructions-for-use (if arg
                                   ;; The documentation is clear this isn't correct -
                                   ;; the INITIAL-CONTENTS variable is deprecated.
                                   ;; However, it's the only way I know to prepopulate
                                   ;; the minibuffer, which is important because the
                                   ;; whole idea is that the user can edit the
                                   ;; instructions this way.
                                   (read-string "Prompt: " instructions-initial 'ekg-llm-prompt-history instructions-initial t)
                                 instructions-initial)))
    (ekg-llm-send-and-use instructions-for-use
                          (if arg
                              (read-number "Temperature: " 0.5)
                            0.5)
                          (when (and arg (listp ekg-llm-provider))
                            (let ((provider-alist (mapcar (lambda (provider)
                                                            (cons (llm-name provider)
                                                                  provider))
                                                          ekg-llm-provider)))
                              (assoc-default (completing-read "Provider: "
                                                              provider-alist
                                                              nil t) provider-alist))))))

(defun ekg-llm-respond-to-note (&optional arg)
  "Send the note text to the LLM and store its response as a child note.
The prompt text is defined by the set of tags and their
co-occurence with a prompt tag.

ARG, if nonzero and nonnil, will let the user edit the prompt
sent before it goes to the LLM.

The response is stored as Markdown and tagged with
`ekg-llm-generated-tag'."
  (interactive "P")
  (when (buffer-modified-p)
    (if ekg-edit-mode
        (ekg-edit-save)
      (user-error "Save the note before requesting an LLM response")))
  (unless (ekg-note-with-id-exists-p (ekg-note-id ekg-note))
    (user-error "Save the note before requesting an LLM response"))
  (ekg-llm--send-and-process-note arg))

(define-obsolete-function-alias 'ekg-llm-send-and-append-note
  #'ekg-llm-respond-to-note "0.10.0")

(defun ekg-llm-send-and-replace-note (&optional arg)
  "Signal that replacing a note with LLM output is no longer supported.
ARG is retained for compatibility and ignored."
  (interactive "P")
  (ignore arg)
  (user-error "LLM output is now stored as a response; use `ekg-llm-respond-to-note'"))

(defun ekg-llm-preview-prompt (&optional arg)
  "Preview the complete prompt that would be sent to the LLM.
This includes both the context/system prompt and the user message.
The prompt text is defined by the set of tags and their
co-occurence with a prompt tag.

ARG, if nonzero and nonnil, will let the user edit the prompt
instructions before previewing.

This is for debugging purposes."
  (interactive "P")
  (let* ((instructions-initial (ekg-llm-instructions-for-note ekg-note))
         (instructions-for-use (if arg
                                   (read-string "Prompt: " instructions-initial 'ekg-llm-prompt-history instructions-initial t)
                                 instructions-initial))
         (provider (ekg-llm--provider))
         (context-tags (ekg-llm--context-tags ekg-note))
         (context-prompt (concat (ekg-llm-prompt-prelude) "\n"
                                 (llm-prompt-fill
                                  'ekg-llm-fill-prompt
                                  provider
                                  :instructions instructions-for-use
                                  :tags (mapconcat #'identity context-tags ", ")
                                  :tag-notes (ekg-llm-make-any-tag-generator context-tags
                                                                             (ekg-note-id ekg-note))
                                  :similar-notes (ekg-llm-make-similar-note-generator ekg-note)
                                  :note-hierarchy (ekg-llm-note-hierarchy-context
                                                   ekg-note))))
         (interactions (ekg-llm-note-interactions))
         (buf (get-buffer-create "*ekg llm prompt preview*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== COMPLETE LLM PROMPT PREVIEW ===\n\n")
      (insert "=== CONTEXT/SYSTEM PROMPT ===\n")
      (insert context-prompt)
      (insert "\n\n=== USER INTERACTIONS ===\n")
      (dolist (interaction interactions)
        (insert (format "Role: %s\n" (llm-chat-prompt-interaction-role interaction)))
        (insert (format "Content:\n%s\n\n" (llm-chat-prompt-interaction-content interaction))))
      (insert "=== END PROMPT PREVIEW ===\n")
      (goto-char (point-min))
      (text-mode))
    (pop-to-buffer buf)))

(defvar ekg-llm-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .") #'ekg-llm-respond-to-note)
    (define-key map (kbd "C-c ?") #'ekg-llm-preview-prompt)
    map)
  "Keymap for ekg-llm bindings in capture and edit modes.")

(define-minor-mode ekg-llm-minor-mode
  "Minor mode providing LLM keybindings in ekg capture/edit buffers."
  :lighter nil
  :keymap ekg-llm-capture-mode-map)

(add-hook 'ekg-capture-mode-hook #'ekg-llm-minor-mode)
(add-hook 'ekg-edit-mode-hook #'ekg-llm-minor-mode)

(defun ekg-llm-note-interactions ()
  "From an ekg note buffer, create the prompt for the LLM.
The return value is a list of `ekg-llm-prompt-interaction'
structs."
  (list
   (make-llm-chat-prompt-interaction
    :role 'user
    :content (substring-no-properties (ekg-edit-note-display-text)))))

(defun ekg-llm--note-id-less-p (a b)
  "Return non-nil when note A's ID should sort before note B's ID."
  (string< (format "%s" (ekg-note-id a))
           (format "%s" (ekg-note-id b))))

(defun ekg-llm--hierarchy-entry (note relationship depth &optional omit-text)
  "Return a hierarchy entry for NOTE.
RELATIONSHIP names its relationship to the focal note and DEPTH is
its signed depth relative to that note.  When OMIT-TEXT is non-nil,
omit the note text because it is supplied elsewhere in the prompt."
  (let ((note-data (ekg-llm--note-to-alist note)))
    (when omit-text
      (setq note-data (assq-delete-all 'text note-data)))
    `((relationship . ,relationship)
      (depth . ,depth)
      (note_id . ,(ekg-note-id note))
      (parent_id . ,(ekg-note-parent-id note))
      (note . ,note-data))))

(defun ekg-llm--hierarchy-subtree (note depth visited &optional current)
  "Return NOTE and its descendants as a hierarchy tree.
DEPTH is NOTE's depth relative to the focal note.  VISITED is used
to detect corrupt cycles.  When CURRENT is non-nil, mark NOTE as
the focal note and omit its text, which is sent as the user message."
  (let ((id (ekg-note-id note)))
    (when (gethash id visited)
      (error "Cycle in LLM note hierarchy involving ID %s" id))
    (puthash id t visited)
    (append
     (ekg-llm--hierarchy-entry
      note (if current "current" "descendant") depth current)
     (list
      (cons
       'children
       (vconcat
        (mapcar
         (lambda (child)
           (ekg-llm--hierarchy-subtree child (1+ depth) visited))
         (sort (ekg-note-child-notes note t)
               #'ekg-llm--note-id-less-p))))))))

(defun ekg-llm-note-hierarchy-context (note)
  "Return NOTE's ancestor chain and descendant tree as JSON.
Ancestors have negative depths, NOTE has depth zero, and descendants
have positive depths.  The current note's text is omitted because it
is supplied separately as the user interaction."
  (let* ((ancestors (ekg-note-ancestors note))
         (ancestor-count (length ancestors))
         (visited (make-hash-table :test #'equal))
         (json-encoding-pretty-print t))
    (json-encode
     `((format . "ekg-note-hierarchy-v1")
       (ancestor_chain
        . ,(vconcat
            (cl-loop for ancestor in ancestors
                     for depth from (- ancestor-count)
                     collect (ekg-llm--hierarchy-entry
                              ancestor "ancestor" depth))))
       (current_note
        . ,(ekg-llm--hierarchy-subtree note 0 visited t))))))

(defun ekg-llm-make-similar-text-generator (text)
  "Return a generator for similar notes to TEXT."
  (iter-lambda ()
    (let ((similar-notes (ekg-embedding-n-most-similar-notes
                          (llm-embedding ekg-embedding-provider
                                         (substring-no-properties text))
                          1000)))
      (dolist (id similar-notes)
        (let ((note (ekg-get-note-with-id id)))
          (when (and note
                     (ekg-note-is-content-p note)
                     (not (member ekg-llm-prompt-tag (ekg-note-tags note))))
            (iter-yield (ekg-llm-note-to-text note))))))))

(defun ekg-llm-make-similar-note-generator (note)
  "Return a generator for similar notes to NOTE."
  (ekg-llm-make-similar-text-generator (ekg-display-note-text note)))

(defun ekg-llm-format-time (time)
  "Return a string representation of TIME in a format suitable for LLMs."
  (format-time-string "%Y-%m-%dT%H:%M:%S" time))

(defun ekg-llm-display-note-text (note &optional numwords)
  "Return text of NOTE for LLM consumption, with LLM-specific truncation.
This is similar to `ekg-display-note-text' but uses [truncated]
instead of … for clearer LLM understanding.

NUMWORDS specifies the maximum number of words to include."
  (with-temp-buffer
    (when (ekg-note-text note)
      (insert (ekg-insert-inlines-results
               (ekg-note-text note)
               (ekg-note-inlines note)
               note)))
    ;; Don't apply mode formatting for LLM output (like plaintext format)
    (mapc #'funcall ekg-format-funcs)
    (let ((text (concat (string-trim-right (ekg-truncate-at
                                            (buffer-string)
                                            (or numwords ekg-note-inline-max-words)
                                            " [truncated]"))
                        "\n")))
      text)))

(defun ekg-llm--note-to-alist (note)
  "Return NOTE as an alist suitable for JSON encoding."
  (let ((result `((tags . ,(ekg-note-tags note))
                  (created . ,(ekg-llm-format-time (ekg-note-creation-time note)))
                  (modified . ,(ekg-llm-format-time (ekg-note-modified-time note)))
                  (text . ,(substring-no-properties
                            (ekg-llm-display-note-text
                             note ekg-llm-note-numwords))))))
    (when (ekg-should-show-id-p note)
      (push (cons "id" (ekg-note-id note)) result))
    (when (ekg-note-mode note)
      (push (cons "mode" (symbol-name (ekg-note-mode note))) result))
    (map-do
     (lambda (prop value)
       (when-let ((label (ekg-property-name-for prop)))
         (unless (member prop ekg-llm-ignored-props-for-json)
           (push (cons (downcase label) value) result))))
     (ekg-note-properties note))
    ;; Sort the result so JSON is deterministic and we can test it.
    (sort result (lambda (a b) (string< (car a) (car b))))
    result))

(defun ekg-llm-note-to-text (note)
  "Return a representation of NOTE in an LLM-friendly format."
  (let ((json-encoding-pretty-print t))
    (json-encode (ekg-llm--note-to-alist note))))

(defun ekg-llm-make-any-tag-generator (tags except-id)
  "Return a generator for notes with any of TAGS, not include EXCEPT-ID."
  (iter-lambda ()
    (dolist (note (ekg-get-notes-with-any-tags tags))
      (when (not (equal except-id (ekg-note-id note)))
        (iter-yield (ekg-llm-note-to-text note))))))

(defun ekg-llm-save-response (parent text &optional tags)
  "Save TEXT as an LLM-generated Markdown response to PARENT.
TAGS are additional tags to apply.  Return the newly created
response note."
  (let ((note (ekg-note-create
               :text text
               :mode 'markdown-mode
               :tags (seq-uniq
                      (append
                       (ekg-response-inherited-tags parent)
                       tags
                       (when (and ekg-llm-generated-tag
                                  (not (string-empty-p
                                        ekg-llm-generated-tag)))
                         (list ekg-llm-generated-tag)))))))
    (ekg-note-set-parent note parent)
    ;; `ekg-save-note' updates the modified state of the current buffer;
    ;; isolate that side effect from the note buffer that initiated the call.
    (with-temp-buffer
      (ekg-save-note note))
    (ekg--refresh-notes-buffers)
    note))

(defun ekg-llm--pending-response-string (text)
  "Return overlay contents showing partial LLM response TEXT."
  (concat
   (propertize "\nLLM response (generating)\n"
               'face 'ekg-hierarchy-heading)
   text))

(defun ekg-llm-send-and-use (instructions &optional temperature provider)
  "Run the LLM and save its output as a response note.
TEMPERATURE is a float between 0 and 1 controlling creativity.
INSTRUCTIONS tells the LLM what to generate.  PROVIDER overrides
the configured default provider."
  (let* ((provider (or provider (ekg-llm--provider)))
         (parent (copy-ekg-note ekg-note))
         (context-tags (ekg-llm--context-tags parent))
         (prompt (make-llm-chat-prompt
                  :temperature temperature
                  :context (concat (ekg-llm-prompt-prelude) "\n"
                                   (llm-prompt-fill
                                    'ekg-llm-fill-prompt
                                    provider
                                    :instructions instructions
                                    :tags (mapconcat #'identity context-tags ", ")
                                    :tag-notes (ekg-llm-make-any-tag-generator
                                                context-tags
                                                (ekg-note-id parent))
                                    :similar-notes (ekg-llm-make-similar-note-generator parent)
                                    :note-hierarchy
                                    (ekg-llm-note-hierarchy-context parent)))
                  :interactions (ekg-llm-note-interactions)))
         (origin-buffer (current-buffer))
         (pending-overlay (make-overlay (point-max) (point-max)
                                        (current-buffer) nil t)))
    (overlay-put pending-overlay 'priority 200)
    (overlay-put pending-overlay 'after-string
                 (ekg-llm--pending-response-string ""))
    (condition-case err
        (llm-chat-streaming
         provider prompt
         (lambda (text)
           (when (overlay-buffer pending-overlay)
             (overlay-put pending-overlay 'after-string
                          (ekg-llm--pending-response-string text))))
         (lambda (text)
           (delete-overlay pending-overlay)
           (let ((response (ekg-llm-save-response parent text)))
             (when (buffer-live-p origin-buffer)
               (with-current-buffer origin-buffer
                 (ekg--refresh-hierarchy-overlays-in-current-buffer)))
             (message "Saved LLM response note %s"
                      (ekg-note-id response))))
         (lambda (_type msg)
           (delete-overlay pending-overlay)
           (message "Could not call LLM: %s" msg)))
      (not-implemented
       (delete-overlay pending-overlay)
       (message "Streaming not supported; waiting for a synchronous response")
       (let ((response (ekg-llm-save-response parent
                                              (llm-chat provider prompt))))
         (message "Saved LLM response note %s" (ekg-note-id response))))
      (error
       (delete-overlay pending-overlay)
       (signal (car err) (cdr err))))))

(defun ekg-llm-note-metadata-for-input (note)
  "Return a brief description of the metadata of NOTE.
The description is appropriate for input to a LLM.  This is
designed to be on a line of its own.  It does not return a
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

(defun ekg-llm--provider ()
  "Return the provider for the LLM."
  (if (listp ekg-llm-provider)
      (car ekg-llm-provider)
    ekg-llm-provider))

(defun ekg-llm-query-with-notes (query)
  "Query the LLM with QUERY, including relevant notes in the prompt.
The answer will appear in a new buffer"
  (interactive "sQuery: ")
  (let ((buf (get-buffer-create
              (format "*ekg llm query '%s'*" (ekg-truncate-at query 5)))))
    (with-current-buffer buf
      (erase-buffer)
      (let ((prompt (llm-make-chat-prompt
                     query
                     :context
                     (llm-prompt-fill 'ekg-llm-note-query-prompt
                                      (ekg-llm--provider)
                                      :notes (ekg-llm-make-similar-text-generator query)))))
        (condition-case nil
            (llm-chat-streaming (ekg-llm--provider)
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
          (not-implemented (llm-chat (ekg-llm--provider) prompt)))))
    (pop-to-buffer buf)))

(provide 'ekg-llm)

;;; ekg-llm.el ends here
