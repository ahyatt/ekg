;;; ekg-embeddings.el --- Create and use embeddings for ekg -*- lexical-binding: t -*-

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
;; This is a module for creating, storing, and using embeddings in ekg.  The
;; embeddings provide the capability of understanding note and tag similarity,
;; as well as searching via embedding.
;;
;; It is highly recommended that you byte-compile this, or, better yet,
;; native-compile this, due to the amount of calculations that happen.

(require 'ekg)
(require 'llm)

;;; Code:

(defgroup ekg-embedding nil
  "Embedding-based functionality for ekg."
  :group 'ekg)

(defcustom ekg-generate-all-buffer "*ekg embedding generation*"
  "Buffer name used for messages related to generating embeddings."
  :type 'string
  :group 'ekg-embedding)

(defcustom ekg-embedding-text-selector #'ekg-embedding-text-selector-initial
  "Function to select the text of the embedding.

This is necessary because there are usually token limits in the
API calls.  The function will be passed the full text and will
return the text to pass to the embedding API."
  :type '(function)
  :group 'ekg-embedding)

(defcustom ekg-embedding-batch-size 100
  "The number of embeddings to generate in a single batch."
  :type 'integer
  :group 'ekg-embedding)

(defconst ekg-embedding-provider nil
  "The provider of the embedding.
This is a struct representing a provider in the `llm' package.")

(defun ekg-embedding-connect ()
  "Ensure the database is connected and ekg-embedding schema exists."
  (ekg-connect)
  (ekg-embedding-add-schema))

(defun ekg-embedding-add-schema ()
  "Add the triples schema for storing embeddings."
  (triples-add-schema ekg-db 'embedding '(embedding :base/unique t :base/type vector)))

(defun ekg-embedding-average (embeddings)
  "Compute the average of all of EMBEDDINGS, a list.
Return the vector embedding.  This assumes all embeddings are the
same size.  There must be at least one embedding passed in."
  (let* ((v (make-vector (length (car embeddings)) 0)))
    (cl-loop for e in (seq-filter (lambda (e) (= (length e) (length v))) embeddings) do
             (cl-loop for i below (length e) do
                      (aset v i (+ (aref v i) (aref e i)))))
    (cl-loop for i below (length v) do
             (aset v i (/ (aref v i) (length embeddings))))
    v))

(defun ekg-embedding-generate-for-note-async (note &optional success-callback error-callback)
  "Calculate and set the embedding for NOTE.
The embedding is calculated asynchronously and the data is
updated afterwards.

If SUCCESS-CALLBACK is non-nil, call it after setting the value,
with NOTE as the argument.

If ERROR-CALLBACK is non-nil use it on error, otherwise log a message."
  (ekg-embedding-connect)
  (llm-embedding-async
   ekg-embedding-provider
   (funcall ekg-embedding-text-selector
            (substring-no-properties
             (ekg-display-note-text note)))
   (lambda (embedding)
     (ekg-connect)
     (triples-set-type ekg-db (ekg-note-id note) 'embedding
                       :embedding embedding)
     (when success-callback (funcall success-callback note)))
   (or error-callback
       (lambda (error-type msg)
         (message "ekg-embedding: error %s: %s" error-type msg)))))

(defun ekg-embedding-generate-for-note-sync (note)
  "Calculate and set the embedding for NOTE.
The embedding is calculated synchronously, and the caller will
wait for the embedding to return and be set."
  (ekg-embedding-connect)
  (let ((embedding (llm-embedding
                    ekg-embedding-provider
                    (funcall ekg-embedding-text-selector
                             (substring-no-properties
                              (ekg-display-note-text note))))))
    (if (ekg-embedding-valid-p embedding)
        (triples-set-type ekg-db (ekg-note-id note) 'embedding
                          :embedding embedding)
      (lwarn 'ekg :error "Invalid and unusable embedding generated from llm-embedding of note %s: %S"
             (ekg-note-id note) embedding))))

(defun ekg-embedding-generate-batch-async (notes success-callback error-callback)
  "Generate embeddings for NOTES in a batch.
SUCCESS-CALLBACK is called with the size of the batch after the batch is
finished.
ERROR-CALLBACK is called with error-type and message on errors."
  (let ((texts (mapcar (lambda (note)
                         (funcall ekg-embedding-text-selector
                                  (substring-no-properties
                                   (ekg-display-note-text note))))
                       notes)))
    (llm-batch-embeddings-async
     ekg-embedding-provider
     texts
     (lambda (embeddings)
       (ekg-connect)
       (cl-loop for note in notes
                for embedding in embeddings
                do (triples-set-type ekg-db (ekg-note-id note) 'embedding
                                     :embedding embedding)
                finally
                (when success-callback (funcall success-callback (length notes)))))
     error-callback)))

(defun ekg-embedding-generate-for-note-tags-delayed (note)
  "Run `ekg-embedding-generate-for-note-tags' after a delay.

The delay is necessary when notes have just been saved, because
they may not have an embedding yet.

NOTE is the note to create an embedding for."
  (run-with-idle-timer (* 60 5) nil
                       (lambda ()
                         (ekg-embedding-generate-for-note-tags note))))

(defun ekg-embedding-generate-for-note-tags (note)
  "Calculate and set the embedding for all the tags of NOTE."
  (ekg-embedding-connect)
  (cl-loop for tag in (ekg-note-tags note) do
           (ekg-embedding-refresh-tag-embedding tag)))

(defun ekg-embedding-note-get (note)
  "Get the already store embedding for NOTE."
  (plist-get (ekg-note-properties note) :embedding/embedding))

(defun ekg-embedding-valid-p (embedding)
  "Return non-nil if EMBEDDING is valid."
  ;; If there's a 0, it can't be a valid embedding - we assume we have to have a
  ;; non-zero value on every dimension of the embedding. This is likely true,
  ;; but more likely 0s tend to indicate issues with how the embedding was
  ;; obtained.
  (and (vectorp embedding) (> (length embedding) 0)
       (not (seq-contains-p embedding 0))))

(defun ekg-embedding-refresh-tag-embedding (tag)
  "Refresh the embedding for TAG.
The embedding for TAG is recomputed by averaging all the
embeddings of notes with the given tag."
  (condition-case err
      (let ((embeddings
             (cl-loop for tagged in
                      (plist-get (triples-get-type ekg-db tag 'tag) :tagged)
                      collect
                      (let ((embedding (plist-get (triples-get-type ekg-db tagged 'embedding)
                                                  :embedding)))
                        (unless (ekg-embedding-valid-p embedding)
                          (message "ekg-embedding: invalid embedding for note %s, attempting to fix" tagged)
                          (let ((note (ekg-get-note-with-id tagged)))
                            (ekg-embedding-generate-for-note-sync note)
                            (if (ekg-embedding-valid-p note)
                                (progn
                                  (ekg-save-note note)
                                  (setf embedding (ekg-embedding-note-get note)))
                              (error "In ekg-embedding: could not fix invalid embedding for note %s, can't refresh tag %s" tagged tag))))
                        embedding))))
        (let ((avg (ekg-embedding-average
                    (seq-filter #'ekg-embedding-valid-p embeddings))))
          (if (ekg-embedding-valid-p avg)
              (triples-set-type ekg-db tag 'embedding :embedding avg)
            (message "ekg-embedding: could not compute average embedding for tag %s" tag))))
    (error (message "ekg-embedding: error when trying to refresh tag %s: %S" tag err))))

(defun ekg-embedding-generate-all (&optional arg)
  "Generate and store embeddings for every entity that needs one.
It is not necessary for the entity to contain a note.  Tags will
be calculated from the average of all tagged entities.
Embeddings will not be calculated for objects with no text,
except for tags.  If called with prefix ARG, embeddings will be
generated even if embeddings already exist.  This is a fairly
slow function, and may take minutes or hours depending on how
much data there is.

Everything here is done asynchronously.  A message will be
printed when everything is finished."
  (interactive "P")
  (ekg-embedding-connect)
  (let* ((count 0)
         (to-generate
          (seq-filter (if arg #'identity (lambda (id)
                                           (not (ekg-embedding-valid-p
                                                 (plist-get
                                                  (triples-get-type ekg-db id 'embedding) :embedding)))))
                      (ekg-active-note-ids)))
         (notes-to-generate
          (seq-filter (lambda (note) (> (length (ekg-note-text note)) 0))
                      (mapcar #'ekg-get-note-with-id to-generate))))
    (cl-labels ((complete-id (num)
                  (cl-incf count num)
                  (with-current-buffer (get-buffer-create ekg-generate-all-buffer)
                    (goto-char (point-max))
                    (insert (format "Generated %d/%d (%.0f%% done)\n"
                                    count (length to-generate)
                                    (/ (* count 100.0) (length to-generate)))))))
      (with-current-buffer (get-buffer-create ekg-generate-all-buffer)
        (goto-char (point-max))
        (insert (format "\nGenerating %d embeddings\n" (length to-generate)))
        (display-buffer (current-buffer))

        ;; Process notes in batches
        (let ((batches (seq-partition notes-to-generate ekg-embedding-batch-size)))
          (cl-loop for batch in batches do
                   (ekg-embedding-generate-batch-async
                    batch
                    #'complete-id
                    (lambda (error-type msg)
                      (insert (format "Could not generate embedding batch: %s %s\n"
                                      error-type msg))))
                   ;; Add a small delay between batches
                   (sit-for 1)))

        ;; Process empty notes
        (cl-loop for id in to-generate
                 when (= (length (ekg-note-text (ekg-get-note-with-id id))) 0)
                 collect id into ids
                 finally
                 do (complete-id (length ids)))

        ;; At this point, async things are happening, wait for idle
        (run-with-idle-timer (* 60 5) nil
                             (lambda ()
                               (let ((tags (ekg-tags)))
                                 (cl-loop for s in tags do
                                          (ekg-embedding-refresh-tag-embedding s))
                                 (with-current-buffer (get-buffer-create ekg-generate-all-buffer)
                                   (insert (format "Refreshed %d tags\n" (length tags)))))))))))

(defun ekg-embedding-cosine-similarity (v1 v2)
  "Calculate the cosine similarity of V1 and V2.
The return is a floating point number between 0 and 1, where the
closer it is to 1, the more similar it is."
  (let ((dot-product (ekg-embedding-dot-product v1 v2))
        (v1-magnitude (ekg-embedding-magnitude v1))
        (v2-magnitude (ekg-embedding-magnitude v2)))
    (if (and v1-magnitude v2-magnitude)
        (/ dot-product (* v1-magnitude v2-magnitude))
      0)))

(defun ekg-embedding-dot-product (v1 v2)
  "Calculate the dot produce of vectors V1 and V2."
  (let ((result 0))
    (dotimes (i (length v1))
      (setq result (+ result (* (aref v1 i) (aref v2 i)))))
    result))

(defun ekg-embedding-magnitude (v)
  "Calculate magnitude of vector V."
  (let ((sum 0))
    (dotimes (i (length v))
      (setq sum (+ sum (* (aref v i) (aref v i)))))
    (sqrt sum)))

(defun ekg-embedding-text-selector-initial (text)
  "Return the TEXT to use for generating embeddings.
This is shortened to abide by token limits, using a conservative
approach, since it is difficult to predict the number of tokens
exactly."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; The target number of words we want is 8191 (the open AI limit is 8192),
    ;; divided by a factor of 1.5 to be conservative, since one word can be
    ;; multiple tokens.
    (let ((target-num-words (floor (/ 8191 1.5)))
          (num-words 0))
      (while (and (< num-words target-num-words)
                  (not (eobp)))
        (forward-word)
        (cl-incf num-words))
      (buffer-substring-no-properties (point-min) (point)))))

(defun ekg-embedding-delete (id)
  "Delete embedding for ID."
  (ekg-embedding-connect)
  (triples-remove-type ekg-db id 'embedding))

(defun ekg-embedding-get (id)
  "Return the embedding of entity with ID.
If there is no embedding, return nil."
  (plist-get (triples-get-type ekg-db id 'embedding) :embedding))

(defun ekg-embedding-get-all-notes ()
  "Return an alist of id to embedding.
IDs that do not have embeddings will not be in the list."
  (seq-filter #'cdr (cl-loop for s in (ekg-active-note-ids)
                             collect (cons s (ekg-embedding-get s)))))

(defun ekg-embedding-n-most-similar-to-id (id n)
  "From an ID, return a list of the N most similar ids.
The results are in order of most similar to least similar."
  (let ((embedding (ekg-embedding-get id)))
    (unless embedding (error "Unable to find embedding of note %S" id))
    (ekg-embedding-n-most-similar-notes embedding n)))

(defun ekg-embedding-n-most-similar-notes (e n)
  "From an embedding E, return a list of the N most similar ids.
The results are in order of most similar to least similar."
  (let* ((embeddings (ekg-embedding-get-all-notes)))
    (setq embeddings
          (sort
           (mapcar (lambda (id-embedding)
                     (cons (car id-embedding)
                           (ekg-embedding-cosine-similarity e (cdr id-embedding))))
                   embeddings)
           (lambda (a b) (> (cdr a) (cdr b)))))
    (mapcar #'car (cl-subseq embeddings 0 (min n (length embeddings))))))

(defun ekg-embedding-show-similar ()
  "Show similar notes to the current note in a new buffer."
  (interactive nil ekg-notes-mode)
  (ekg-embedding-connect)
  (let ((note (ekg-current-note-or-error)))
    (ekg-setup-notes-buffer
     (format "similar to note \"%s\"" (ekg-note-snippet note))
     (lambda () (mapcar #'ekg-get-note-with-id
                        ;; remove the first match, since the current note will
                        ;; always be the most similar.
                        (cdr (ekg-embedding-n-most-similar-to-id (ekg-note-id note) ekg-notes-size))))
     (ekg-note-tags note))))

(defun ekg-embedding-search (&optional text)
  "Show similar notes to the TEXT in a new buffer."
  (interactive "MSearch: ")
  (ekg-embedding-connect)
  (ekg-setup-notes-buffer
   (format "similar to \"%s\"" text)
   (lambda () (mapcar #'ekg-get-note-with-id (ekg-embedding-n-most-similar-notes
                                              (llm-embedding ekg-embedding-provider text)
                                              ekg-notes-size)))
   nil))

(defun ekg-embedding-show-similar-to-current-buffer ()
  "Show similar notes to the text in the current buffer."
  (interactive)
  (ekg-embedding-connect)
  (ekg-setup-notes-buffer
   (format "similar to buffer \"%s\"" (buffer-name (current-buffer)))
   (lambda () (mapcar #'ekg-get-note-with-id
                      (ekg-embedding-n-most-similar-notes
                       (llm-embedding ekg-embedding-provider
                                      (funcall ekg-embedding-text-selector
                                               (substring-no-properties (buffer-string))))
                       ekg-notes-size)))
   nil))

(defun ekg-embedding-generate-on-save ()
  "Enable embedding generation for new notes.
If you have created notes without embeddings enabled, you should
run `ekg-embedding-generate-all' to generate embeddings for all
notes."
  (add-hook 'ekg-note-pre-save-hook #'ekg-embedding-generate-for-note-async)
  ;; Generating embeddings from a note's tags has to be post-save, since it works
  ;; by loading saved embeddings.
  (add-hook 'ekg-note-save-hook #'ekg-embedding-generate-for-note-tags-delayed))

(defun ekg-embedding-disable-generate-on-save ()
  "Disable the embedding module for the Emacs session."
  (remove-hook 'ekg-note-pre-save-hook #'ekg-embedding-generate-for-note-async)
  (remove-hook 'ekg-note-save-hook #'ekg-embedding-generate-for-note-tags-delayed))

;; Regardless of whether notes are generated on save, when notes are deleted we
;; need to clean up the embeddings.
(add-hook 'ekg-note-delete-hook #'ekg-embedding-delete)

(provide 'ekg-embedding)

;;; ekg-embedding.el ends here
