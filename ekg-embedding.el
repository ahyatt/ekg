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
;; This is a module for creating, storing, and using embeddings in ekg. The
;; embeddings provide the capability of understanding note and tag similarity,
;; as well as searching via embedding.
;;
;; It is highly recommended that you byte-compile this, or, better yet,
;; native-compile this, due to the amount of calculations that happen.

(require 'ekg)
(require 'llm)

;;; Code:

(defcustom ekg-embedding-text-selector #'ekg-embedding-text-selector-initial
  "Function to select the text of the embedding, which is necessary
because there are usually token limits in the API calls. The
function will be passed the full text and will return the text to
pass to the embedding API."
  :type '(function)
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
Return the vector embedding. This assumes all embeddings are the
same size.  There must be at least one embedding passed in."
(let* ((v (make-vector (length (car embeddings)) 0)))
    (cl-loop for e in (seq-filter (lambda (e) (= (length e) (length v))) embeddings) do
             (cl-loop for i below (length e) do
                      (aset v i (+ (aref v i) (aref e i)))))
    (cl-loop for i below (length v) do
             (aset v i (/ (aref v i) (length embeddings))))
    v))

(defun ekg-embedding-generate-for-note (note)
  "Calculate and set the embedding for NOTE.
The embedding is calculated asynchronously and the data is
updated afterwards."
  (llm-embedding-async
   ekg-embedding-provider
   (funcall ekg-embedding-text-selector
            (substring-no-properties
             (ekg-display-note-text note)))
   (lambda (embedding)
     (ekg-connect)
     (triples-set-type ekg-db (ekg-note-id note) 'embedding
                       :embedding embedding))
   (lambda (error-type msg)
     (message "ekg-embedding: error %s: %s" error-type msg))))

(defun ekg-embedding-generate-for-note-tags (note)
  "Calculate and set the embedding for all the tags of NOTE."
  (cl-loop for tag in (ekg-note-tags note) do
           (ekg-embedding-refresh-tag-embedding tag)))

(defun ekg-embedding-note-get (note)
  "Get the already store embedding for NOTE."
  (plist-get (ekg-note-properties note) :embedding/embedding))

(defun ekg-embedding-valid-p (embedding)
  "Return non-nil if the embedding is valid."
  ;; If there's a 0, it can't be a valid embedding - we assume we have to have a
  ;; non-zero value on every dimension of the embedding. This is likely true,
  ;; but more likely 0s tend to indicate issues with how the embedding was
  ;; obtained.
  (and (vectorp embedding) (> (length embedding) 0)
       (not (seq-contains-p (lambda (e) (= 0 e)) embedding))))

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
                        (ekg-embedding-generate-for-note note)
                        (if (ekg-embedding-valid-p note)
                            (progn
                              (ekg-save-note note)
                              (setf embedding (ekg-embedding-note-get note)))
                          (error "ekg-embedding: could not fix invalid embedding for note %s, can't refresh tag %s" tagged tag))))
                    embedding))))
    (let ((avg (ekg-embedding-average
                (seq-filter #'ekg-embedding-valid-p embeddings))))
      (if (ekg-embedding-valid-p avg)
          (triples-set-type ekg-db tag 'embedding :embedding avg)
        (message "ekg-embedding: could not compute average embedding for tag %s" tag))))
    (error (message "ekg-embedding: error when trying not refresh tag %s: %S" tag err))))

(defun ekg-embedding-generate-all (arg)
  "Generate and store embeddings for every entity that needs one.
It is not necessary for the entity to contain a note. Tags will
be calculated from the average of all tagged entities. Embeddings
will not be calculated for objects with no text, except for tags.
If called with a prefix arg, embeddings will be generated even if
embeddings already exist. This is a fairly slow function, and may
take minutes or hours depending on how much data there is.."
  (interactive "P")
  (ekg-embedding-connect)
  (let ((count 0))
       (cl-loop for s in (ekg-active-note-ids) do
                (thread-yield)
                (let ((note (ekg-get-note-with-id s))
                      (embedding (triples-get-type ekg-db s 'embedding)))
                  (when (and (or arg (not (ekg-embedding-valid-p embedding)))
                             (> (length (ekg-note-text note)) 0))
                    (cl-incf count)
                    (triples-set-type ekg-db s 'embedding :embedding (ekg-embedding
                                                                      (substring-no-properties (ekg-note-text note)))))))
       (cl-loop for s in (triples-subjects-of-type ekg-db 'tag) do
                (ekg-embedding-refresh-tag-embedding s))
       (triples-backups-maybe-backup ekg-db (ekg-db-file))
       (message "Generated %s embeddings" count)))

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
  "Show similar notes to the current note in a new buffer."
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
     (lambda () (mapcar #'ekg-get-note-with-id (ekg-embedding-n-most-similar-notes
                                                (ekg-embedding (buffer-string)) ekg-notes-size)))
     nil))

(add-hook 'ekg-note-pre-save-hook #'ekg-embedding-generate-for-note)
;; Generating embeddings from a note's tags has to be post-save, since it works
;; by loading saved embeddings.
(add-hook 'ekg-note-save-hook #'ekg-embedding-generate-for-note-tags)
(add-hook 'ekg-note-delete-hook #'ekg-embedding-delete)

(provide 'ekg-embedding)

;;; ekg-embedding.el ends 
