;;; ekg-embeddings.el --- Creating and using embeddings for ekg -*- lexical-binding: t -*-

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

(require 'request)

;;; Code:

(defcustom ekg-embedding-provider 'openapi
  "The provider of the embedding."
  :type '(choice (const :tag "Open API" 'openapi))
  :group 'ekg-embedding)

(defconst ekg-embedding-api-key nil
  "Key used to access whatever embedding API used.")

(defun ekg-embedding-average (embeddings)
  "Compute the average of all of EMBEDDINGS, a list.
Return the vector embedding. This assumes all embeddings are the
same size.  There must be at least one embedding passed in."
  (let ((v (make-vector (length (car embeddings)) 0)))
    (cl-loop for e in embeddings do
             (cl-loop for i below (length e) do
                      (aset v i (+ (aref v i) (aref e i)))))
    (cl-loop for i below (length v) do
             (aset v i (/ (aref v i) (length embeddings))))
    v))

(defun ekg-embedding-generate-all (arg)
  "Generate and store embeddings for every entity.
It is not necessary for the entity to contain a note. Tags will
be calculated from the average of all tagged entities. Embeddings
will not be calculated for objects with no text, except for tags.
If called with a prefix arg, embeddings will be generated even if
embeddings already exist. This is a fairly slow function, and may
take minutes or hours depending on how much data there is.."
  (interactive "P")
  (ekg--connect)
  (triples-add-schema ekg-db 'embedding '(embedding :base/unique t :base/type vector))
  (cl-loop for s in (ekg-active-note-ids) do
           (let ((note (ekg-get-note-with-id s))
                 (embedding (triples-get-type ekg-db s 'embedding)))
             (when (and (or arg (null embedding))
                        (> (length (ekg-note-text note)) 0))
               (triples-set-type ekg-db s 'embedding :embedding (ekg-embedding
                                                                 (substring-no-properties (ekg-note-text note)))))))
  (cl-loop for s in (triples-subjects-of-type ekg-db 'tag) do
           (let ((embeddings (cl-loop for tagged in
                                      (plist-get (triples-get-type ekg-db s 'tag) :tagged)
                                      collect (plist-get (triples-get-type ekg-db tagged 'embedding)
                                                         :embedding))))
             (triples-set-type ekg-db s 'embedding :embedding (ekg-embedding-average embeddings)))))

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
    (dotimes (i (length v1) result)
      (setq result (+ result (* (aref v1 i) (aref v2 i)))))))

(defun ekg-embedding-magnitude (v)
  "Calculate magnitude of vector V."
  (let ((sum 0))
    (dotimes (i (length v) (sqrt sum))
      (setq sum (+ sum (* (aref v i) (aref v i)))))))

(defun ekg-embedding-openai (text)
  "Get an embedding of TEXT from Open AI API."
  (unless ekg-embedding-api-key
    (error "To call Open AI API, provide the ekg-embedding-api-key"))
  (let ((resp (request "https://api.openai.com/v1/embeddings"
                :type "POST"
                :headers `(("Authorization" . ,(format "Bearer %s" ekg-embedding-api-key))
                           ("Content-Type" . "application/json"))
                :data (json-encode `(("input" . ,text) ("model" . "text-embedding-ada-002")))
                :parser 'json-read
                :error (cl-function (lambda (&key error-thrown data &allow-other-keys)
                                      (error (format "Problem calling Open AI: %s, type: %s message: %s"
                                                     (cdr error-thrown)
                                                     (assoc-default 'type (cdar data))
                                                     (assoc-default 'message (cdar data))))))
                :sync t)))
    (cdr (assoc 'embedding (aref (cdr (assoc 'data (request-response-data resp))) 0)))))

(defun ekg-embedding (text)
  "Get an embedding of TEXT.
Returns the vector representing the embedding."
  (pcase ekg-embedding-provider
    ('openapi (ekg-get-embedding-openai text))))

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
    (unless e (error "Unable to find embedding of note %S" id))
    (ekg-embedding-n-most-similar-notes e n)))

(defun ekg-embedding-n-most-similar-notes (e n)
  "From an embedding E, return a list of the N most similar ids.
The results are in order of most similar to least similar."
  (let* ((embeddings (ekg-embedding-get-all-notes)))
    (setq embeddings
          (cdr (sort
                (mapcar (lambda (id-embedding)
                          (cons (car id-embedding)
                                (ekg-embedding-cosine-similarity e (cdr id-embedding))))
                        embeddings)
                (lambda (a b) (> (cdr a) (cdr b))))))
    (mapcar #'car (cl-subseq embeddings 0 n))))

(defun ekg-embedding-show-similar ()
  "Show similar notes to the current note in a new buffer."
  (interactive nil ekg-notes-mode)
  (let ((note (ekg--current-note-or-error)))
    (ekg-setup-notes-buffer
     (format "similar to note \"%s\"" (ekg-note-snippet note))
     (lambda () (mapcar #'ekg-get-note-with-id (ekg-embedding-n-most-similar-id (ekg-note-id note) 10)))
     (ekg-note-tags note))))

(defun ekg-embedding-search (&optional text)
  "Show similar notes to the current note in a new buffer."
  (interactive "MSearch: ")
  (ekg-setup-notes-buffer
     (format "similar to \"%s\"" text)
     (lambda () (mapcar #'ekg-get-note-with-id (ekg-embedding-n-most-similar-notes
                                                (ekg-embedding text) 10)))
     nil))

(provide 'ekg-embedding)

;;; ekg-embedding.el ends here
