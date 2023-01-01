;;; ekg-test.el --- Tests for ekg  -*- lexical-binding: t; -*-

;; Copyright (c) 2022  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
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

;; These tests should be run and pass before every commit.

;;; Code:
(require 'ekg)
(require 'ert)
(require 'ert-x)

(defmacro ekg-deftest (name _ &rest body)
  "A test that will set up an empty `ekg-db' for use."
  (declare (debug t) (indent 2))
  `(ert-deftest ,name ()
     (let ((ekg-db-file (make-temp-file "ekg-test"))
           (ekg-db nil)
           (orig-buffers (buffer-list)))
       (ekg--connect)
       (save-excursion
         (unwind-protect 
             (progn ,@body)
           ;; Kill all opened bufferes
           (mapc #'kill-buffer (seq-difference (buffer-list) orig-buffers)))))))

(ekg-deftest ekg-test-note-lifecycle ()
  (let ((note (ekg-note-create "Test text" 'text-mode '("tag1" "tag2"))))
    (ekg-save-note note)
    ;; We should have an ID now.
    (should (ekg-note-id note))
    (should (= 1 (length (ekg-get-notes-with-tags '("tag1")))))
    ;; Just one note, even if it has both of the tags.
    (should (= 1 (length (ekg-get-notes-with-tags '("tag1" "tag2")))))
    ;; WHen we get notes with tags, it's an OR not an AND, so it's OK if one of
    ;; the tags doesn't apply.
    (should (= 1 (length (ekg-get-notes-with-tags '("tag1" "tag2" "nonexistent")))))
    (should (equal note (car (ekg-get-notes-with-tags '("tag1")))))
    (should (ekg-has-live-tags-p (ekg-note-id note)))
    (ekg-note-delete note)
    (should-not (ekg-has-live-tags-p (ekg-note-id note)))
    (should (= 0 (length (ekg-get-notes-with-tags '("tag1" "tag2")))))))

(ekg-deftest ekg-test-tags ()
  (should-not (ekg-tags))
  ;; Make sure we trim and lowercase all tags.
  (ekg-save-note (ekg-note-create "" 'text-mode '(" a" " B ")))
  (should (equal (sort (ekg-tags) #'string<) '("a" "b")))
  (should (equal (ekg-tags-including "b") '("b")))
  (should (string= (ekg-tags-display '("a" "b")) "a b")))

(ekg-deftest ekg-test-org-link-to-id ()
  (require 'ol)
  (let* ((note (ekg-note-create "" 'text-mode '("a" "b")))
         (note-buf (ekg-edit note)))
    (unwind-protect
     (progn
       ;; Can we store a link?
       (with-current-buffer note-buf
         (org-store-link nil 1)
         (should (car org-stored-links)))
       (with-temp-buffer
         ;; Does the link look correct?
         (org-mode)
         (org-insert-last-stored-link nil)
         (should (string= (buffer-string) (format "[[ekg-note:%d][EKG note: %d]]\n" (ekg-note-id note) (ekg-note-id note))))
         ;; Does the link work?
         (goto-char 1)
         (org-open-at-point nil)
         (should (eq note-buf (current-buffer)))))
     (kill-buffer note-buf))))

(ekg-deftest ekg-test-org-link-to-tags ()
  (require 'ol)
  (ekg-note-create "" 'text-mode '("a" "b"))
  (let* ((tag-buf (ekg-show-tags-any '("a" "b"))))
    (unwind-protect
     (progn 
       ;; Can we store a link?
       (with-current-buffer tag-buf
         (org-store-link nil 1)
         (should (car org-stored-links)))
       (with-temp-buffer
         ;; Does the link look correct?
         (org-mode)
         (org-insert-last-stored-link nil)
         (should (string= (buffer-string) "[[ekg-tags-any:(\"a\" \"b\")][EKG page for any of the tags: a, b]]\n"))
         ;; Does the link work?
         (goto-char 1)
         (org-open-at-point nil)
         (should (eq tag-buf (current-buffer)))))
     (kill-buffer tag-buf))))

(ekg-deftest ekg-test-url-handling ()
  (ekg-capture-url "http://testurl" "A URL used for testing")
  (insert "Text added to the URL")
  (ert-simulate-command '(ekg-capture-finalize))
  (should (equal (ekg-document-titles) (list (cons "http://testurl" "A URL used for testing"))))
  (should (member "doc/a url used for testing" (ekg-tags)))
  (ekg-edit (ekg-get-note-with-id "http://testurl"))
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
    (when (search-backward "http://testurl")
      (replace-match "http://testurl/v2"))
    (ert-simulate-command '(ekg-capture-finalize)))
  (should (equal (ekg-document-titles) (list (cons "http://testurl/v2" "A URL used for testing")))))

(provide 'ekg-test)

;;; ekg-test.el ends here
