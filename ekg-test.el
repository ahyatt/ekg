;;; ekg-test.el --- Tests for ekg  -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2023  Andrew Hyatt <ahyatt@gmail.com>

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
  (declare (debug t) (indent defun))
  `(ert-deftest ,name ()
     (let ((ekg-db-file (make-temp-file "ekg-test"))
           (ekg-db nil)
           (orig-buffers (buffer-list))
           ;; Remove hooks
           (ekg-add-schema-hook nil)
           (ekg-note-pre-save-hook nil)
           (ekg-note-save-hook nil)
           (ekg-note-pre-delete-hook nil)
           (ekg-note-delete-hook nil)
           (ekg-note-add-tag-hook nil))
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
    ;; WHen we get notes with tags, it's an AND, so it shouldn't get anything.
    (should (= 0 (length (ekg-get-notes-with-tags '("tag1" "tag2" "nonexistent")))))
    (should (equal note (car (ekg-get-notes-with-tags '("tag1")))))
    (should (ekg-has-live-tags-p (ekg-note-id note)))
    (ekg-note-trash note)
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
  (ekg-save-note (ekg-note-create "" 'text-mode '("a" "b")))
  (ekg-show-notes-with-any-tags '("a" "b"))
  (let* ((tag-buf (get-buffer "*ekg tags (any): a b*")))
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
  ;; Re-capture, should edit the existing note.
  (ekg-capture-url "http://testurl" "A URL used for testing")
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
    (when (search-backward "http://testurl")
      (replace-match "http://testurl/v2"))
    (ert-simulate-command '(ekg-capture-finalize)))
  (should (equal (ekg-document-titles) (list (cons "http://testurl/v2" "A URL used for testing")))))

(ekg-deftest ekg-test-sort-nondestructive ()
  (mapc #'ekg-save-note
      (list (ekg-note-create "a" ekg-capture-default-mode '("tag/a"))
            (ekg-note-create "b" ekg-capture-default-mode '("tag/b"))))
  (ekg-show-notes-with-any-tags '("tag/b" "tag/a"))
  (should (string= (car (ewoc-get-hf ekg-notes-ewoc)) "tags (any): tag/a tag/b")))

(ekg-deftest ekg-test-note-roundtrip ()
  (let ((text "foo\n\tbar \"baz\" ☃"))
    (ekg-save-note (ekg-note-create text #'text-mode '("test")))
    (let ((note (car (ekg-get-notes-with-tag "test"))))
      (should (ekg-note-id note))
      (should (equal text (ekg-note-text note)))
      (should (equal 'text-mode (ekg-note-mode note))))))

(ekg-deftest ekg-test-templating ()
  (ekg-save-note (ekg-note-create "ABC" #'text-mode '("test" "template")))
  (ekg-save-note (ekg-note-create "DEF" #'text-mode '("test" "template")))
  (let ((ekg-note-add-tag-hook '(ekg-on-add-tag-insert-template)))
    (ekg-capture '("test"))
    (let ((text (substring-no-properties (buffer-string))))
      (should (string-match (rx (literal "ABC")) text))
      (should (string-match (rx (literal "DEF")) text)))))

(ekg-deftest ekg-test-get-notes-with-tags ()
  (ekg-save-note (ekg-note-create "ABC" #'text-mode '("foo" "bar")))
  (should-not (ekg-get-notes-with-tags '("foo" "none")))
  (should-not (ekg-get-notes-with-tags '("none" "foo")))
  (should (= (length (ekg-get-notes-with-tags '("bar" "foo"))) 1)))

(ekg-deftest ekg-test-overlay-interaction ()
  (let ((ekg-capture-auto-tag-funcs nil))
    (ekg-capture '("test"))
    (let ((o (ekg--metadata-overlay)))
      (should (= (overlay-start o) 1))
      ;; The overlay end is the character just past the end of the visible
      ;; overlay, but still in the overlay's extent.
      (should (= (overlay-end o) (+ 1 (length "Tags: test\n"))))
      ;; Go to the end of the overlay, insert, the overlay should grow
      (goto-char (overlay-end o))
      (insert "Property: ")
      (ekg--metadata-modification o t nil nil)
      (should (= (overlay-end o) (+ 1 (length "Tags: test\nProperty: "))))
      ;; Insert a new line, the overlay should grow again
      (insert "\n")
      (ekg--metadata-modification o t nil nil)
      (should (= (overlay-end o) (+ 1 (length "Tags: test\nProperty: \n"))))
      ;; But there shouldn't be two newlines in a row in the overlay, so the
      ;; overlay should not grow.
      (insert "\n")
      (ekg--metadata-modification o t nil nil)
      (should (= (overlay-end o) (+ 1 (length "Tags: test\nProperty: \n")))))))

(provide 'ekg-test)

;;; ekg-test.el ends here
