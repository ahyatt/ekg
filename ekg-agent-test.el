;;; ekg-agent-test.el --- Tests for ekg-agent  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Andrew Hyatt <ahyatt@gmail.com>

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

;; Tests for ekg-agent functionality.

;;; Code:

(require 'ert)
(require 'ekg-agent)

;; Indentation adjustment

(ert-deftest ekg-agent-test-adjust-indentation-strip-extra ()
  "Extra leading whitespace in the replacement is stripped."
  (should (equal "    (defun foo ()\n      (bar))"
                 (ekg-agent--adjust-indentation
                  "        (defun foo ()\n          (bar))" 4))))

(ert-deftest ekg-agent-test-adjust-indentation-no-change ()
  "Replacement already at the correct indentation is unchanged."
  (should (equal "    (defun foo ()\n      (bar))"
                 (ekg-agent--adjust-indentation
                  "    (defun foo ()\n      (bar))" 4))))

(ert-deftest ekg-agent-test-adjust-indentation-add-whitespace ()
  "Whitespace is added when the replacement is under-indented."
  (should (equal "    (defun foo ()\n      (bar))"
                 (ekg-agent--adjust-indentation
                  "  (defun foo ()\n    (bar))" 4))))

(ert-deftest ekg-agent-test-adjust-indentation-empty-lines ()
  "Empty lines in the replacement are preserved as-is."
  (should (equal "    line1\n\n    line3"
                 (ekg-agent--adjust-indentation
                  "        line1\n\n        line3" 4))))

(ert-deftest ekg-agent-test-adjust-indentation-zero-target ()
  "Adjustment to column zero strips all common indentation."
  (should (equal "(foo)\n  (bar)"
                 (ekg-agent--adjust-indentation
                  "    (foo)\n      (bar)" 0))))

(ert-deftest ekg-agent-test-adjust-indentation-single-line ()
  "A single line replacement is adjusted correctly."
  (should (equal "  hello"
                 (ekg-agent--adjust-indentation "      hello" 2))))

;; Error-as-text macro

(ert-deftest ekg-agent-test-error-as-text-on-error ()
  "Errors are caught and returned as a descriptive string."
  (should (equal "Error: something broke"
                 (ekg-agent--with-error-as-text
                   (error "something broke")))))

(ert-deftest ekg-agent-test-error-as-text-on-success ()
  "Successful results pass through unchanged."
  (should (equal 42
                 (ekg-agent--with-error-as-text
                   (+ 40 2)))))

;; Line ID generation

(ert-deftest ekg-agent-test-line-ids-unique ()
  "Line IDs for different lines of the same file are unique."
  (let ((ids (mapcar (lambda (n) (ekg-agent--line-id "/tmp/test.txt" n))
                     (number-sequence 1 100))))
    (should (= (length ids)
               (length (delete-dups (copy-sequence ids)))))))

(ert-deftest ekg-agent-test-line-ids-three-chars ()
  "Every line ID is exactly 3 characters."
  (let ((ids (mapcar (lambda (n) (ekg-agent--line-id "/tmp/test.txt" n))
                     (number-sequence 1 50))))
    (should (cl-every (lambda (id) (= 3 (length id))) ids))))

(ert-deftest ekg-agent-test-line-ids-deterministic ()
  "The same path and line number always produce the same ID."
  (should (equal (ekg-agent--line-id "/tmp/test.txt" 42)
                 (ekg-agent--line-id "/tmp/test.txt" 42))))

;; File read/edit round-trip

(ert-deftest ekg-agent-test-read-file-with-line-ids ()
  "Reading a file returns content prefixed with 3-char identifiers."
  (let ((path (make-temp-file "ekg-agent-test")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "alpha\nbeta\ngamma\n"))
          (let ((result (ekg-agent--read-file path)))
            (should (stringp result))
            (dolist (line (split-string result "\n"))
              (when (not (string-empty-p line))
                (should (string-match "\\`...: " line))))))
      (delete-file path))))

(ert-deftest ekg-agent-test-read-file-from-buffer ()
  "Reading a file that is open in a buffer reflects buffer content."
  (let ((path (make-temp-file "ekg-agent-test")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "disk content\n"))
          (let ((buf (find-file-noselect path)))
            (unwind-protect
                (progn
                  (with-current-buffer buf
                    (erase-buffer)
                    (insert "buffer content\n"))
                  (let ((result (ekg-agent--read-file path)))
                    (should (string-match-p "buffer content" result))))
              (kill-buffer buf))))
      (delete-file path))))

(ert-deftest ekg-agent-test-edit-file-round-trip ()
  "Editing a file replaces the identified region and returns context."
  (let ((path (make-temp-file "ekg-agent-test")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "line one\nline two\nline three\n"))
          (let* ((output (ekg-agent--read-file path))
                 (lines (split-string output "\n"))
                 (id1 (substring (nth 0 lines) 0 3))
                 (id2 (substring (nth 1 lines) 0 3)))
            (ekg-agent--edit-file path id1 "line one" id2 "line two" "replaced")
            (with-temp-buffer
              (insert-file-contents path)
              (should (string-match-p "replaced" (buffer-string)))
              (should-not (string-match-p "line one" (buffer-string)))
              (should-not (string-match-p "line two" (buffer-string)))
              (should (string-match-p "line three" (buffer-string))))))
      (delete-file path))))

(ert-deftest ekg-agent-test-edit-file-adjusts-indentation ()
  "Editing a file corrects over-indented replacement text."
  (let ((path (make-temp-file "ekg-agent-test")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "  hello world\n  goodbye world\n"))
          (let* ((output (ekg-agent--read-file path))
                 (lines (split-string output "\n"))
                 (id1 (substring (nth 0 lines) 0 3)))
            ;; Replace with over-indented text (8 spaces instead of 2)
            (ekg-agent--edit-file path id1 "hello world" id1 "hello world"
                                 "        new content")
            (with-temp-buffer
              (insert-file-contents path)
              ;; Should be adjusted to 2-space indent
              (should (string-match-p "^  new content" (buffer-string))))))
      (delete-file path))))

(provide 'ekg-agent-test)
;;; ekg-agent-test.el ends here
