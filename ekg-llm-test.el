;;; ekg-llm-test.el --- Tests for ekg-llm  -*- lexical-binding: t; -*-

;; Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>

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

;; These tests should be run and pass before every commit to ekg-llm.

;;; Code:
(require 'ekg)
(require 'ekg-llm)

(defun ekg-llm-test-replace-text (markers text)
  "Replace text between MARKERS."
  (let ((beg (car markers))
        (end (cdr markers)))
    (goto-char beg)
    (delete-region beg end)
    (insert text)))

(ert-deftest ekg-llm-test-create-output-holder ()
  (with-temp-buffer
    (let ((markers (ekg-llm-create-output-holder "BEGIN" "END")))
      (should (equal "BEGIN\n\nEND\n"
                     (substring-no-properties (buffer-string))))
      (ekg-llm-test-replace-text markers "text 1")
      (should (equal "BEGIN\ntext 1\nEND\n"
                     (substring-no-properties (buffer-string))))
      (ekg-llm-test-replace-text markers "very different text")
      (should (equal "BEGIN\nvery different text\nEND\n"
                     (substring-no-properties (buffer-string)))))))

(ert-deftest ekg-llm-test-note-to-text ()
  (let* ((time (current-time))
         (time-str (format-time-string "%Y-%m-%dT%H:%M:%S" time))
         (json-encoding-pretty-print t))
    (should (equal
             (json-encode
              (sort
               `(("tags" . ["tag1" "tag2"])
                 ("created" . ,time-str)
                 ("modified" . ,time-str)
                 ("title" . ["Title"])
                 ("text" . "Content")
                 ("id" . "http://example.com/1"))
               (lambda (a b) (string< (car a) (car b)))))
             (ekg-llm-note-to-text
              (make-ekg-note :id "http://example.com/1"
                             :properties '(:titled/title ("Title"))
                             :text "Content"
                             :creation-time time
                             :modified-time time
                             :tags '("tag1" "tag2")))))))

(provide 'ekg-llm-test)

;;; ekg-llm-test.el ends here
