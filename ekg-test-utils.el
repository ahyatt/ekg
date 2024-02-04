;;; ekg-test-utils.el --- Testing utilities for ekg  -*- lexical-binding: t; -*-

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

;; Helpers for testing out ekg, needed by multiple test files.

;;; Code:
(defmacro ekg-deftest (name _ &rest body)
  "A test that will set up an empty `ekg-db' for use.
Argument NAME is the name of the testing function.
BODY is the test body."
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
           (ekg-note-add-tag-hook nil)
           (ekg-confirm-on-buffer-kill nil))
       (ekg-connect)
       (triples-set-type ekg-db 'ekg 'ekg :version (version-to-list ekg-version))
       (save-excursion
         (unwind-protect
             (progn ,@body)
           ;; Kill all opened bufferes
           (mapc #'kill-buffer (seq-difference (buffer-list) orig-buffers)))))))

(provide 'ekg-test-utils)

;;; ekg-test-utils.el ends here
