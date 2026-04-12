;;; ekg-test-utils.el --- Testing utilities for ekg  -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2023  Andrew Hyatt <ahyatt@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
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


(require 'cl-lib)
(require 'seq)
(require 'ert)


(defmacro ekg--with-default-configuration (&rest body)
  "Remove any user hooks or custom options for testing.

BODY are the expressions to run in this context."
  (declare (debug t) (indent defun))
  `(let ((ekg-add-schema-hook nil)
         (ekg-note-pre-save-hook nil)
         (ekg-note-save-hook nil)
         (ekg-note-pre-delete-hook nil)
         (ekg-note-delete-hook nil)
         (ekg-note-add-tag-hook nil)
         (ekg-confirm-on-buffer-kill nil))
     (progn ,@body)))


(defmacro ekg--with-buffer-cleanup (&rest body)
  "Cleanup any buffers that are created.

BODY are the expressions to run in this context."
  (declare (debug t) (indent defun))
  `(let ((orig-buffers (buffer-list)))
     (unwind-protect
         (progn ,@body)
       (mapc #'kill-buffer (seq-difference (buffer-list) orig-buffers)))))


(defmacro ekg--with-testing-env (&rest body)
  "Run in an environment suitable for testing.

BODY are the expressions to run in this context."
  (declare (debug t) (indent defun))
  `(ekg--with-default-configuration
    (ekg--with-buffer-cleanup
     (save-excursion ,@body))))


(defmacro ekg--with-error-on-connect (&rest body)
  "Mock `ekg-connect' to error when called.

This allows us to ensure tests aren't trying to connect to a database
when they shouldn't be.

BODY are the expressions to run in this context."
  (declare (debug t) (indent defun))
  `(cl-letf (((symbol-function 'ekg-connect)
              (lambda () (error "This test tried to use ekg-connect"))))
     (progn ,@body)))


(defmacro ekg-deftest (name _ &rest body)
  "A test that will setup a clean environment.

This avoids tests interacting with preexisting resources, like the
user's database.

Argument NAME is the name of the testing function.
BODY is the test body."
  (declare (debug t) (indent defun))
  `(ert-deftest ,name ()
     (ekg--with-testing-env
      (ekg--with-error-on-connect
       ,@body))))


(defmacro ekg--with-sequential-id-generation (&rest body)
  "Number ids sequentially.

This allows us to see what order they were generated in.

BODY are the expressions to run in this context."
  (declare (debug t) (indent defun))
  `(let ((id-count 0))
     (cl-letf (((symbol-function 'ekg--generate-id)
                (lambda () (cl-incf id-count))))
       (progn ,@body))))


(defmacro ekg--with-testing-db (&rest body)
  "Run in an environment with a clean database connection setup.

BODY are the expressions to run in this context."
  (declare (debug t) (indent defun))
  `(ekg--with-testing-env
    (ekg--with-sequential-id-generation
     (let* ((ekg-db-dir (make-temp-file "ekg-test-dir" t))
            (ekg-db-file (expand-file-name "db" ekg-db-dir))
            (ekg-db nil)
            (triples-default-database-filename nil))
       (ekg-connect)
       (triples-set-type ekg-db 'ekg 'ekg :version (version-to-list ekg-version))
       ,@body))))


(defmacro ekg-deftest-with-db (name _ &rest body)
  "Run a test with a clean database connection already setup.

Argument NAME is the name of the testing function.
BODY is the test body."
  (declare (debug t) (indent defun))
  `(ert-deftest ,name ()
     (ekg--with-testing-db
      ,@body)))


(provide 'ekg-test-utils)

;;; ekg-test-utils.el ends here
