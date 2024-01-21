;;; ekg-denote-test.el --- Tests for ekg-denote  -*- lexical-binding: t; -*-

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

;; These tests should be run and pass before every commit.

;;; Code:

(require 'ert)
(require 'ekg-test-utils)
(require 'ekg-denote)
(require 'cl-lib)

(ert-deftest ekg-denote-test-sublist-kws ()
  "Combined length of the sublist kws is as per the allowed combined length argument."
  (should (equal '("kw1") (ekg-denote-sublist-kws '("kw1" "kw2" "kw 3") 4)))
  (should (equal '("kw1" "kw2") (ekg-denote-sublist-kws '("kw1" "kw2" "kw 3") 8)))
  (should (equal '("kw1" "kw2" "kw 3") (ekg-denote-sublist-kws '("kw1" "kw2" "kw 3") 13))))

(ert-deftest ekg-denote-test-assert-notes-have-duplicate-creation-time-should-error ()
  "Duplicate notes should error."
  (let* ((time (time-convert (current-time) 'integer))
	 (note1 (make-ekg-note :id "ID1" :creation-time time))
	 (note2 (make-ekg-note :id "ID2" :creation-time time))
	 (notes (list note1 note2)))
    (should-error (ekg-denote-assert-notes-have-unique-creation-time notes))))

(ert-deftest ekg-denote-test-assert-notes-have-unique-creation-time-should-not-error ()
  "Unique creation time should not error."
  (let* ((time (time-convert (current-time) 'integer))
	 (note1 (make-ekg-note :id "ID1" :creation-time time))
	 (note2 (make-ekg-note :id "ID2" :creation-time (1+ time)))
	 (notes (list note1 note2)))
    (should-not (ekg-denote-assert-notes-have-unique-creation-time notes))))

(ert-deftest ekg-denote-test-notes-have-creation-time ()
  "Test for creation time uniquess needed in notes."

  ;; Raise error if creation time missing.
  (let* ((time (time-convert (current-time) 'integer))
	 (note1 (make-ekg-note :id "ID1" :creation-time time))
	 (note2 (make-ekg-note :id "ID2")))
    (should-error (ekg-denote-assert-notes-have-creation-time notes)))

  ;; Raise no error when creation-time is not missing.
  (let* ((time (time-convert (current-time) 'integer))
	 (note1 (make-ekg-note :id "ID1" :creation-time time))
	 (note2 (make-ekg-note :id "ID2" :creation-time time))
	 (notes (list note1 note2)))
    (should-not (ekg-denote-assert-notes-have-creation-time notes))))


(ert-deftest ekg-denote-test-notes-have-creation-time)

(ert-deftest ekg-denote-test-merge ()
  "Validate the merging of the text with the existing file."
  (let ((denote1 (make-ekg-denote :text "denote-text..." :path "/mock/file/path")))
    (cl-letf (((symbol-function 'ekg-denote-read-file)
	       (lambda (path) "file-txt...")))
      (ekg-denote-merge denote1)
      (should (equal
	       ">>>>>>>\nfile-txt...\n=======\n>>>>>>>\ndenote-text...\n======="
	       (ekg-denote-text denote1))))))
