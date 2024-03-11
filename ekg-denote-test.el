;;; ekg-denote-test.el --- Tests for ekg-denote  -*- lexical-binding: t; -*-

;; Copyright (c) 2024  Jay Rajput <jayrajput@gmail.com>

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

(ert-deftest ekg-denote-test-sublist-keywords ()
  "Combined length of the sublist keywords is as per the allowed combined length argument."
  (should (equal '("kw1") (ekg-denote-sublist-keywords '("kw1" "kw2" "kw 3") 4)))
  (should (equal '("kw1" "kw2") (ekg-denote-sublist-keywords '("kw1" "kw2" "kw 3") 8)))
  (should (equal '("kw1" "kw2" "kw 3") (ekg-denote-sublist-keywords '("kw1" "kw2" "kw 3") 13))))

(ert-deftest ekg-denote-test-notes-having-duplicate-creation-time ()
  "Notes with duplicate creation time should error."
  (let* ((time (time-convert (current-time) 'integer))
	 (note1 (make-ekg-note :id "ID1" :creation-time time))
	 (note2 (make-ekg-note :id "ID2" :creation-time time))
	 (notes (list note1 note2)))
    (should-error (ekg-denote-assert-notes-have-unique-creation-time notes))))

(ert-deftest ekg-denote-test-notes-having-unique-creation-tiem ()
  "Notes with unique creation time should not error."
  (let* ((time (time-convert (current-time) 'integer))
	 (note1 (make-ekg-note :id "ID1" :creation-time time))
	 (note2 (make-ekg-note :id "ID2" :creation-time (1+ time)))
	 (notes (list note1 note2)))
    (should-not (ekg-denote-assert-notes-have-unique-creation-time notes))))

(ert-deftest ekg-denote-test-notes-missing-creation-time ()
  "Notes missing creation time should error."
  (let* ((time (time-convert (current-time) 'integer))
	 (note1 (make-ekg-note :id "ID1" :creation-time time))
	 (note2 (make-ekg-note :id "ID2")))
    (should-error (ekg-denote-assert-notes-have-creation-time notes))))

(ert-deftest ekg-denote-test-notes-not-missing-creation-time ()
  "Notes having creation time should not error."
  (let* ((time (time-convert (current-time) 'integer))
	 (note1 (make-ekg-note :id "ID1" :creation-time time))
	 (note2 (make-ekg-note :id "ID2" :creation-time (1+ time)))
	 (notes (list note1 note2)))
    (should-not (ekg-denote-assert-notes-have-creation-time notes))))

(ert-deftest ekg-denote-test-merge ()
  "Validate the merging of the text with the existing file."
  (let ((denote1 (make-ekg-denote :text "denote-text..." :path "/mock/file/path")))
    (cl-letf (((symbol-function 'ekg-denote-text-from-file)
	       (lambda (path) "file-txt...")))
      (ekg-denote-merge denote1)
      (should (equal
	       "\n>>>>>>>\nfile-txt...\n<<<<<<<\n\n>>>>>>>\ndenote-text...\n<<<<<<<\n"
	       (ekg-denote-text denote1))))))

(ert-deftest ekg-denote-test-create ()
  "Verify creation of `ekg-denote' against given ekg note."

  ; date tag is removed.
  ; text defaults to ""
  ; ext defaults to ekg-default-capture-mode
  ; title defaults to ""
  ; path uses denote-id and tags
  ; ekg-id is copied
  (let* ((time (time-convert (current-time) 'integer))
	 (denote-directory "/tmp")
	 (denote-id (format-time-string denote-id-format time))
	 (ekg-default-capture-mode 'org-mode)
	 (note (make-ekg-note :id "ID1"
			      :creation-time time
			      :tags '("date/20230101" "portfolio" "tag2")))
	 (denote (make-ekg-denote :id denote-id
				  :note-id "ID1"
				  :title ""
				  :text ""
				  :path (format "/tmp/%s__portfolio_tag2.org" denote-id )
				  :kws '("portfolio" "tag2"))))
    (should (equal denote (ekg-denote-create note))))

  ; text is copied as it is
  ; title is truncated, copied and sluggified
  (let* ((time (time-convert (current-time) 'integer))
	 (denote-directory "/tmp")
	 (denote-id (format-time-string denote-id-format time))
	 (ekg-default-capture-mode 'org-mode)
	 (ekg-denote-title-max-len 10)
	 (note (make-ekg-note :id "ID1"
			      :creation-time time
			      :text "Text"
			      :properties `(:titled/title ,(list "Title 123456789"))
			      :tags '("date/20230101" "portfolio")))
	 (denote (make-ekg-denote :id denote-id
				  :note-id "ID1"
				  :title "title-1234"
				  :text "Text"
				  :path (format "/tmp/%s--title-1234__portfolio.org" denote-id )
				  :kws '("portfolio"))))
    (should (equal denote (ekg-denote-create note)))))

(ekg-deftest ekg-test-export ()
  "Verify export."

  ; export create new files
  (let* ((denote-directory (make-temp-file "denote" t)))
    (ekg-save-note (ekg-note-create :text "text1" :mode 'org-mode :tags '("portfolio")))
    (ekg-denote-export)
    (let* ((files (denote-directory-text-only-files))
	   (file (when (length> files 0) (car files))))
      (should file)
      (should (string-match-p "__portfolio" file))
      (should (equal "text1" (with-temp-buffer
			       (insert-file-contents file)
			       (buffer-string)))))))
