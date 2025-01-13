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

  ;; scalar title values shall work
  (let* ((time (time-convert (current-time) 'integer))
	     (denote-directory "/tmp")
	     (denote-id (format-time-string denote-id-format time))
	     (ekg-default-capture-mode 'org-mode)
	     (note (make-ekg-note :id "ID1"
			                  :creation-time time
			                  :text "Text"
			                  :properties `(:titled/title "MyTitle")
			                  :tags '("date/20230101" "portfolio")))
	     (denote (make-ekg-denote :id denote-id
				                  :note-id "ID1"
				                  :title "mytitle"
				                  :text "Text"
				                  :path (format "/tmp/%s--mytitle__portfolio.org" denote-id )
				                  :kws '("portfolio"))))
    (should (equal denote (ekg-denote-create note))))

  ;; text is copied as it is
  ;; title is truncated, copied and sluggified
  (let* ((time (time-convert (current-time) 'integer))
	     (denote-directory "/tmp")
	     (denote-id (format-time-string denote-id-format time))
	     (ekg-default-capture-mode 'org-mode)
	     (ekg-denote-export-title-max-len 10)
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


(defun ekg-test--matching-denote (regexp)
  "Get denote file name containing REGEXP.
Enforces single match."
  (let ((files (seq-filter (lambda (x) (string-match-p regexp x))
			               (denote-directory-files nil nil :text-only))))
    (when files (car files))))

(defun ekg-test--denote-text (file)
  "Get text from denote FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun ekg-test--creation-time-from-denote-file (file)
  "Return creation time for the denote FILE."
  (time-convert
   (encode-time
    (parse-time-string
     (denote-retrieve-filename-identifier file)))
   'integer))

(defun ekg-test--denote-ekg (file)
  "Return ekg note for given denote FILE."
  (let* ((creation-time (ekg-test--creation-time-from-denote-file file))
	     (triples (triples-db-select-pred-op ekg-db :time-tracked/creation-time '= creation-time)))
    (should (length= triples 1))
    (ekg-get-note-with-id (car (car triples)))))

(ekg-deftest ekg-denote-test-export ()
  "Verify export."
  (ekg-denote-add-schema)
  (let ((denote-directory (make-temp-file "denote" t)))
    ;; ekg note creations are exported
    (ekg-save-note (ekg-note-create :text "text1" :mode 'org-mode :tags '("portfolio")))
    (sleep-for 1)
    (ekg-denote-export)
    (setq denote-file (ekg-test--matching-denote "__portfolio"))
    (should (equal "text1" (ekg-test--denote-text denote-file)))
    ;; ekg note updates are exported
    (setq note (ekg-test--denote-ekg denote-file))
    (setf (ekg-note-text note) "text2")
    (ekg-save-note note)
    (sleep-for 1)
    (ekg-denote-export)
    (setq denote-file (ekg-test--matching-denote "__portfolio"))
    (should (equal "text2" (ekg-test--denote-text denote-file)))
    ;; ekg note tag update cause denote file rename
    (setq note (ekg-test--denote-ekg denote-file))
    (setf (ekg-note-tags note) '("updatedtag"))
    (ekg-save-note note)
    (sleep-for 1)
    (ekg-denote-export)
    (should (ekg-test--matching-denote "__updatedtag"))
    (should-not (ekg-test--matching-denote "__portfolio"))))

(ekg-deftest ekg-denote-test-last-export ()
  "Verify last export time updates."
  (ekg-denote-add-schema)
  (should (= 0 (ekg-denote-get-last-export)))
  (ekg-denote-set-last-export 123)
  (should (= 123 (ekg-denote-get-last-export))))
