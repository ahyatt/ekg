;;; ekg-logseq-test.el --- Tests for ekg-logseq  -*- lexical-binding: t; -*-

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
(require 'ekg-logseq)
(require 'cl-lib)
(require 'org)

(ert-deftest ekg-logseq-test-to-import-text ()
  (with-temp-buffer
    (insert "* Heading\n** Subheading\n*** Subsubheading\n"
            "* Heading 2\n:PROPERTIES:\n:EKG_HASH: 123\n:END:\n"
            "* Heading 3\n:PROPERTIES:\n:ekg_hash: 456\n:END:\n"
            "* Heading 4\nText for another subheading")
    (org-mode)
    (should (equal '("* Heading\n** Subheading\n*** Subsubheading\n"
                     "* Heading 4\nText for another subheading")
                   (ekg-logseq--to-import-text))))

  (with-temp-buffer
    (insert "- ## My markdown file\n" "Contents")
    (should (equal '("## My markdown file\nContents")
                   (ekg-logseq--to-import-text))))
  (with-temp-buffer
    (insert "id:: 123\n- ## My markdown file\n" "Contents")
    (should (equal '("## My markdown file\nContents") (ekg-logseq--to-import-text))))
  (with-temp-buffer
    (insert "- Item 1\nid:: abc\nText 1\n- Item 2\n  - ekg_hash:: 123 \n- ## Heading 1\nid:: def\n"
            "- ## Heading 2\nText 2")
    (should (equal (ekg-logseq--to-import-text)
                   '("Item 1\nid:: abc\nText 1\n"
                     "## Heading 1\nid:: def\n"
                     "## Heading 2\nText 2")))))

(ert-deftest ekg-logseq-test-to-import-tags ()
  (should (equal nil (ekg-logseq--to-import-tags "Just text here, no tags")))
  (should (equal '("foo" "bar baz")
                 (ekg-logseq--to-import-tags "Sometimes I say #foo, sometimes I say #[[bar baz]]"))))

(ert-deftest ekg-logseq-test-to-import-md-id ()
  (should (equal nil (ekg-logseq--to-import-md-id "Just text here, no id")))
  (should (equal "123" (ekg-logseq--to-import-md-id "Headline\n  id:: 123\nText"))))

(ert-deftest ekg-logseq-test-to-import-org-id ()
  (should (equal nil (ekg-logseq--to-import-org-id "Just text here, no id")))
  (should (equal "123" (ekg-logseq--to-import-org-id "* Headline\n:PROPERTIES:\n:ID: 123\n:END:\nText")))
  (should (equal "123" (ekg-logseq--to-import-org-id "* Headline\n:PROPERTIES:\n:id: 123\n:END:\nText"))))

(ert-deftest ekg-logseq-test-note-to-logseq ()
  (let ((note (ekg-note-create "line1\nline2\n" 'org-mode '("tag1" "tag2" "tag3"))))
    (setf (ekg-note-id note) "123")
    (setf (ekg-note-modified-time note) 123456789)
    (should (equal
             "* Untitled Note\n:PROPERTIES:\n:ID: 123\n:EKG_HASH: c696f3d4b296c737155637d3a708d2b986ab6f6f\n:END:\n#[[tag1]] #[[tag2]]\nline1\nline2\n"
             (ekg-logseq-note-to-logseq-org note "tag3")))
    (should (equal
             "- Untitled Note\n  id:: 123\n  ekg_hash:: c696f3d4b296c737155637d3a708d2b986ab6f6f\n  #[[tag1]] #[[tag2]]\n  line1\n  line2\n"
             (ekg-logseq-note-to-logseq-md note "tag3")))))

(ert-deftest ekg-logseq-test-note-to-logseq-org-demotion ()
  (let ((note (ekg-note-create "* Heading 1\n* Heading 2" 'org-mode nil)))
    (setf (ekg-note-id note) "123")
    (setf (ekg-note-modified-time note) 123456789)
    (should (equal
             "* Untitled Note\n:PROPERTIES:\n:ID: 123\n:EKG_HASH: c3475d5573fae5cd21fb32a9ec2e75a7d0e4d409\n:END:\n** Heading 1\n** Heading 2"
             (ekg-logseq-note-to-logseq-org note "tag3")))))

(ert-deftest ekg-logseq-test-notes-to-logseq ()
  (let ((note1 (ekg-note-create "note1" 'org-mode '("a" "b" "c")))
        (note2 (ekg-note-create "note2" 'org-mode '("b" "a" "c")))
        (note3 (ekg-note-create "" 'org-mode '("a" "b" "c")))
        (note4 (ekg-note-create "note4" 'org-mode '("a" "b" "c" "trash/d"))))
    (setf (ekg-note-id note1) "1")
    (setf (ekg-note-modified-time note1) 123456789)
    (setf (ekg-note-id note4) "4")
    (setf (ekg-note-modified-time note1) 123456789)
    (equal (concat
            "* Untitled Note\n:PROPERTIES:\n:ID: 1\n:EKG_HASH: 123456789\n:END:\n#[[b]] #[[c]]\nnote1\n"
            "* Untitled Note\n:PROPERTIES:\n:ID: 4\n:EKG_HASH: 123456789\n:END:\n#[[b]] #[[c]]\nnote4\n")
           (ekg-logseq-notes-to-logseq (list note1 note2 note3 note4) "a" t))
    (equal (concat
            "- Untitled Note\n  id:: 1\n  ekg_hash:: 123456789\n#[[b]] #[[c]]\nnote1\n"
            "- Untitled Note\n  id:: 4\n  ekg_hash:: 123456789\n#[[b]] #[[c]]\nnote4\n")
           (ekg-logseq-notes-to-logseq (list note1 note2 note3 note4) "a" nil))))

(ert-deftest ekg-logseq-test-filename-to-tag ()
  (should (equal "foo" (ekg-logseq-filename-to-tag "foo.org")))
  (should (equal "foo/bar" (ekg-logseq-filename-to-tag "foo$bar.org"))))

;;; ekg-logseq-test.el ends here
