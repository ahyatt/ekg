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
(require 'ekg-test-utils)
(require 'ekg-logseq)
(require 'cl-lib)
(require 'org)

(ert-deftest ekg-logseq-test-to-import-text ()
  (with-temp-buffer
    (insert
     "#+title:test\n#+ekg_export: true\n\n"
     "* Untitled Note\n:PROPERTIES:\n:ID: 123\n:EKG_HASH: abc\n:END:\n#[[test]]\ntest\n"
     "* Untitled Note\n:PROPERTIES:\n:ID: 123\n:EKG_HASH: bce\n:END:\n#[[test]]\ntest\n")
    (org-mode)
    (should (equal (ekg-logseq--to-import-text) nil)))
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
    (insert ":PROPERTIES:\n:ID:123\n:END:#+title: My org file\nContents")
    (org-mode)
    (should (equal '("Contents") (ekg-logseq--to-import-text))))
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

(ert-deftest ekg-logseq-test-text-to-note ()
  (cl-letf (((symbol-function 'ekg-note-with-id-exists-p) (lambda (id) (or (equal id 321) (equal id 123)))))
    (let ((note (ekg-logseq--text-to-note "tag1" "Headline\n  id:: 123\nText and {{embed ((abc))}} {{embed ((321))}}")))
      (should (equal "Headline\n  id:: 123\nText and  " (ekg-note-text note)))
      (should (= 2 (length (ekg-note-inlines note))))
      (should (equal (nth 0 (ekg-note-inlines note))
                     (make-ekg-inline :pos 29 :command '(transclude-note "abc") :type 'command)))
      (should (equal (nth 1 (ekg-note-inlines note))
                     (make-ekg-inline :pos 30 :command '(transclude-note 321) :type 'command)))
      (should (equal 123 (ekg-note-id note)))
      (should (equal '("tag1") (ekg-note-tags note))))))

(ert-deftest ekg-logseq-test-note-to-logseq ()
  (let ((note (ekg-note-create :text "line1\nline2\n" :mode 'org-mode :tags '("tag1" "tag2" "tag3"))))
    (setf (ekg-note-id note) 123)
    (setf (ekg-note-modified-time note) 123456789)
    (should (equal
             "* Untitled Note\n:PROPERTIES:\n:ID: 123\n:EKG_HASH: c696f3d4b296c737155637d3a708d2b986ab6f6f\n:END:\n#[[tag1]] #[[tag2]]\nline1\nline2\n"
             (ekg-logseq-note-to-logseq-org note "tag3")))
    (should (equal
             "- Untitled Note\n  id:: 123\n  ekg_hash:: c696f3d4b296c737155637d3a708d2b986ab6f6f\n  #[[tag1]] #[[tag2]]\n  line1\n  line2\n"
             (ekg-logseq-note-to-logseq-md note "tag3")))))

(ert-deftest ekg-logseq-test-note-to-logseq-with-inlines ()
  (let ((note (ekg-note-create :text " " :mode 'org-mode :tags '("tag1" "tag2" "tag3"))))
    (setf (ekg-note-id note) 123)
    (setf (ekg-note-modified-time note) 1682139975)
    (setf (ekg-note-creation-time note) 1682053575)
    (setf (ekg-note-inlines note)
          (list (make-ekg-inline :pos 0 :command '(transclude-note "abc") :type 'command)
                (make-ekg-inline :pos 1 :command '(time-tracked) :type 'note)))
    (should (equal
             "* Untitled Note\n:PROPERTIES:\n:ID: 123\n:EKG_HASH: b858cb282617fb0956d960215c8e84d1ccf909c6\n:END:\n#[[tag1]] #[[tag2]]\n{{embed ((abc))}} Created: 2023-04-21   Modified: 2023-04-22\n"
             (ekg-logseq-note-to-logseq-org note "tag3")))
    (should (equal
             "- Untitled Note\n  id:: 123\n  ekg_hash:: b858cb282617fb0956d960215c8e84d1ccf909c6\n  #[[tag1]] #[[tag2]]\n  {{embed ((abc))}} Created: 2023-04-21   Modified: 2023-04-22\n"
             (ekg-logseq-note-to-logseq-md note "tag3")))))

(ert-deftest ekg-logseq-test-note-with-resource-and-title-to-logseq ()
  (let ((note (ekg-note-create :text "line1\nline2\n" :mode 'org-mode :tags '("tag1" "tag2" "tag3"))))
    (setf (ekg-note-id note) "http://www.example.com")
    (setf (ekg-note-modified-time note) 123456789)
    (setf (ekg-note-properties note) '(:titled/title ("Title")))
    (should (equal
             "* Title\n:PROPERTIES:\n:ID: http://www.example.com\n:EKG_HASH: c696f3d4b296c737155637d3a708d2b986ab6f6f\n:END:\n#[[tag1]] #[[tag2]]\nhttp://www.example.com\nline1\nline2\n"
             (ekg-logseq-note-to-logseq-org note "tag3")))
    (should (equal
             "- Title\n  id:: http://www.example.com\n  ekg_hash:: c696f3d4b296c737155637d3a708d2b986ab6f6f\n  #[[tag1]] #[[tag2]]\n  http://www.example.com\n  line1\n  line2\n"
             (ekg-logseq-note-to-logseq-md note "tag3")))))

(ert-deftest ekg-logseq-test-note-to-logseq-org-demotion ()
  (let ((note (ekg-note-create :text "* Heading 1\n* Heading 2" :mode 'org-mode :tags nil)))
    (setf (ekg-note-id note) 123)
    (setf (ekg-note-modified-time note) 123456789)
    (should (equal
             "* Untitled Note\n:PROPERTIES:\n:ID: 123\n:EKG_HASH: c3475d5573fae5cd21fb32a9ec2e75a7d0e4d409\n:END:\n** Heading 1\n** Heading 2"
             (ekg-logseq-note-to-logseq-org note "tag3")))))

(ert-deftest ekg-logseq-test-notes-to-logseq ()
  (let ((note1 (ekg-note-create :text "note1" :mode 'org-mode :tags '("a" "b" "c")))
        (note2 (ekg-note-create :text "note2" :mode 'org-mode :tags '("b" "a" "c")))
        (note3 (ekg-note-create :text "" :mode 'org-mode :tags '("a" "b" "c")))
        (note4 (ekg-note-create :text "note4" :mode 'org-mode :tags '("a" "b" "c" "trash"))))
    (setf (ekg-note-id note1) 1)
    (setf (ekg-note-modified-time note1) 123456789)
    (setf (ekg-note-id note4) 4)
    (setf (ekg-note-modified-time note1) 123456789)
    (should
     (equal (concat
             "#+title: a\n#+ekg_export: true\n\n"
             "* Untitled Note\n:PROPERTIES:\n:ID: 1\n:EKG_HASH: 829ab920fad6efe045caf218b813be4e42ac779a\n:END:\n#[[b]] #[[c]]\nnote1")
            (ekg-logseq-notes-to-logseq (list note1 note2 note3 note4) "a" t)))
    (should (equal (concat
                    "title:: a\nekg_export:: true\n\n"
                    "- Untitled Note\n  id:: 1\n  ekg_hash:: 829ab920fad6efe045caf218b813be4e42ac779a\n  #[[b]] #[[c]]\n  note1")
                   (ekg-logseq-notes-to-logseq (list note1 note2 note3 note4) "a" nil)))))

(ert-deftest ekg-logseq-test-filename-to-tag ()
  (should (equal "foo" (ekg-logseq-filename-to-tag "foo.org")))
  (should (equal "foo/bar" (ekg-logseq-filename-to-tag "foo$bar.org"))))

(ekg-deftest ekg-logseq-test-last-import-export ()
  (should (= 0 (ekg-logseq-get-last-export)))
  (should (= 0 (ekg-logseq-get-last-import)))
  (ekg-logseq-set-last-export 123)
  (should (= 123 (ekg-logseq-get-last-export)))
  (should (= 0 (ekg-logseq-get-last-import)))
  (ekg-logseq-set-last-import 456)
  (should (= 123 (ekg-logseq-get-last-export)))
  (should (= 456 (ekg-logseq-get-last-import))))

(ekg-deftest ekg-logseq-test-tags-with-notes-modified-since ()
  (cl-flet ((create (time tags)
              (let ((note (ekg-note-create :text "" :mode 'org-mode :tags tags)))
                (cl-letf (((symbol-function 'time-convert)
                           (lambda (&rest _) time)))
                  (ekg-save-note note)))))
    (create 100 '("a"))
    (create 200 '("a" "b"))
    (create 300 '("b"))
    (create 400 '("c" "d"))
    (should (equal
             (sort (ekg-logseq-tags-with-notes-modified-since 0) #'string<)
             '("a" "b" "c" "d")))
    (should (equal
             (sort (ekg-logseq-tags-with-notes-modified-since 1) #'string<)
             '("a" "b" "c" "d")))
    (should (equal
             (ekg-logseq-tags-with-notes-modified-since 1000)
             nil))
    (should (equal
             (sort (ekg-logseq-tags-with-notes-modified-since 300) #'string<)
             '("c" "d")))))

;;; ekg-logseq-test.el ends here
