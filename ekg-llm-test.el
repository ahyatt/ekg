;;; ekg-llm-test.el --- Tests for ekg-llm  -*- lexical-binding: t; -*-

;; Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>

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

;; These tests should be run and pass before every commit to ekg-llm.

;;; Code:
(require 'ekg)
(require 'ekg-test-utils)
(require 'ekg-llm)
(require 'ekg-test-utils)
(require 'llm-fake)

(ekg-deftest-with-db ekg-llm-test-save-response ()
  (let ((parent (ekg-note-create :text "Question" :mode 'org-mode
                                 :tags '("topic" "prompt"))))
    (ekg-save-note parent)
    (let* ((response (ekg-llm-save-response parent "**Answer**"))
           (reloaded (ekg-get-note-with-id (ekg-note-id response))))
      (should (equal (ekg-note-text reloaded) "**Answer**"))
      (should (eq (ekg-note-mode reloaded) 'markdown-mode))
      (should (member "topic" (ekg-note-tags reloaded)))
      (should-not (member "prompt" (ekg-note-tags reloaded)))
      (should (member ekg-llm-generated-tag (ekg-note-tags reloaded)))
      (should (equal (ekg-note-parent-id reloaded) (ekg-note-id parent)))
      (should (equal (mapcar #'ekg-note-id
                             (ekg-note-child-notes parent))
                     (list (ekg-note-id response))))
      (should (equal (ekg-note-text (ekg-get-note-with-id
                                     (ekg-note-id parent)))
                     "Question")))))

(ekg-deftest-with-db ekg-llm-test-generated-tag-can-be-disabled ()
  (let ((parent (ekg-note-create :text "Question"))
        (ekg-llm-generated-tag nil))
    (ekg-save-note parent)
    (should-not (ekg-note-tags
                 (ekg-llm-save-response parent "Answer")))))

(ekg-deftest-with-db ekg-llm-test-streaming-creates-response-note ()
  (let* ((parent (ekg-note-create :text "Question" :tags '("topic")))
         (provider
          (make-llm-fake :chat-action-func (lambda () "Generated answer")))
         (ekg-llm-provider provider)
         (ekg-embedding-provider provider))
    (ekg-save-note parent)
    (ekg-edit parent)
    (ekg-llm-respond-to-note)
    (let ((responses (ekg-note-child-notes parent)))
      (should (= (length responses) 1))
      (should (equal (ekg-note-text (car responses)) "Generated answer"))
      (should (member ekg-llm-generated-tag
                      (ekg-note-tags (car responses)))))))

(ekg-deftest-with-db ekg-llm-test-streaming-error-creates-no-response ()
  (let* ((parent (ekg-note-create :text "Question" :tags '("topic")))
         (provider
          (make-llm-fake :chat-action-func
                         (lambda () '(error "Generation failed"))))
         (ekg-llm-provider provider)
         (ekg-embedding-provider provider))
    (ekg-save-note parent)
    (ekg-edit parent)
    (should-error (ekg-llm-respond-to-note))
    (should-not (ekg-note-child-notes parent))
    (should-not
     (seq-some
      (lambda (overlay)
        (string-match-p "LLM response (generating)"
                        (or (overlay-get overlay 'after-string) "")))
      (overlays-at (point-max))))))

(ekg-deftest-with-db ekg-llm-test-note-to-text ()
  (let* ((time (current-time))
         (time-str (format-time-string "%Y-%m-%dT%H:%M:%S" time))
         (json-encoding-pretty-print t))
    (cl-letf (((symbol-function 'ekg-inline-command-const-inline)
               (lambda (&rest _) "inline")))
      (should (equal
               (json-encode
                (sort
                 `(("tags" . ["tag1" "tag2"])
                   ("created" . ,time-str)
                   ("modified" . ,time-str)
                   ("title" . ["Title"])
                   ("text" . "Contentinline\n")
                   ("mode" . "text-mode")
                   ("id" . "http://example.com/1"))
                 (lambda (a b) (string< (car a) (car b)))))
               (ekg-llm-note-to-text
                (make-ekg-note :id "http://example.com/1"
                               :properties '(:titled/title ("Title"))
                               :text "Content"
                               :mode 'text-mode
                               :creation-time time
                               :modified-time time
                               :tags '("tag1" "tag2")
                               :inlines
                               (list (make-ekg-inline :pos 7
                                                      :command '(const-inline)
                                                      :type 'command)))))))))

(ekg-deftest-with-db ekg-llm-test-note-hierarchy-context ()
  (let ((root (ekg-note-create :id "root" :text "Root text"))
        (parent (ekg-note-create :id "parent" :text "Parent text"))
        (current (ekg-note-create :id "current" :text "Current text"))
        (child (ekg-note-create :id "child" :text "Child text"))
        (grandchild (ekg-note-create :id "grandchild"
                                     :text "Grandchild text"))
        (sibling (ekg-note-create :id "sibling" :text "Sibling text")))
    (ekg-save-note root)
    (dolist (pair `((,parent . ,root)
                    (,current . ,parent)
                    (,child . ,current)
                    (,grandchild . ,child)
                    (,sibling . ,parent)))
      (ekg-note-set-parent (car pair) (cdr pair))
      (ekg-save-note (car pair)))
    (let ((payload
           (json-parse-string
            (ekg-llm-note-hierarchy-context current)
            :object-type 'alist :array-type 'list :null-object nil)))
      (cl-labels ((field (key object)
                    (alist-get key object nil nil #'string=)))
        (should (equal (field "format" payload)
                       "ekg-note-hierarchy-v1"))
        (let* ((ancestors (field "ancestor_chain" payload))
               (root-entry (nth 0 ancestors))
               (parent-entry (nth 1 ancestors))
               (current-entry (field "current_note" payload))
               (child-entry (car (field "children" current-entry)))
               (grandchild-entry
                (car (field "children" child-entry))))
          (should (= (length ancestors) 2))
          (should (equal (field "relationship" root-entry) "ancestor"))
          (should (= (field "depth" root-entry) -2))
          (should-not (field "parent_id" root-entry))
          (should (equal (field "note_id" parent-entry) "parent"))
          (should (= (field "depth" parent-entry) -1))
          (should (equal (field "parent_id" parent-entry) "root"))
          (should (equal (field "relationship" current-entry) "current"))
          (should (= (field "depth" current-entry) 0))
          (should (equal (field "parent_id" current-entry) "parent"))
          ;; Current text is sent as the user message, so it is not duplicated.
          (should-not (assoc "text" (field "note" current-entry)))
          (should (equal (field "note_id" child-entry) "child"))
          (should (= (field "depth" child-entry) 1))
          (should (equal (field "parent_id" child-entry) "current"))
          (should (equal (field "note_id" grandchild-entry) "grandchild"))
          (should (= (field "depth" grandchild-entry) 2))
          (should (equal (field "parent_id" grandchild-entry) "child"))
          (should-not (field "children" grandchild-entry))
          ;; Other branches from an ancestor are not part of this slice.
          (should-not (string-match-p "Sibling text"
                                      (json-encode payload))))))))

(provide 'ekg-llm-test)

;;; ekg-llm-test.el ends here
