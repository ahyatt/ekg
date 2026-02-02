;;; ekg-org-test.el --- Tests for ekg-org.el  -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

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

;; These tests should be run and pass before every commit to ekg-org.

(require 'ert)
(require 'ekg)
(require 'ekg-org)
(require 'ekg-test-utils)

(defun ekg-org-test-parse-out-id (result-string)
  "Helper to parse out integer ID from RESULT-STRING."
  (when (string-match "\\([0-9]+\\)" result-string)
    (string-to-number (match-string 1 result-string))))

(ekg-deftest ekg-org-test-basic-rendering ()
  "Test that a basic task is rendered correctly."
  (ekg-org-add-schema)
  (let* ((note-id (ekg-org-test-parse-out-id
                   (ekg-org--agent-tool-add-item
                    "Test Task"
                    "This is a test task content."
                    '("tag1" "tag2")
                    nil
                    "TODO"
                    nil
                    nil)))
         (note (ekg-get-note-with-id note-id))
         (rendered (ekg-org-task-to-string (ekg-get-note-with-id note-id))))
    (should (string-match-p "\\* TODO Test Task" rendered))
    (should (string-match-p ":EKG_ID:" rendered))
    (should (string-match-p "This is a test task content." rendered))
    (should (string-match-p ":tag1:tag2:" rendered))
    ;; Verify org/task tag is present
    (should (member ekg-org-task-tag (ekg-note-tags note)))
    ;; Verify org/state tag is present
    (should (seq-some (lambda (tag) (string-prefix-p ekg-org-state-tag-prefix tag))
                      (ekg-note-tags note)))))

(ekg-deftest ekg-org-test-timestamps ()
  "Test that deadlines and scheduled dates are stored correctly."
  (ekg-org-add-schema)
  (let* ((deadline "2026-02-01 10:00")
         (scheduled "2026-01-31 09:00")
         (note-result (ekg-org--agent-tool-add-item
                       "Timed Task"
                       "Content"
                       nil
                       nil
                       "TODO"
                       deadline
                       scheduled))
         (note-id (ekg-org-test-parse-out-id note-result))
         (note (ekg-get-note-with-id note-id))
         (rendered (ekg-org-task-to-string note)))
    ;; Verify basic structure
    (should (string-match-p "\\* TODO Timed Task" rendered))
    (should (string-match-p "Content" rendered))
    ;; Verify the timestamps are stored as properties
    (let ((props (ekg-note-properties note)))
      (should (plist-get props :org/deadline))
      (should (plist-get props :org/scheduled)))))

(ekg-deftest ekg-org-test-hierarchy ()
  "Test that child tasks have correct parent relationship."
  (ekg-org-add-schema)
  (let* ((parent-result (ekg-org--agent-tool-add-item
                         "Parent Task"
                         "Parent content"
                         nil nil "TODO" nil nil))
         ;; The result will have the integer ID as a substring, let's just parse it out.
         (parent-id (ekg-org-test-parse-out-id parent-result))
         ;; Add a child task
         (child (ekg-org--agent-tool-add-item
                 "Child Task"
                 "Child content"
                 nil parent-id "TODO" nil nil))
         (child-id (ekg-org-test-parse-out-id child)))
    ;; Verify both tasks are created
    (should (ekg-get-note-with-id parent-id))
    (should (ekg-get-note-with-id child-id))
    ;; Verify child has correct parent-id property
    (let* ((child-note (ekg-get-note-with-id child-id))
           (child-props (ekg-note-properties child-note)))
      (should (= (plist-get child-props :org/parent) parent-id)))
    ;; Verify parent task can be rendered
    (let* ((parent-note (ekg-get-note-with-id parent-id))
           (rendered (ekg-org-task-to-string parent-note)))
      (should (string-match-p "\\* TODO Parent Task" rendered))
      (should (string-match-p "Parent content" rendered)))))

(ekg-deftest ekg-org-test-generate-content ()
  "Test ekg-org-generate-org-content function."
  (ekg-org-add-schema)
  ;; Create multiple tasks
  (let* ((task1-id (ekg-org--agent-tool-add-item
                    "Task One"
                    "First task content"
                    '("project1") nil "TODO" nil nil))
         (task2-id (ekg-org--agent-tool-add-item
                    "Task Two"
                    "Second task content"
                    '("project2") nil "DONE" nil nil))
         ;; Generate content
         (content (ekg-org-generate-org-content)))
    ;; Verify both tasks appear in generated content
    (should (string-match-p "\\* TODO Task One" content))
    (should (string-match-p "\\* DONE Task Two" content))
    (should (string-match-p "First task content" content))
    (should (string-match-p "Second task content" content))
    ;; Verify EKG_ID properties are present
    (should (string-match-p ":EKG_ID:" content))
    ;; Verify tags are present
    (should (string-match-p ":project1:" content))
    (should (string-match-p ":project2:" content))))

(ekg-deftest ekg-org-test-archive ()
  "Test that archived tasks are handled correctly."
  (ekg-org-add-schema)
  ;; Create normal task
  (let* ((normal (ekg-org-test-parse-out-id
                  (ekg-org--agent-tool-add-item
                   "Normal Task"
                   "Normal content"
                   nil nil "TODO" nil nil)))
         ;; Create archived task by adding archive tag
         (archived-id (ekg-org-test-parse-out-id
                       (ekg-org--agent-tool-add-item
                        "Archived Task"
                        "Archived content"
                        (list ekg-org-archive-tag) nil "DONE" nil nil))))

    ;; Verify normal content does not include archived task
    (let ((normal-content (ekg-org-generate-org-content nil)))
      (should (string-match-p "Normal Task" normal-content))
      (should-not (string-match-p "Archived Task" normal-content)))

    ;; Verify archive content includes only archived task
    (let ((archive-content (ekg-org-generate-org-content t)))
      (should (string-match-p "Archived Task" archive-content))
      (should-not (string-match-p "Normal Task" archive-content)))

    ;; Verify archived task has the archive tag
    (let* ((archived-note (ekg-get-note-with-id archived-id)))
      (should (member ekg-org-archive-tag (ekg-note-tags archived-note))))))

(ekg-deftest ekg-org-test-set-status ()
  "Test changing status of a task."
  (ekg-org-add-schema)
  (let* ((note-result (ekg-org--agent-tool-add-item
                       "Status Task"
                       "Content"
                       nil nil "TODO" nil nil))
         (note-id (ekg-org-test-parse-out-id note-result)))
    ;; Change status to DONE
    (ekg-org--agent-tool-set-status note-id "DONE")
    (let* ((note (ekg-get-note-with-id note-id))
           (rendered (ekg-org-task-to-string note)))
      ;; Verify status changed in rendered output
      (should (string-match-p "\\* DONE Status Task" rendered))
      ;; Verify org/state tag is updated
      (should (member (concat ekg-org-state-tag-prefix "done") (ekg-note-tags note)))
      ;; Verify old TODO tag is removed
      (should-not (member (concat ekg-org-state-tag-prefix "todo") (ekg-note-tags note))))))

(defun ekg-org-test-count-tasks (content)
  "Helper to count number of tasks in CONTENT string."
  (let ((count 0)
        (start 0))
    (while (string-match "^\\* " content start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(ekg-deftest ekg-org-test-list-items ()
  "Test listing tasks."
  (ekg-org-add-schema)
  (ekg-org--agent-tool-add-item "Task 1" "C1" nil nil "TODO" nil nil)
  (ekg-org--agent-tool-add-item "Task 2" "C2" nil nil "DONE" nil nil)
  (ekg-org--agent-tool-add-item "Archived" "AC"
                                (list ekg-org-archive-tag) nil "DONE" nil nil)
  (let ((all (ekg-org--agent-tool-list-items))
        (todos (ekg-org--agent-tool-list-items "TODO")))
    ;; Normal listing should not include archived
    (should (= (ekg-org-test-count-tasks all) 2))
    (should (= (ekg-org-test-count-tasks todos) 1))
    (should (string-match-p "Task 1" todos))))

(ekg-deftest ekg-org-test-tag-handling ()
  "Test that org/task and org/state tags are handled correctly."
  (ekg-org-add-schema)
  (let* ((note-id
          (ekg-org-test-parse-out-id
           (ekg-org--agent-tool-add-item
            "Tagged Task"
            "Content with tags"
            '("custom-tag" "project/test")
            nil
            "IN-PROGRESS"
            nil nil)))
         (note (ekg-get-note-with-id note-id))
         (tags (ekg-note-tags note))
         (rendered (ekg-org-task-to-string note)))

    ;; Verify automatic tags are added
    (should (member ekg-org-task-tag tags))
    (should (member (concat ekg-org-state-tag-prefix "in-progress") tags))

    ;; Verify custom tags are preserved
    (should (member "custom-tag" tags))
    (should (member "project/test" tags))

    ;; Verify rendering shows correct status
    (should (string-match-p "\\* IN-PROGRESS Tagged Task" rendered))

    ;; Verify all tags appear in rendered output
    (should (string-match-p ":custom-tag:" rendered))
    (should (string-match-p ":project/test:" rendered))))

(provide 'ekg-org-test)
