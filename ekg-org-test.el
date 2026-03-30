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
(require 'ekg-agent)
(require 'ekg-test-utils)

(defun ekg-org-test-parse-out-id (result-string)
  "Helper to parse out integer ID from RESULT-STRING."
  (when (string-match "\\([0-9]+\\)" result-string)
    (string-to-number (match-string 1 result-string))))

(ekg-deftest ekg-org-test-basic-rendering ()
  "Test that a basic task is rendered correctly."
  (ekg-org-add-schema)
  (let* ((note-id (ekg-org-test-parse-out-id
                   (ekg-agent-org--tool-add-item
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
         (note-result (ekg-agent-org--tool-add-item
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
  (let* ((parent-result (ekg-agent-org--tool-add-item
                         "Parent Task"
                         "Parent content"
                         nil nil "TODO" nil nil))
         ;; The result will have the integer ID as a substring, let's just parse it out.
         (parent-id (ekg-org-test-parse-out-id parent-result))
         ;; Add a child task
         (child (ekg-agent-org--tool-add-item
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

(ekg-deftest ekg-org-test-save-with-virtual-reversed ()
  "Test that saving a parent note with org/children doesn't error.
When a note has children, reading it populates :org/children as a
virtual-reversed property.  Saving it back must not attempt to
write that property."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test-parse-out-id
                     (ekg-agent-org--tool-add-item
                      "Parent" "content" nil nil "TODO" nil nil)))
         (child-id (ekg-org-test-parse-out-id
                    (ekg-agent-org--tool-add-item
                     "Child" "child content" nil parent-id "TODO" nil nil)))
         (parent-note (ekg-get-note-with-id parent-id)))
    ;; Verify the virtual-reversed property is present when reading.
    (should (plist-get (ekg-note-properties parent-note) :org/children))
    ;; Saving the parent should not error.
    (ekg-save-note parent-note)
    ;; Verify the child relationship is still intact after save.
    (let ((reloaded (ekg-get-note-with-id child-id)))
      (should (= (plist-get (ekg-note-properties reloaded) :org/parent)
                 parent-id)))))

(ekg-deftest ekg-org-test-generate-content ()
  "Test ekg-org-generate-org-content function."
  (ekg-org-add-schema)
  ;; Create multiple tasks
  (let* ((task1-id (ekg-agent-org--tool-add-item
                    "Task One"
                    "First task content"
                    '("project1") nil "TODO" nil nil))
         (task2-id (ekg-agent-org--tool-add-item
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
                  (ekg-agent-org--tool-add-item
                   "Normal Task"
                   "Normal content"
                   nil nil "TODO" nil nil)))
         ;; Create archived task by adding archive tag
         (archived-id (ekg-org-test-parse-out-id
                       (ekg-agent-org--tool-add-item
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
  (let* ((note-result (ekg-agent-org--tool-add-item
                       "Status Task"
                       "Content"
                       nil nil "TODO" nil nil))
         (note-id (ekg-org-test-parse-out-id note-result)))
    ;; Change status to DONE
    (ekg-agent-org--tool-set-status note-id "DONE")
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
  (ekg-agent-org--tool-add-item "Task 1" "C1" nil nil "TODO" nil nil)
  (ekg-agent-org--tool-add-item "Task 2" "C2" nil nil "DONE" nil nil)
  (ekg-agent-org--tool-add-item "Archived" "AC"
                                (list ekg-org-archive-tag) nil "DONE" nil nil)
  (let ((all (ekg-agent-org--tool-list-items))
        (todos (ekg-agent-org--tool-list-items "TODO")))
    ;; Normal listing should not include archived
    (should (= (ekg-org-test-count-tasks all) 2))
    (should (= (ekg-org-test-count-tasks todos) 1))
    (should (string-match-p "Task 1" todos))))

(ekg-deftest ekg-org-test-tag-handling ()
  "Test that org/task and org/state tags are handled correctly."
  (ekg-org-add-schema)
  (let* ((note-id
          (ekg-org-test-parse-out-id
           (ekg-agent-org--tool-add-item
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

;;; View tests

(defun ekg-org-test--add-task (title &optional parent-id state)
  "Create a task with TITLE, optional PARENT-ID and STATE.
Returns the note ID."
  (let ((result (ekg-agent-org--tool-add-item
                 title "" nil parent-id (or state "TODO") nil nil)))
    (ekg-org-test-parse-out-id result)))

(defun ekg-org-test--view-headings ()
  "Return a list of (LEVEL TITLE) pairs from the current view buffer."
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (let (headings)
      (while (not (eobp))
        (when (get-text-property (point) :ekg-org-heading)
          (let* ((level (get-text-property (point) :ekg-org-level))
                 (id (get-text-property (point) :ekg-org-note-id))
                 (note (ekg-get-note-with-id id))
                 (title (car (plist-get (ekg-note-properties note)
                                        :titled/title))))
            (push (list level title) headings)))
        (forward-line 1))
      (nreverse headings))))

(defun ekg-org-test--view-titles ()
  "Return a flat list of titles from the current view buffer."
  (mapcar #'cadr (ekg-org-test--view-headings)))

(defun ekg-org-test--task-id-by-title (title)
  "Return the note ID of the task with TITLE."
  (let ((notes (ekg-get-notes-with-tag ekg-org-task-tag)))
    (cl-loop for note in notes
             when (equal (car (plist-get (ekg-note-properties note)
                                         :titled/title))
                         title)
             return (ekg-note-id note))))

(ekg-deftest ekg-org-test-view-basic-hierarchy ()
  "Test that the view renders a parent with children in order."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test--add-task "Parent"))
         (_child1 (ekg-org-test--add-task "Child A" parent-id))
         (_child2 (ekg-org-test--add-task "Child B" parent-id)))
    (ekg-org-view)
    (let ((headings (ekg-org-test--view-headings)))
      (should (equal headings
                     '((1 "Parent") (2 "Child A") (2 "Child B")))))))

(ekg-deftest ekg-org-test-view-body-fontified ()
  "Test that org body text in the view is fontified, not raw text."
  (ekg-org-add-schema)
  (ekg-agent-org--tool-add-item
   "Task With Body" "This has *bold* and /italic/ text." nil nil "TODO" nil nil)
  (ekg-org-view)
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    ;; Find the body text in the buffer.
    (should (search-forward "bold" nil t))
    ;; The word "bold" should have a face property from org fontification,
    ;; not just the old ekg-org-view-body shadow face.
    (let ((face (get-text-property (match-beginning 0) 'face)))
      (should face)
      (should-not (eq face 'ekg-org-view-body)))))

(ekg-deftest ekg-org-test-view-multiple-top-level ()
  "Test that multiple top-level tasks all appear."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "First")
  (ekg-org-test--add-task "Second")
  (ekg-org-test--add-task "Third")
  (ekg-org-view)
  (let ((titles (ekg-org-test--view-titles)))
    (should (= (length titles) 3))
    (should (member "First" titles))
    (should (member "Second" titles))
    (should (member "Third" titles))))

(ekg-deftest ekg-org-test-view-collapse ()
  "Test that collapsing a parent hides its children."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test--add-task "Parent"))
         (_child (ekg-org-test--add-task "Child" parent-id)))
    (ekg-org-view)
    (should (= (length (ekg-org-test--view-headings)) 2))
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (ekg-org-view-toggle-collapse))
    (should (equal (ekg-org-test--view-headings)
                   '((1 "Parent"))))))

(ekg-deftest ekg-org-test-view-archive-removes-from-view ()
  "Test that archiving a task removes it from the active view."
  (ekg-org-add-schema)
  (let ((id (ekg-org-test--add-task "To Archive")))
    (ekg-org-view)
    (should (member "To Archive" (ekg-org-test--view-titles)))
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (let ((note (ekg-get-note-with-id id)))
        (push ekg-org-archive-tag (ekg-note-tags note))
        (ekg-save-note note)))
    (ekg-org-view)
    (should-not (member "To Archive" (ekg-org-test--view-titles)))))

(ekg-deftest ekg-org-test-view-archive-appears-in-archive-view ()
  "Test that archived tasks appear in the archive view."
  (ekg-org-add-schema)
  (let ((id (ekg-org-test--add-task "Archived Task")))
    (let ((note (ekg-get-note-with-id id)))
      (push ekg-org-archive-tag (ekg-note-tags note))
      (ekg-save-note note))
    (ekg-org-archive-view)
    (with-current-buffer "*ekg-org-archive*"
      (goto-char (point-min))
      (let (titles)
        (while (not (eobp))
          (when (get-text-property (point) :ekg-org-heading)
            (let* ((nid (get-text-property (point) :ekg-org-note-id))
                   (note (ekg-get-note-with-id nid))
                   (title (car (plist-get (ekg-note-properties note)
                                          :titled/title))))
              (push title titles)))
          (forward-line 1))
        (should (member "Archived Task" titles))))))

(ekg-deftest ekg-org-test-view-promote ()
  "Test that promoting a child makes it a sibling of its parent."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test--add-task "Parent"))
         (child-id (ekg-org-test--add-task "Child" parent-id)))
    (ekg-org-view)
    ;; Navigate to the child and promote it
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (ekg-org-view-next-heading)
      (ekg-org-view-promote))
    ;; Child should now be top-level
    (let ((headings (ekg-org-test--view-headings)))
      (should (= (length headings) 2))
      (should (equal (assoc 1 headings) '(1 "Parent")))
      (should (member '(1 "Child") headings)))))

(ekg-deftest ekg-org-test-view-demote ()
  "Test that demoting a task makes it a child of the previous sibling."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "First")
  (ekg-org-test--add-task "Second")
  (ekg-org-view)
  ;; Navigate to "Second" and demote it
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (ekg-org-view-next-heading)
    (ekg-org-view-demote))
  (let ((headings (ekg-org-test--view-headings)))
    (should (= (length headings) 2))
    (should (equal (car headings) '(1 "First")))
    (should (equal (cadr headings) '(2 "Second")))))

(ekg-deftest ekg-org-test-view-move-up ()
  "Test that moving a task up swaps it with its previous sibling."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "First")
  (ekg-org-test--add-task "Second")
  (ekg-org-test--add-task "Third")
  (ekg-org-view)
  (should (equal (ekg-org-test--view-titles) '("First" "Second" "Third")))
  ;; Navigate to "Third" and move it up
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (ekg-org-view-next-heading)
    (ekg-org-view-next-heading)
    (ekg-org-view-move-up))
  (should (equal (ekg-org-test--view-titles) '("First" "Third" "Second"))))

(ekg-deftest ekg-org-test-view-move-down ()
  "Test that moving a task down swaps it with its next sibling."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "First")
  (ekg-org-test--add-task "Second")
  (ekg-org-test--add-task "Third")
  (ekg-org-view)
  (should (equal (ekg-org-test--view-titles) '("First" "Second" "Third")))
  ;; Navigate to "First" and move it down
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (ekg-org-view-move-down))
  (should (equal (ekg-org-test--view-titles) '("Second" "First" "Third"))))

(ekg-deftest ekg-org-test-view-move-up-at-top ()
  "Test that moving up at the top position is a no-op."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "First")
  (ekg-org-test--add-task "Second")
  (ekg-org-view)
  ;; Point is on "First", moving up should do nothing
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (ekg-org-view-move-up))
  (should (equal (ekg-org-test--view-titles) '("First" "Second"))))

(ekg-deftest ekg-org-test-view-move-down-at-bottom ()
  "Test that moving down at the bottom position is a no-op."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "First")
  (ekg-org-test--add-task "Second")
  (ekg-org-view)
  ;; Navigate to "Second" and try moving down
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (ekg-org-view-next-heading)
    (ekg-org-view-move-down))
  (should (equal (ekg-org-test--view-titles) '("First" "Second"))))

(ekg-deftest ekg-org-test-view-state-change ()
  "Test that changing state updates the heading."
  (ekg-org-add-schema)
  (let ((id (ekg-org-test--add-task "My Task" nil "TODO")))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (should (string-match-p "TODO" (buffer-substring
                                      (line-beginning-position)
                                      (line-end-position)))))
    ;; Change state directly and refresh
    (let ((note (ekg-get-note-with-id id)))
      (setf (ekg-note-tags note)
            (cons (concat ekg-org-state-tag-prefix "done")
                  (seq-remove
                   (lambda (tag)
                     (string-prefix-p ekg-org-state-tag-prefix tag))
                   (ekg-note-tags note))))
      (ekg-save-note note))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (should (string-match-p "DONE" (buffer-substring
                                      (line-beginning-position)
                                      (line-end-position)))))))

(ekg-deftest ekg-org-test-view-insert-mode-sibling ()
  "Test that insert mode creates a sibling after the current task."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "First")
  (ekg-org-test--add-task "Third")
  (ekg-org-view)
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    ;; Enter insert mode and find the slot after "First" at level 1.
    (ekg-org-view-create)
    (let ((first-id (ekg-org-test--task-id-by-title "First"))
          (target-slot nil))
      (cl-loop for slot in ekg-org-view--insert-slots
               when (and (= (plist-get slot :level) 1)
                         (equal (plist-get slot :after-id) first-id))
               do (setq target-slot slot))
      (ekg-org-view--insert-cleanup)
      (ekg-org-view--insert-create-task target-slot "Second")))
  (let ((titles (ekg-org-test--view-titles)))
    (should (equal titles '("First" "Second" "Third")))))

(ekg-deftest ekg-org-test-view-insert-mode-child ()
  "Test that insert mode creates a child task."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "Parent")
  (ekg-org-view)
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (ekg-org-view-create)
    (let ((parent-id (ekg-org-test--task-id-by-title "Parent"))
          (target-slot nil))
      (cl-loop for slot in ekg-org-view--insert-slots
               when (and (= (plist-get slot :level) 2)
                         (equal (plist-get slot :parent-id) parent-id))
               do (setq target-slot slot))
      (ekg-org-view--insert-cleanup)
      (ekg-org-view--insert-create-task target-slot "Child")))
  (let ((headings (ekg-org-test--view-headings)))
    (should (equal headings '((1 "Parent") (2 "Child"))))))

(ekg-deftest ekg-org-test-view-insert-mode-top-level ()
  "Test that insert mode creates a top-level task at the beginning."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "Existing")
  (ekg-org-view)
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (ekg-org-view-create)
    (let ((target-slot nil))
      (cl-loop for slot in ekg-org-view--insert-slots
               when (and (= (plist-get slot :level) 1)
                         (null (plist-get slot :after-id)))
               do (progn (setq target-slot slot) (cl-return)))
      (ekg-org-view--insert-cleanup)
      (ekg-org-view--insert-create-task target-slot "New First")))
  (let ((titles (ekg-org-test--view-titles)))
    (should (equal (car titles) "New First"))
    (should (= (length titles) 2))))

(ekg-deftest ekg-org-test-view-insert-initial-position ()
  "Test that insert mode defaults to first-child of the heading at point."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "Alpha")
  (let ((alpha-id (ekg-org-test--task-id-by-title "Alpha")))
    (ekg-org-test--add-task "Alpha Child" alpha-id))
  (ekg-org-test--add-task "Beta")
  (ekg-org-view)
  (with-current-buffer "*ekg-org-tasks*"
    ;; Navigate to "Beta" heading.
    (goto-char (point-min))
    (while (not (equal (ekg-org-view--note-at-point)
                       (ekg-org-test--task-id-by-title "Beta")))
      (forward-line 1))
    (ekg-org-view-create)
    (let* ((slot (nth ekg-org-view--insert-index ekg-org-view--insert-slots))
           (beta-id (ekg-org-test--task-id-by-title "Beta")))
      (ekg-org-view--insert-cleanup)
      (should (equal (plist-get slot :parent-id) beta-id))
      (ekg-org-view--insert-create-task slot "Beta Child")))
  (let ((headings (ekg-org-test--view-headings)))
    (should (equal headings '((1 "Alpha") (2 "Alpha Child")
                              (1 "Beta") (2 "Beta Child"))))))

(ekg-deftest ekg-org-test-view-top-level-spacing ()
  "Test that blank lines separate top-level task groups after creation."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "Alpha")
  (ekg-org-test--add-task "Beta")
  (ekg-org-view)
  (with-current-buffer "*ekg-org-tasks*"
    ;; Create a child of Beta via insert mode.
    (goto-char (point-min))
    (while (not (equal (ekg-org-view--note-at-point)
                       (ekg-org-test--task-id-by-title "Beta")))
      (forward-line 1))
    (ekg-org-view-create)
    (let ((slot (nth ekg-org-view--insert-index ekg-org-view--insert-slots)))
      (ekg-org-view--insert-cleanup)
      (ekg-org-view--insert-create-task slot "Beta Child"))
    ;; There should be a blank line between Alpha and Beta groups.
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\n\n\\*" text)
              ))))

(ekg-deftest ekg-org-test-view-insert-point-on-new-item ()
  "Test that point lands on the newly created item after insertion."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "First")
  (ekg-org-test--add-task "Third")
  (ekg-org-view)
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (ekg-org-view-create)
    (let ((first-id (ekg-org-test--task-id-by-title "First"))
          (target-slot nil))
      (cl-loop for slot in ekg-org-view--insert-slots
               when (and (= (plist-get slot :level) 1)
                         (equal (plist-get slot :after-id) first-id))
               do (setq target-slot slot))
      (ekg-org-view--insert-cleanup)
      (ekg-org-view--insert-create-task target-slot "Second"))
    (should (ekg-org-view--on-heading-p))
    (let* ((id (ekg-org-view--note-at-point))
           (note (ekg-get-note-with-id id))
           (title (car (plist-get (ekg-note-properties note)
                                  :titled/title))))
      (should (equal title "Second")))))

(ekg-deftest ekg-org-test-view-insert-slot-after-subtree ()
  "Test that the sibling-after slot is placed after the full subtree."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test--add-task "Parent"))
         (_child-id (ekg-org-test--add-task "Child" parent-id)))
    (ekg-org-test--add-task "Next")
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (ekg-org-view-create)
      ;; Find the sibling-after slot for "Parent" at level 1.
      (let ((parent-note-id (ekg-org-test--task-id-by-title "Parent"))
            (next-note-id (ekg-org-test--task-id-by-title "Next"))
            (target-slot nil)
            (next-heading-pos nil))
        ;; Find the heading position for "Next".
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (and (ekg-org-view--on-heading-p)
                       (equal (ekg-org-view--note-at-point) next-note-id))
              (setq next-heading-pos (point)))
            (forward-line 1)))
        (cl-loop for slot in ekg-org-view--insert-slots
                 when (and (= (plist-get slot :level) 1)
                           (equal (plist-get slot :after-id) parent-note-id))
                 do (setq target-slot slot))
        (ekg-org-view--insert-cleanup)
        ;; The slot's buffer-pos should be at "Next", not at "Child".
        (should target-slot)
        (should (equal (plist-get target-slot :buffer-pos)
                       next-heading-pos))))))

(ekg-deftest ekg-org-test-view-trash ()
  "Test that trashing a task removes it from the view."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "Keep")
  (ekg-org-test--add-task "Trash Me")
  (ekg-org-view)
  (should (= (length (ekg-org-test--view-titles)) 2))
  (with-current-buffer "*ekg-org-tasks*"
    (goto-char (point-min))
    (ekg-org-view-next-heading)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
      (ekg-org-view-delete)))
  (let ((titles (ekg-org-test--view-titles)))
    (should (= (length titles) 1))
    (should (equal (car titles) "Keep"))))

(ekg-deftest ekg-org-test-view-trash-with-children ()
  "Test that trashing a task also trashes its children."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test--add-task "Parent"))
         (child-id (ekg-org-test--add-task "Child" parent-id))
         (grandchild-id (ekg-org-test--add-task "Grandchild" child-id)))
    (ekg-org-test--add-task "Keep Me")
    (ekg-org-view)
    (should (= (length (ekg-org-test--view-titles)) 4))
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (ekg-org-view--ensure-on-heading)
      ;; Find and trash the parent.
      (search-forward "Parent")
      (beginning-of-line)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
        (ekg-org-view-delete)))
    (let ((titles (ekg-org-test--view-titles)))
      (should (= (length titles) 1))
      (should (equal (car titles) "Keep Me")))
    (should-not (ekg-note-active-p (ekg-get-note-with-id parent-id)))
    (should-not (ekg-note-active-p (ekg-get-note-with-id child-id)))
    (should-not (ekg-note-active-p (ekg-get-note-with-id grandchild-id)))))

(ekg-deftest ekg-org-test-view-archive-with-children ()
  "Test that archiving a task also archives its children."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test--add-task "Archive Parent"))
         (child-id (ekg-org-test--add-task "Archive Child" parent-id)))
    (ekg-org-test--add-task "Stay Visible")
    (ekg-org-view)
    (should (= (length (ekg-org-test--view-titles)) 3))
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (search-forward "Archive Parent")
      (beginning-of-line)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
        (ekg-org-view-archive)))
    (let ((titles (ekg-org-test--view-titles)))
      (should (= (length titles) 1))
      (should (equal (car titles) "Stay Visible")))
    (should (member ekg-org-archive-tag
                    (ekg-note-tags (ekg-get-note-with-id parent-id))))
    (should (member ekg-org-archive-tag
                    (ekg-note-tags (ekg-get-note-with-id child-id))))))

(ekg-deftest ekg-org-test-view-navigation ()
  "Test heading navigation commands."
  (ekg-org-add-schema)
  (let* ((p1 (ekg-org-test--add-task "Parent 1"))
         (_c1 (ekg-org-test--add-task "Child 1" p1))
         (_p2 (ekg-org-test--add-task "Parent 2")))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      ;; At "Parent 1"
      (should (equal (get-text-property (point) :ekg-org-level) 1))
      ;; Next heading → "Child 1"
      (ekg-org-view-next-heading)
      (should (equal (get-text-property (point) :ekg-org-level) 2))
      ;; Up → back to "Parent 1"
      (ekg-org-view-up-heading)
      (should (equal (get-text-property (point) :ekg-org-level) 1))
      (let* ((id (get-text-property (point) :ekg-org-note-id))
             (title (car (plist-get (ekg-note-properties
                                     (ekg-get-note-with-id id))
                                    :titled/title))))
        (should (equal title "Parent 1")))
      ;; Next sibling → "Parent 2"
      (ekg-org-view-next-sibling)
      (let* ((id (get-text-property (point) :ekg-org-note-id))
             (title (car (plist-get (ekg-note-properties
                                     (ekg-get-note-with-id id))
                                    :titled/title))))
        (should (equal title "Parent 2"))))))

(ekg-deftest ekg-org-test-view-deep-hierarchy ()
  "Test a three-level deep hierarchy renders correctly."
  (ekg-org-add-schema)
  (let* ((gp (ekg-org-test--add-task "Grandparent"))
         (p (ekg-org-test--add-task "Parent" gp))
         (_c (ekg-org-test--add-task "Child" p)))
    (ekg-org-view)
    (should (equal (ekg-org-test--view-headings)
                   '((1 "Grandparent") (2 "Parent") (3 "Child"))))))

(ekg-deftest ekg-org-test-view-promote-to-grandparent ()
  "Test promoting a deeply nested child to its grandparent's level."
  (ekg-org-add-schema)
  (let* ((gp (ekg-org-test--add-task "Grandparent"))
         (p (ekg-org-test--add-task "Parent" gp))
         (_c (ekg-org-test--add-task "Child" p)))
    (ekg-org-view)
    ;; Navigate to "Child" (3rd heading) and promote
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (ekg-org-view-next-heading)
      (ekg-org-view-next-heading)
      (ekg-org-view-promote))
    ;; "Child" should now be at level 2 (sibling of Parent)
    (let ((headings (ekg-org-test--view-headings)))
      (should (= (length headings) 3))
      (should (member '(2 "Child") headings))
      (should (member '(2 "Parent") headings)))))

(ekg-deftest ekg-org-test-view-auto-refresh-on-external-save ()
  "Test that the view refreshes when notes change outside of user actions.
An agent saving a new note should cause the view to update automatically."
  (ekg-org-add-schema)
  (ekg-org-test--add-task "Initial Task")
  (ekg-org-view)
  (should (equal (ekg-org-test--view-titles) '("Initial Task")))
  ;; Simulate an external agent adding a task by directly saving a note.
  ;; The save hook should trigger a view refresh.
  (let ((note (ekg-note-create
               :text ""
               :mode 'org-mode
               :tags (list ekg-org-task-tag
                           (concat ekg-org-state-tag-prefix "todo"))
               :properties (list :titled/title '("Agent Task")))))
    (ekg-save-note note))
  (let ((titles (ekg-org-test--view-titles)))
    (should (= (length titles) 2))
    (should (member "Agent Task" titles))
    (should (member "Initial Task" titles))))

(ekg-deftest ekg-org-test-view-auto-refresh-on-external-delete ()
  "Test that the view refreshes when a note is deleted externally."
  (ekg-org-add-schema)
  (let ((id (ekg-org-test--add-task "Doomed Task")))
    (ekg-org-test--add-task "Survivor")
    (ekg-org-view)
    (should (= (length (ekg-org-test--view-titles)) 2))
    ;; Simulate an external agent deleting the note.
    (ekg-note-delete (ekg-get-note-with-id id))
    (let ((titles (ekg-org-test--view-titles)))
      (should (= (length titles) 1))
      (should (equal (car titles) "Survivor")))))

(ekg-deftest ekg-org-test-view-auto-refresh-on-external-state-change ()
  "Test that the view updates when a note's state changes externally."
  (ekg-org-add-schema)
  (let ((id (ekg-org-test--add-task "My Task" nil "TODO")))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (should (string-match-p "TODO" (buffer-substring
                                      (line-beginning-position)
                                      (line-end-position)))))
    ;; Simulate an external agent changing the state.
    (let ((note (ekg-get-note-with-id id)))
      (setf (ekg-note-tags note)
            (cons (concat ekg-org-state-tag-prefix "done")
                  (seq-remove
                   (lambda (tag)
                     (string-prefix-p ekg-org-state-tag-prefix tag))
                   (ekg-note-tags note))))
      (ekg-save-note note))
    ;; The view should have refreshed automatically.
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (should (string-match-p "DONE" (buffer-substring
                                      (line-beginning-position)
                                      (line-end-position)))))))

(ekg-deftest ekg-org-test-view-refile-to-new-parent ()
  "Test that refiling moves a task under a new parent."
  (ekg-org-add-schema)
  (let* ((p1 (ekg-org-test--add-task "Parent A"))
         (p2 (ekg-org-test--add-task "Parent B"))
         (child (ekg-org-test--add-task "Child" p1)))
    (ekg-org-view)
    ;; Navigate to "Child" and refile it under "Parent B".
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (while (not (equal (ekg-org-view--note-at-point) child))
        (forward-line 1))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (car (cl-find-if
                         (lambda (c) (string-match-p "Parent B" (car c)))
                         choices)))))
        (ekg-org-view-refile)))
    ;; Child should now be under Parent B.
    (let ((headings (ekg-org-test--view-headings)))
      (should (equal headings
                     '((1 "Parent A") (1 "Parent B") (2 "Child")))))))

(ekg-deftest ekg-org-test-view-refile-to-top-level ()
  "Test that refiling to top level removes the parent."
  (ekg-org-add-schema)
  (let* ((parent (ekg-org-test--add-task "Parent"))
         (child (ekg-org-test--add-task "Child" parent)))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (while (not (equal (ekg-org-view--note-at-point) child))
        (forward-line 1))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (car (cl-find-if
                         (lambda (c) (string= "Top level" (car c)))
                         choices)))))
        (ekg-org-view-refile)))
    ;; Both tasks should now be top-level.
    (let ((headings (ekg-org-test--view-headings)))
      (should (= (length headings) 2))
      (should (member '(1 "Parent") headings))
      (should (member '(1 "Child") headings)))))

(ekg-deftest ekg-org-test-view-refile-excludes-descendants ()
  "Test that refile targets exclude the task itself and its descendants."
  (ekg-org-add-schema)
  (let* ((p (ekg-org-test--add-task "Parent"))
         (c (ekg-org-test--add-task "Child" p))
         (gc (ekg-org-test--add-task "Grandchild" c)))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      ;; Get refile choices for "Parent" — should not include itself,
      ;; Child, or Grandchild.
      (let ((choices (ekg-org-view--all-tasks-for-refile p)))
        (should (= (length choices) 1))
        (should (string= "Top level" (caar choices)))))))

(ekg-deftest ekg-org-test-properties ()
  "Test generic org property get/set/remove."
  (ekg-org-add-schema)
  (let* ((id (ekg-org-test--add-task "Task with props"))
         (note (ekg-get-note-with-id id)))
    ;; Initially no properties
    (should (null (ekg-org-properties-alist note)))
    (should (null (ekg-org-get-property note "WORKTREE")))
    ;; Set a property
    (ekg-org-set-property note "WORKTREE" "my-branch")
    (ekg-save-note note)
    (setq note (ekg-get-note-with-id id))
    (should (equal "my-branch" (ekg-org-get-property note "WORKTREE")))
    ;; Case-insensitive lookup
    (should (equal "my-branch" (ekg-org-get-property note "worktree")))
    ;; Set a second property
    (ekg-org-set-property note "CATEGORY" "work")
    (ekg-save-note note)
    (setq note (ekg-get-note-with-id id))
    (should (equal "work" (ekg-org-get-property note "CATEGORY")))
    (should (equal "my-branch" (ekg-org-get-property note "WORKTREE")))
    (should (= 2 (length (ekg-org-properties-alist note))))
    ;; Update existing property
    (ekg-org-set-property note "WORKTREE" "other-branch")
    (ekg-save-note note)
    (setq note (ekg-get-note-with-id id))
    (should (equal "other-branch" (ekg-org-get-property note "WORKTREE")))
    (should (= 2 (length (ekg-org-properties-alist note))))
    ;; Remove a property
    (ekg-org-remove-property note "CATEGORY")
    (ekg-save-note note)
    (setq note (ekg-get-note-with-id id))
    (should (null (ekg-org-get-property note "CATEGORY")))
    (should (equal "other-branch" (ekg-org-get-property note "WORKTREE")))
    (should (= 1 (length (ekg-org-properties-alist note))))))

(provide 'ekg-org-test)
