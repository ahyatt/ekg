;;; ekg-org-test.el --- Tests for ekg-org.el  -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

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

;; These tests should be run and pass before every commit to ekg-org.

(require 'ert)
(require 'ekg)
(require 'ekg-org)
(require 'ekg-agent)
(require 'ekg-test-utils)

(ekg-deftest-with-db ekg-org-test-view-inherits-notes-actions ()
  "Task queries keep the standard notes UI and its key meanings."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (ekg-org-test--add-task "Task")
  (ekg-org-view)
  (dolist (key '("c" "n" "p" "o" "RET" "r" "t" "a" "b" "d" "g" "TAB"))
    (should (eq (key-binding (kbd key))
                (lookup-key ekg-notes-mode-map (kbd key))))))

(ekg-deftest-with-db ekg-org-test-view-notes-capture-root-from-nested ()
  "The normal create action captures a root even from a nested task."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (let* ((parent (ekg-org-test--add-task "Parent"))
         (child (ekg-org-test--add-task "Child" parent))
         (org-todo-keywords '((sequence "OPEN" "|" "CLOSED"))))
    (ekg-org-view-task parent)
    (ekg-org-view--goto-note-id child)
    (call-interactively (key-binding "c"))
    (should ekg-capture-mode)
    (should (derived-mode-p 'org-mode))
    (should (equal (ekg-org-task-state ekg-note) "OPEN"))
    (should-not (ekg-note-parent-id ekg-note))
    (let ((id (ekg-note-id ekg-note)))
      (should-not (ekg-note-with-id-exists-p id))
      (insert "New root body")
      (ekg-capture-finalize)
      (let ((note (ekg-get-note-with-id id)))
        (should (equal (ekg-note-text note) "New root body"))
        (should (member ekg-org-task-tag (ekg-note-tags note)))
        (should-not (ekg-note-parent-id note))
        (should (string-match-p "Untitled task"
                                (ekg-org-task-to-string note))))
      (ekg-org-view)
      (should (member id (ekg-notes-visible-ids))))))

(ekg-deftest-with-db ekg-org-test-view-notes-capture-child-and-abort ()
  "Child capture uses the shared editor and abort leaves siblings intact."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (let* ((parent (ekg-org-test--add-task "Parent"))
         (existing (ekg-org-test--add-task "Existing" parent))
         (note (ekg-get-note-with-id parent)))
    (push "project" (ekg-note-tags note))
    (ekg-save-note note)
    ;; Sparse orders must still append after existing children, not at count 1.
    (ekg-org-view--set-sort-order existing 100)
    (ekg-org-view)
    (ekg-org-view--goto-note-id parent)
    (let ((order (ekg-org-view--sort-order (ekg-get-note-with-id existing))))
      (call-interactively (key-binding "r"))
      (should (equal (ekg-note-parent-id ekg-note) parent))
      (should (derived-mode-p 'markdown-mode))
      (should-not (plist-member (ekg-note-properties ekg-note) :org/sort-order))
      (should (equal (ekg-org-task-state ekg-note) "TODO"))
      (should (member "project" (ekg-note-tags ekg-note)))
      (let ((id (ekg-note-id ekg-note)))
        (insert "Cancelled body")
        (ekg-capture-abort)
        (should-not (ekg-note-with-id-exists-p id)))
      (should (= order (ekg-org-view--sort-order
                        (ekg-get-note-with-id existing)))))
    (ekg-org-view)
    (ekg-org-view--goto-note-id parent)
    (call-interactively (key-binding "r"))
    (let ((id (ekg-note-id ekg-note)))
      (insert "Child body")
      (ekg-capture-finalize)
      (should (equal (ekg-note-parent-id (ekg-get-note-with-id id)) parent))
      (should (= (ekg-org-view--sort-order (ekg-get-note-with-id id)) 101))
      (should (equal (mapcar #'ekg-note-id
                            (ekg-org-view--sorted-children parent))
                     (list existing id))))))

(ekg-deftest-with-db ekg-org-test-view-notes-capture-archive-and-defaults ()
  "Task capture rejects archives without leaking defaults to ordinary notes."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (ekg-org-archive-view)
  (should-error (ekg-notes-create) :type 'user-error)
  (ekg-org-view)
  (let ((org-todo-keywords '((sequence "|" "CLOSED"))))
    (should-error (ekg-notes-create) :type 'user-error))
  (ekg-setup-notes-buffer "ordinary" (lambda () nil) '("topic"))
  (ekg-notes-create)
  (should (member "topic" (ekg-note-tags ekg-note)))
  (should-not (member ekg-org-task-tag (ekg-note-tags ekg-note)))
  (should-not (plist-member (ekg-note-properties ekg-note) :org/sort-order))
  (ekg-capture-abort))

(ekg-deftest-with-db ekg-org-test-capture-hook-defaults-outside-task-view ()
  "Any capture with the task tag gets defaults without a view-specific handler."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (let ((org-todo-keywords '((sequence "OPEN" "|" "CLOSED"))))
    (ekg-capture :mode 'text-mode :tags '("org/task" "project"))
    (should (eq major-mode 'text-mode))
    (should (equal (ekg-org-task-state ekg-note) "OPEN"))
    (should (member "project" (ekg-note-tags ekg-note)))
    (let ((id (ekg-note-id ekg-note)))
      (should-not (ekg-note-with-id-exists-p id))
      (insert "Task from a normal capture")
      (ekg-capture-finalize)
      (should (equal (ekg-org-task-state (ekg-get-note-with-id id)) "OPEN")))))

(ekg-deftest-with-db ekg-org-test-capture-hook-preserves-explicit-state ()
  "Task defaults never replace an explicit state or order."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (let ((org-todo-keywords '((sequence "|" "CLOSED"))))
    (ekg-capture :tags '("org/task" "org/state/closed")
                 :properties '(:org/sort-order 7))
    (should (equal (seq-filter
                    (lambda (tag) (string-prefix-p ekg-org-state-tag-prefix tag))
                    (ekg-note-tags ekg-note))
                   '("org/state/closed")))
    (should (= (plist-get (ekg-note-properties ekg-note) :org/sort-order) 7))
    (let ((id (ekg-note-id ekg-note)))
      (insert "An explicitly positioned completed task")
      (ekg-capture-finalize)
      (let ((saved (ekg-get-note-with-id id)))
        (should (equal (ekg-org-task-state saved) "CLOSED"))
        (should (= (plist-get (ekg-note-properties saved) :org/sort-order) 7))))))

(ekg-deftest-with-db ekg-org-test-capture-header-after-setup-hooks ()
  "Capture formats the header after mode hooks and initial tag hooks finish."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (let ((ekg-capture-mode-hook
         (append ekg-capture-mode-hook
                 (list (lambda ()
                         (should-not header-line-format)
                         (push "mode-ready" (ekg-note-tags ekg-note))))))
        (ekg-note-add-tag-hook
         (list (lambda (tag)
                 (should-not header-line-format)
                 (when (equal tag ekg-org-task-tag)
                   (push "tag-ready" (ekg-note-tags ekg-note)))))))
    (ekg-capture :tags '("org/task"))
    (should (string-match-p "tag-ready" header-line-format))
    (should (member "mode-ready" (ekg-note-tags ekg-note)))
    (should (equal (ekg-org-task-state ekg-note) "TODO"))
    (ekg-capture-abort)))

(ekg-deftest-with-db ekg-org-test-view-respond-to-ordinary-note ()
  "Removing task defaults saves an ordinary child without Org properties."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (let* ((parent (ekg-org-test--add-task "Task"))
         (response (ekg-note-create :text "Discussion" :mode 'markdown-mode
                                    :tags '("project")
                                    :properties (list :hierarchy/parent parent))))
    (ekg-save-note response)
    (ekg-org-view)
    (ekg-org-view--goto-note-id (ekg-note-id response))
    (call-interactively (key-binding "r"))
    (should (derived-mode-p 'markdown-mode))
    (should (equal (ekg-note-parent-id ekg-note) (ekg-note-id response)))
    (should (member ekg-org-task-tag (ekg-note-tags ekg-note)))
    (should (member "project" (ekg-note-tags ekg-note)))
    (should-not (plist-member (ekg-note-properties ekg-note) :org/sort-order))
    (setf (ekg-note-tags ekg-note)
          (seq-remove (lambda (tag) (string-prefix-p "org/" tag))
                      (ekg-note-tags ekg-note)))
    (let ((id (ekg-note-id ekg-note)))
      (insert "An ordinary discussion reply")
      (ekg-capture-finalize)
      (let ((saved (ekg-get-note-with-id id)))
        (should-not (ekg-org--org-note-p saved))
        (should-not (triples-get-type ekg-db id 'org))
        (should (equal (ekg-note-parent-id saved) (ekg-note-id response)))))))

(ekg-deftest-with-db ekg-org-test-task-captures-order-by-save ()
  "Open captures receive append orders when saved, not when opened."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (let ((parent (ekg-org-test--add-task "Task")))
    (ekg-org-view)
    (ekg-notes-respond)
    (let ((first (current-buffer)) (first-id (ekg-note-id ekg-note)))
      (should-not (plist-member (ekg-note-properties ekg-note) :org/sort-order))
      (ekg-org-view)
      (ekg-org-view--goto-note-id parent)
      (ekg-notes-respond)
      (let ((second-id (ekg-note-id ekg-note)))
        (insert "Saved first")
        (ekg-capture-finalize)
        (switch-to-buffer first)
        (insert "Saved second")
        (ekg-capture-finalize)
        (should (equal (mapcar #'ekg-note-id
                              (ekg-org-view--sorted-children parent))
                       (list second-id first-id)))))))

(ekg-deftest-with-db ekg-org-test-task-marker-overrides-old-properties ()
  "Stale Org metadata cannot turn an untagged note back into a task."
  (ekg-org-add-schema)
  (add-hook 'ekg-note-pre-save-hook #'ekg-org--save-task-defaults)
  (let ((note (ekg-note-create :text "Discussion" :tags '("project")
                               :properties '(:org/sort-order 5))))
    (ekg-save-note note)
    (should-not (ekg-org--org-note-p (ekg-get-note-with-id (ekg-note-id note))))))

(defun ekg-org-test-parse-out-id (result-string)
  "Helper to parse out integer ID from RESULT-STRING."
  (when (string-match "\\([0-9]+\\)" result-string)
    (string-to-number (match-string 1 result-string))))

(ekg-deftest-with-db ekg-org-test-response-tags-exclude-task-control ()
  (let ((task (ekg-note-create
               :text "Task"
               :tags (list "project"
                           ekg-org-task-tag
                           ekg-org-archive-tag
                           (concat ekg-org-state-tag-prefix "todo")))))
    (ekg-save-note task)
    (should (equal (ekg-response-inherited-tags task) '("project")))))

(ekg-deftest-with-db ekg-org-test-basic-rendering ()
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

(ekg-deftest-with-db ekg-org-test-timestamps ()
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
    (should (string-match-p "DEADLINE: <2026-02-01 Sun 10:00>" rendered))
    (should (string-match-p "SCHEDULED: <2026-01-31 Sat 09:00>" rendered))
    ;; Verify the timestamps are stored as properties
    (let ((props (ekg-note-properties note)))
      (should (plist-get props :org/deadline))
      (should (plist-get props :org/scheduled)))))

(ekg-deftest-with-db ekg-org-test-date-only-timestamps ()
  "Keep midnight deadlines and schedules as date-only Org timestamps."
  (ekg-org-add-schema)
  (let* ((note-id (ekg-org-test-parse-out-id
                   (ekg-agent-org--tool-add-item
                    "Date-only Task" nil nil nil "TODO"
                    "2026-02-01" "2026-01-31")))
         (rendered (ekg-org-task-to-string
                    (ekg-get-note-with-id note-id))))
    (should (string-match-p "DEADLINE: <2026-02-01 Sun>" rendered))
    (should (string-match-p "SCHEDULED: <2026-01-31 Sat>" rendered))
    (should-not (string-match-p "2026-02-01 Sun 00:00" rendered))
    (should-not (string-match-p "2026-01-31 Sat 00:00" rendered))))

(ekg-deftest-with-db ekg-org-test-hierarchy ()
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
      (should (= (plist-get child-props :hierarchy/parent) parent-id)))
    ;; Verify parent task can be rendered
    (let* ((parent-note (ekg-get-note-with-id parent-id))
           (rendered (ekg-org-task-to-string parent-note)))
      (should (string-match-p "\\* TODO Parent Task" rendered))
      (should (string-match-p "Parent content" rendered)))))

(ekg-deftest-with-db ekg-org-test-non-task-response-in-task-tree ()
  "Keep response notes visible without treating them as Org tasks."
  (ekg-org-add-schema)
  (let* ((task-id (ekg-org-test-parse-out-id
                   (ekg-agent-org--tool-add-item
                    "Task" "Task body" nil nil "TODO" nil nil)))
         (task (ekg-get-note-with-id task-id))
         (response (ekg-note-create :text "User answer"
                                    :mode 'markdown-mode)))
    (ekg-note-set-parent response task)
    (ekg-save-note response)
    (should (equal (mapcar #'ekg-note-id
                           (ekg-org-get-child-tasks-of-id task-id))
                   nil))
    (let ((org-text (ekg-org-task-to-string task)))
      (should (string-match-p "\\*\\* Response" org-text))
      (should (string-match-p "User answer" org-text))
      (should-not (string-match-p "\\*\\* TODO Response" org-text)))
    (should (member (ekg-note-id response)
                    (mapcar #'ekg-note-id
                            (ekg-org-view--display-children task-id))))))

(ekg-deftest-with-db ekg-org-test-child-dependency-type ()
  "Test parallel and sequential child dependency rendering."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test-parse-out-id
                     (ekg-agent-org--tool-add-item
                      "Parent Task" "Parent content"
                      nil nil "TODO" nil nil)))
         (_child-id (ekg-org-test-parse-out-id
                     (ekg-agent-org--tool-add-item
                      "Child Task" "Child content"
                      nil parent-id "TODO" nil nil)))
         (parent (ekg-get-note-with-id parent-id)))
    (should (eq (ekg-org-dependency-type parent) 'sequential))
    (should (string-match-p "^:ORDERED: *t$"
                            (ekg-org-task-to-string parent)))
    (ekg-org-set-property parent "OWNER" "Andrew")
    (ekg-org-set-dependency-type parent 'parallel)
    (ekg-save-note parent)
    (setq parent (ekg-get-note-with-id parent-id))
    (should (equal (ekg-org-get-property parent "ORDERED") "nil"))
    (should (eq (ekg-org-dependency-type parent) 'parallel))
    (let* ((rendered (ekg-org-task-to-string parent))
           (parent-rendered
            (substring rendered 0 (string-match "^\\*\\* " rendered))))
      (should-not (string-match-p "^:ORDERED:" parent-rendered))
      (should (string-match-p "^:OWNER: *Andrew$" parent-rendered)))
    (ekg-org-set-dependency-type parent 'sequential)
    (ekg-save-note parent)
    (setq parent (ekg-get-note-with-id parent-id))
    (should (eq (ekg-org-dependency-type parent) 'sequential))
    (should (string-match-p "^:ORDERED: *t$"
                            (ekg-org-task-to-string parent)))))

(ekg-deftest-with-db ekg-org-test-save-with-virtual-reversed ()
  "Test that saving a parent note with hierarchy/children doesn't error.
When a note has children, reading it populates :hierarchy/children as a
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
    (should (plist-get (ekg-note-properties parent-note)
                       :hierarchy/children))
    ;; Saving the parent should not error.
    (ekg-save-note parent-note)
    ;; Verify the child relationship is still intact after save.
    (let ((reloaded (ekg-get-note-with-id child-id)))
      (should (= (plist-get (ekg-note-properties reloaded)
                            :hierarchy/parent)
                 parent-id)))))

(ekg-deftest-with-db ekg-org-test-generate-content ()
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

(ekg-deftest-with-db ekg-org-test-archive ()
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

(ekg-deftest-with-db ekg-org-test-set-status ()
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

(ekg-deftest-with-db ekg-org-test-list-items ()
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

(ekg-deftest-with-db ekg-org-test-tag-handling ()
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
        (when (ekg-org-view--on-heading-p)
          (let* ((level (ekg-org-view--level-at-point))
                 (id (get-text-property (point) :ekg-note-id))
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

(ekg-deftest-with-db ekg-org-test-view-basic-hierarchy ()
  "Test that the view renders a parent with children in order."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test--add-task "Parent"))
         (_child1 (ekg-org-test--add-task "Child A" parent-id))
         (_child2 (ekg-org-test--add-task "Child B" parent-id)))
    (ekg-org-view)
    (let ((headings (ekg-org-test--view-headings)))
      (should (equal headings
                     '((1 "Parent") (2 "Child A") (2 "Child B")))))))

(ekg-deftest-with-db ekg-org-test-view-shows-user-response ()
  "A task response is visible and remains a non-task note."
  (ekg-org-add-schema)
  (let* ((task-id (ekg-org-test--add-task "Question"))
         (response (ekg-note-create :text "My answer"
                                    :mode 'markdown-mode)))
    (ekg-note-set-parent response task-id)
    (ekg-save-note response)
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (should (string-match-p "My answer" (buffer-string)))
      (goto-char (point-min))
      (search-forward "My answer")
      (beginning-of-line)
      (should (equal (ekg-org-view--note-at-point)
                     (ekg-note-id response)))
      (should-error (ekg-org-view-cycle-state) :type 'user-error)
      (should-error (ekg-org-view-set-property "OWNER" "Someone")
                    :type 'user-error)
      (goto-char (point-min))
      (ekg-org-view-respond)
      (should ekg-capture-mode)
      (should (eq major-mode 'markdown-mode))
      (should (equal (ekg-note-parent-id ekg-note) task-id)))))

(ekg-deftest-with-db ekg-org-test-archive-view-keeps-response ()
  "Archived task subtrees still display their response notes."
  (ekg-org-add-schema)
  (let* ((task-id (ekg-org-test--add-task "Archived question"))
         (response (ekg-note-create :text "Archived answer"
                                    :mode 'markdown-mode)))
    (ekg-note-set-parent response task-id)
    (ekg-save-note response)
    (ekg-org-view--archive-note (ekg-get-note-with-id task-id))
    (ekg-org-archive-view)
    (with-current-buffer "*ekg-org-archive*"
      (should (string-match-p "Archived question" (buffer-string)))
      (should (string-match-p "Archived answer" (buffer-string))))))

(ekg-deftest-with-db ekg-org-test-view-default-directory ()
  "Test that task views use `ekg-notes-default-directory'."
  (let* ((ekg-notes-default-directory
          (file-name-directory (directory-file-name ekg-db-dir)))
         (expected-directory
          (file-name-as-directory
           (expand-file-name ekg-notes-default-directory))))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (should (equal default-directory expected-directory)))
    (ekg-org-view-task (ekg-org-test--add-task "Focused"))
    (should (equal default-directory expected-directory))))

(ekg-deftest-with-db ekg-org-test-view-multiple-top-level ()
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

(ekg-deftest-with-db ekg-org-test-view-collapse ()
  "Test that collapsing a parent hides its children."
  (ekg-org-add-schema)
  (let* ((parent-id (ekg-org-test--add-task "Parent"))
         (_child (ekg-org-test--add-task "Child" parent-id)))
    (ekg-org-view)
    (should (= (length (ekg-org-test--view-headings)) 2))
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (ekg-notes-toggle-collapse)
      (ekg-notes-toggle-collapse))
    (should (equal (ekg-org-test--view-headings)
                   '((1 "Parent"))))))

(ekg-deftest-with-db ekg-org-test-view-archive-removes-from-view ()
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

(ekg-deftest-with-db ekg-org-test-view-archive-appears-in-archive-view ()
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
          (when (ekg-org-view--on-heading-p)
            (let* ((nid (get-text-property (point) :ekg-note-id))
                   (note (ekg-get-note-with-id nid))
                   (title (car (plist-get (ekg-note-properties note)
                                          :titled/title))))
              (push title titles)))
          (forward-line 1))
        (should (member "Archived Task" titles))))))

(ekg-deftest-with-db ekg-org-test-view-promote ()
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

(ekg-deftest-with-db ekg-org-test-view-demote ()
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

(ekg-deftest-with-db ekg-org-test-view-move-up ()
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

(ekg-deftest-with-db ekg-org-test-view-move-down ()
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

(ekg-deftest-with-db ekg-org-test-view-reorder-after-warm-refresh ()
  "Reordering warmed VUI components keeps their rendered regions valid."
  (ekg-org-add-schema)
  (let ((first (ekg-org-test--add-task "First"))
        (second (ekg-org-test--add-task "Second with a longer title")))
    (save-window-excursion
      (ekg-org-view)
      (let ((root ekg-notes--instance))
        (dolist (move '(ekg-org-view-move-up ekg-org-view-move-down
                        ekg-org-view-move-up ekg-org-view-move-down))
          ;; The first refresh establishes VUI's incremental render record.
          (ekg-org-view-refresh)
          (ekg-org-view--goto-note-id second)
          (funcall move)
          (should vui-incremental-render)
          (should (eq ekg-notes--instance root))
          (should (equal (ekg-notes-visible-ids)
                         (if (eq move #'ekg-org-view-move-up)
                             (list second first)
                           (list first second))))
          (let ((rendered (buffer-string)))
            (with-temp-buffer
              (ekg-org-view-mode)
              (ekg-org-view--refresh)
              (should (equal-including-properties rendered (buffer-string))))))))))

(ekg-deftest-with-db ekg-org-test-view-move-up-at-top ()
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

(ekg-deftest-with-db ekg-org-test-view-move-down-at-bottom ()
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

(ekg-deftest-with-db ekg-org-test-view-state-change ()
  "Test that changing state updates the heading."
  (ekg-org-add-schema)
  (let ((id (ekg-org-test--add-task "My Task" nil "TODO")))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (should (equal (ekg-org-task-state (ekg-current-note-or-error))
                     "TODO")))
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
      (should (equal (ekg-org-task-state (ekg-current-note-or-error))
                     "DONE")))))

(ekg-deftest-with-db ekg-org-test-view-trash ()
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

(ekg-deftest-with-db ekg-org-test-view-trash-with-children ()
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

(ekg-deftest-with-db ekg-org-test-view-archive-with-children ()
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

(ekg-deftest-with-db ekg-org-test-view-navigation ()
  "Test heading navigation commands."
  (ekg-org-add-schema)
  (let* ((p1 (ekg-org-test--add-task "Parent 1"))
         (_c1 (ekg-org-test--add-task "Child 1" p1))
         (_p2 (ekg-org-test--add-task "Parent 2")))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      ;; At "Parent 1"
      (should (equal (ekg-org-view--level-at-point) 1))
      ;; Next heading → "Child 1"
      (ekg-org-view-next-heading)
      (should (equal (ekg-org-view--level-at-point) 2))
      ;; Up → back to "Parent 1"
      (ekg-org-view-up-heading)
      (should (equal (ekg-org-view--level-at-point) 1))
      (let* ((id (get-text-property (point) :ekg-note-id))
             (title (car (plist-get (ekg-note-properties
                                     (ekg-get-note-with-id id))
                                    :titled/title))))
        (should (equal title "Parent 1")))
      ;; Next sibling → "Parent 2"
      (ekg-org-view-next-sibling)
      (let* ((id (get-text-property (point) :ekg-note-id))
             (title (car (plist-get (ekg-note-properties
                                     (ekg-get-note-with-id id))
                                    :titled/title))))
        (should (equal title "Parent 2"))))))

(ekg-deftest-with-db ekg-org-test-view-deep-hierarchy ()
  "Test a three-level deep hierarchy renders correctly."
  (ekg-org-add-schema)
  (let* ((gp (ekg-org-test--add-task "Grandparent"))
         (p (ekg-org-test--add-task "Parent" gp))
         (_c (ekg-org-test--add-task "Child" p)))
    (ekg-org-view)
    (should (equal (ekg-org-test--view-headings)
                   '((1 "Grandparent") (2 "Parent") (3 "Child"))))))

(ekg-deftest-with-db ekg-org-test-view-promote-to-grandparent ()
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

(ekg-deftest-with-db ekg-org-test-view-auto-refresh-on-external-save ()
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

(ekg-deftest-with-db ekg-org-test-view-refresh-preserves-windows ()
  "Background insertions and edits retain independent points and viewports."
  (ekg-org-add-schema)
  (let* ((parent (ekg-org-test--add-task "Parent"))
         (child (ekg-org-test--add-task "Child" parent))
         (later (ekg-org-test--add-task "Later")))
    (save-window-excursion
      (delete-other-windows)
      (ekg-org-view)
      (let* ((view (current-buffer))
             (first (selected-window))
             (second (split-window-right)))
        (ekg-org-view--goto-note-id parent)
        (set-window-start first (point) t)
        (ekg-org-view--goto-note-id child)
        (forward-char 2)
        (set-window-point first (point))
        (let ((saved-point (point))
              (saved-start (window-start first)))
          (ekg-org-view--goto-note-id later)
          (set-window-start second (point) t)
          (set-window-point second (+ (point) 3))
          (goto-char saved-point)
          ;; Save from a different buffer, as an asynchronous callback does.
          (with-temp-buffer
            (ekg-org-test--add-task "New child" parent)
            (let ((note (ekg-get-note-with-id child)))
              (setf (ekg-note-text note) "New body\nWith more lines")
              (ekg-save-note note)))
          (should (= (window-start first) saved-start))
          (should (= (window-point first) saved-point))
          (with-current-buffer view
            (should (member "New child" (ekg-org-test--view-titles))))
          (with-current-buffer view
            (let ((later-start (text-property-any
                                (point-min) (point-max) :ekg-note-id later)))
              (should (= (window-start second) later-start))
              (should (= (window-point second) (+ later-start 3)))))
          (ekg-org-view--goto-note-id parent)
          (set-window-start first (point) t)
          (let ((start (window-start first)))
            (ekg-org-view-add-child "Interactive child")
            (should (= (window-start first) start))))))))

(ekg-deftest-with-db ekg-org-test-view-insertion-refreshes-other-views ()
  "Inhibited insertions update focused views through VUI's scheduler."
  (ekg-org-add-schema)
  (dolist (vui-render-delay '(nil 0.01))
    (let ((parent (ekg-org-test--add-task "Parent")))
      (save-window-excursion
        (ekg-org-view-task parent)
        (let ((focused (current-buffer)))
          (ekg-org-view)
          (ekg-org-view--goto-note-id parent)
          (ekg-org-view-add-child "New child")
          (with-current-buffer focused
            ;; The current view was flushed by the command; the other view's
            ;; state update is still queued on its own VUI root.
            (should (= (length (ekg-notes-visible-ids)) 1))
            (vui-flush-sync)
            (should (= (length (ekg-notes-visible-ids)) 2))
            (should (string-match-p "New child" (buffer-string)))))))))

(ekg-deftest-with-db ekg-org-test-view-auto-refresh-on-external-delete ()
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

(ekg-deftest-with-db ekg-org-test-view-auto-refresh-on-external-state-change ()
  "Test that the view updates when a note's state changes externally."
  (ekg-org-add-schema)
  (let ((id (ekg-org-test--add-task "My Task" nil "TODO")))
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (goto-char (point-min))
      (should (equal (ekg-org-task-state (ekg-current-note-or-error))
                     "TODO")))
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
      (should (equal (ekg-org-task-state (ekg-current-note-or-error))
                     "DONE")))))

(ekg-deftest-with-db ekg-org-test-view-refile-to-new-parent ()
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

(ekg-deftest-with-db ekg-org-test-view-refile-to-top-level ()
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

(ekg-deftest-with-db ekg-org-test-view-refile-excludes-descendants ()
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

(ekg-deftest-with-db ekg-org-test-properties ()
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

(ekg-deftest-with-db ekg-org-test-scalar-title-and-string-id-sorting ()
  "Handle scalar titles and string note IDs safely in Org export and sorting."
  (ekg-org-add-schema)
  (let ((note1 (ekg-note-create :id "str-1" :text "Body 1"
                                :properties '(:titled/title "Scalar Title 1" :org/sort-order 0)))
        (note2 (ekg-note-create :id "str-2" :text "Body 2"
                                :properties '(:titled/title "Scalar Title 2" :org/sort-order 0))))
    (should (string= (ekg-org--note-title note1) "Scalar Title 1"))
    (should (ekg-org-view--sort-predicate note1 note2))
    (should-not (ekg-org-view--sort-predicate note2 note1))))

(ekg-deftest-with-db ekg-org-test-task-summary-custom-states ()
  "Summaries honor custom done states and ignore archived task children."
  (ekg-org-add-schema)
  (let* ((org-todo-keywords '((sequence "TODO" "WAIT(w)" "|" "DONE(d)"
                                      "CANCELLED(c)")))
         (parent (ekg-org-test--add-task "Parent"))
         (_open (ekg-org-test--add-task "Open" parent "WAIT"))
         (_closed (ekg-org-test--add-task "Closed" parent "CANCELLED"))
         (archived (ekg-org-test--add-task "Archived" parent "TODO"))
         (note (ekg-get-note-with-id archived)))
    (push ekg-org-archive-tag (ekg-note-tags note))
    (ekg-save-note note)
    (let ((summary (ekg-org-task-summary (ekg-get-note-with-id parent))))
      (should (equal (plist-get summary :state) "TODO"))
      (should (= (plist-get summary :children) 2))
      (should (= (plist-get summary :completed) 1))
      (should (= (plist-get summary :remaining) 1))
      (should-not (plist-get summary :done)))))

(ekg-deftest-with-db ekg-org-test-notes-view-scope-and-folding ()
  "Task views fold independently and preserve selection across refreshes."
  (ekg-org-add-schema)
  (let* ((parent (ekg-org-test--add-task "Parent"))
         (child (ekg-org-test--add-task "Child" parent))
         (ordinary (ekg-note-create :text "Ordinary note" :mode 'text-mode)))
    (ekg-save-note ordinary)
    (let ((keys (copy-keymap ekg-notes-mode-map))
          (template ekg-display-note-template)
          (hidden (copy-sequence ekg-hidden-tags)))
      (ekg-org-view)
      (should (derived-mode-p 'ekg-notes-mode))
      (should (equal (plist-get (ekg-org-view-state) :visible-ids)
                     (list parent child)))
      (ekg-notes-toggle-collapse)
      (ekg-notes-toggle-collapse)
      (should (equal (plist-get (ekg-org-view-state) :visible-ids)
                     (list parent)))
      (should (equal (plist-get (ekg-org-view-state) :folded-ids)
                     (list parent)))
      ;; An external save must not unfold the tree or switch action targets.
      (ekg-save-note (ekg-get-note-with-id child))
      (should (equal (plist-get (ekg-org-view-state) :current-id) parent))
      (should (equal (ekg-notes-visible-ids) (list parent)))
      (ekg-notes-toggle-collapse)
      (ekg-org-view-next-heading)
      (should (equal (ekg-note-id (ekg-current-note-or-error)) child))
      (let ((note (ekg-get-note-with-id parent)))
        (setf (ekg-note-text note) "Changed elsewhere")
        (ekg-save-note note))
      (should (equal (ekg-note-id (ekg-current-note-or-error)) child))
      (with-temp-buffer
        (ekg-notes-mode)
        (should (equal ekg-notes-mode-map keys))
        (should (equal ekg-display-note-template template))
        (should (equal ekg-hidden-tags hidden))
        (should-not ekg-notes-display-states)
        (should (eq ekg-notes-children-function #'ekg--notes-children))
        (should ekg-notes-show-child-count)))))

(ekg-deftest-with-db ekg-org-test-notes-view-toggle-done-and-add-child ()
  "Task actions persist state and unfold a parent when adding a child."
  (ekg-org-add-schema)
  (let* ((org-todo-keywords '((sequence "TODO" "WAIT" "|" "DONE")))
         (parent (ekg-org-test--add-task "Parent"))
         (child (ekg-org-test--add-task "Child" parent)))
    (ekg-org-view)
    (ekg-org-view-next-heading)
    (ekg-org-view-toggle-done)
    (should (equal (ekg-note-id (ekg-current-note-or-error)) child))
    (should (ekg-org-task-done-p (ekg-get-note-with-id child)))
    (ekg-org-view-toggle-done)
    (should (equal (ekg-org-task-state (ekg-get-note-with-id child)) "TODO"))
    (should-error (ekg-org-change-state "INVALID") :type 'user-error)
    (ekg-org-view-up-heading)
    (ekg-notes-toggle-collapse)
    (ekg-notes-toggle-collapse)
    (ekg-org-view-add-child "New child")
    (should-not (member parent (plist-get (ekg-org-view-state) :folded-ids)))
    (should (equal (ekg-note-parent-id (ekg-current-note-or-error)) parent))
    (should (= (length (ekg-notes-visible-ids)) 3))))

(ekg-deftest-with-db ekg-org-test-view-independent-of-strict-notes ()
  "The task hierarchy is available with strict ordinary note lists."
  (ekg-org-add-schema)
  (let* ((ekg-notes-include-descendants nil)
         (parent (ekg-org-test--add-task "Parent"))
         (child (ekg-org-test--add-task "Child" parent)))
    (ekg-org-view)
    (should (equal (ekg-notes-visible-ids) (list parent child)))))

(ekg-deftest-with-db ekg-org-test-view-insert-under-folded-parent ()
  "Inserting into a folded tree selects the new task for subsequent actions."
  (ekg-org-add-schema)
  (let ((parent (ekg-org-test--add-task "Parent")))
    (ekg-org-view)
    (ekg-notes-toggle-collapse)
    (ekg-notes-toggle-collapse)
    (ekg-org-view-add-child "Child")
    (let ((note (ekg-current-note-or-error)))
      (should (equal (ekg-note-parent-id note) parent))
      (should (equal (ekg-org--note-title note) "Child")))))

(ekg-deftest-with-db ekg-org-test-view-demote-under-folded-parent ()
  "Demoting into a folded tree preserves the action target."
  (ekg-org-add-schema)
  (let* ((parent (ekg-org-test--add-task "Parent"))
         (task (ekg-org-test--add-task "Task")))
    (ekg-org-view)
    (ekg-notes-toggle-collapse)
    (ekg-notes-toggle-collapse)
    (ekg-org-view-next-heading)
    (ekg-org-view-demote)
    (should (equal (ekg-note-parent-id (ekg-get-note-with-id task)) parent))
    (should (equal (ekg-note-id (ekg-current-note-or-error)) task))))

(ekg-deftest-with-db ekg-org-test-view-fold-all-selects-ancestor ()
  "Folding all tasks selects the current task's own visible ancestor."
  (ekg-org-add-schema)
  (let* ((_first (ekg-org-test--add-task "First"))
         (parent (ekg-org-test--add-task "Second"))
         (child (ekg-org-test--add-task "Child" parent)))
    (ekg-org-view)
    (ekg-org-view--goto-note-id child)
    (ekg-org-view-toggle-all)
    (should (equal (ekg-note-id (ekg-current-note-or-error)) parent))))

(ekg-deftest-with-db ekg-org-test-view-archive-rejects-creation ()
  "Archived views cannot create active tasks under archived roots."
  (ekg-org-add-schema)
  (let* ((parent (ekg-org-test--add-task "Parent"))
         (note (ekg-get-note-with-id parent)))
    (push ekg-org-archive-tag (ekg-note-tags note))
    (ekg-save-note note)
    (ekg-org-archive-view)
    (should-error (ekg-org-view-add-child "Child") :type 'user-error)
    (should-error (ekg-notes-create) :type 'user-error)
    (should-error (ekg-notes-respond))
    (should-not (ekg-note-child-notes (ekg-get-note-with-id parent)))
    (should-not (ekg-org-get-tasks))))

(ekg-deftest-with-db ekg-org-test-view-create-custom-state ()
  "New children use the first configured unfinished task state."
  (ekg-org-add-schema)
  (let* ((org-todo-keywords '((sequence "OPEN(o)" "|" "CLOSED(c)")))
         (parent (ekg-org-test--add-task "Parent" nil "OPEN")))
    (ekg-org-view)
    (ekg-org-view-add-child "Child")
    (should (equal (ekg-note-parent-id (ekg-current-note-or-error)) parent))
    (should (equal (ekg-org-task-state (ekg-current-note-or-error)) "OPEN"))
    (let ((org-todo-keywords '((sequence "|" "CLOSED"))))
      (should-error (ekg-org-view-add-child "Invalid") :type 'user-error))
    (should (= 1 (length (ekg-note-child-notes
                         (ekg-get-note-with-id parent)))))))

(ekg-deftest-with-db ekg-org-test-focused-view-archive-scope ()
  "Archiving a focused root removes it and prevents active child creation."
  (ekg-org-add-schema)
  (let ((parent (ekg-org-test--add-task "Parent")))
    (ekg-org-view-task parent)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (ekg-org-view-archive))
    (should-not (ekg-notes-visible-ids))
    (should-error (ekg-notes-respond))
    (should-not (ekg-note-child-notes (ekg-get-note-with-id parent)))))

(ekg-deftest-with-db ekg-org-test-toggle-done-multiple-sequences ()
  "Completion and reopening remain within the task's keyword sequence."
  (ekg-org-add-schema)
  (let* ((org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                             (sequence "OPEN(o)" "WAIT(w)" "|"
                                       "CLOSED(c)" "CANCELLED(a)")))
         (task (ekg-org-test--add-task "Task" nil "OPEN")))
    (ekg-org-view)
    (dolist (states '(("OPEN" . "CLOSED") ("CLOSED" . "OPEN")
                      ("WAIT" . "CLOSED") ("CANCELLED" . "OPEN")
                      ("TODO" . "DONE") ("DONE" . "TODO")))
      (ekg-org-change-state (car states))
      (ekg-org-view-toggle-done)
      (should (equal (ekg-org-task-state (ekg-get-note-with-id task))
                     (cdr states))))
    ;; A removed configuration must not silently move the task to another
    ;; workflow or persist a new state.
    (let ((org-todo-keywords '((sequence "OPEN" "|" "CLOSED"))))
      (should-error (ekg-org-view-toggle-done) :type 'user-error))
    (should (equal (ekg-org-task-state (ekg-get-note-with-id task)) "TODO"))))

(ekg-deftest-with-db ekg-org-test-hidden-tasks-remain-available-to-org ()
  "Hide task notes in ordinary queries while retaining task integration."
  (ekg-org-add-schema)
  (should (member ekg-org-task-tag ekg-hidden-tags))
  (let* ((parent (ekg-org-test--add-task "Parent"))
         (child (ekg-org-test--add-task "Child" parent))
         (ordinary (ekg-note-create :text "Ordinary" :tags '("project"))))
    (dolist (id (list parent child))
      (let ((note (ekg-get-note-with-id id)))
        (push "project" (ekg-note-tags note))
        (ekg-save-note note)))
    (ekg-save-note ordinary)
    (should (equal (mapcar #'ekg-note-id (ekg-get-notes-with-tag "project"))
                   (list (ekg-note-id ordinary))))
    (should (equal (mapcar #'ekg-note-id
                          (ekg-get-notes-with-tags '("project")))
                   (list (ekg-note-id ordinary))))
    (should (equal (sort (mapcar #'ekg-note-id
                                (ekg-get-notes-with-tag ekg-org-task-tag)) #'<)
                   (sort (list parent child) #'<)))
    (ekg-show-notes-with-tag ekg-org-task-tag)
    (should (equal (ekg-notes-visible-ids) (list parent child)))
    (should (equal (mapcar #'ekg-note-id (ekg-org-get-tasks)) (list parent)))
    (should (equal (mapcar #'ekg-note-id (ekg-org-get-child-tasks-of-id parent))
                   (list child)))
    (should (string-match-p "Child" (ekg-org-generate-org-content)))
    (ekg-org-view-task parent)
    (should (equal (ekg-notes-visible-ids) (list parent child)))))

(ekg-deftest-with-db ekg-org-test-hidden-tasks-respect-other-hidden-tags ()
  "The Org integration bypasses only its task tag."
  (ekg-org-add-schema)
  (let* ((ekg-hidden-tags (cons "private" ekg-hidden-tags))
         (parent (ekg-org-test--add-task "Parent")))
    (dolist (tag (list ekg-trash-tag ekg-draft-tag "private"))
      (let ((root (ekg-org-test--add-task tag))
            (child (ekg-org-test--add-task tag parent)))
        (dolist (id (list root child))
          (let ((note (ekg-get-note-with-id id)))
            (push tag (ekg-note-tags note))
            (ekg-save-note note)))))
    (should (equal (mapcar #'ekg-note-id (ekg-org-get-tasks)) (list parent)))
    (should-not (ekg-org-get-child-notes-of-id parent))
    (should (= 0 (plist-get (ekg-org-task-summary
                            (ekg-get-note-with-id parent)) :children)))))

(ekg-deftest-with-db ekg-org-test-working-directory-inheritance ()
  "DIR survives saving and is inherited until a child overrides it."
  (ekg-org-add-schema)
  (let* ((directory (make-temp-file "ekg-task-dir-" t))
         (child-directory (expand-file-name "child" directory))
         (parent (ekg-note-create :text "Parent" :mode 'org-mode))
         (child (ekg-note-create :text "Child" :mode 'org-mode)))
    (unwind-protect
        (progn
          (make-directory child-directory)
          (should-not (ekg-org-working-directory parent))
          (ekg-org-set-property parent "dir" directory)
          (ekg-save-note parent)
          (ekg-note-set-parent child (ekg-note-id parent))
          (ekg-save-note child)
          (setq child (ekg-get-note-with-id (ekg-note-id child)))
          (should (equal (ekg-org-working-directory child)
                         (file-name-as-directory directory)))
          (let ((default-directory (file-name-as-directory directory)))
            (ekg-org-set-property child "DIR" "child")
            (should (equal (ekg-org-working-directory child)
                           (file-name-as-directory child-directory))))
          (ekg-org-remove-property child "DIR")
          (should (equal (ekg-org-working-directory child)
                         (file-name-as-directory directory))))
      (delete-directory directory t))))

(ekg-deftest ekg-org-test-working-directory-invalid ()
  "Invalid DIR properties fail instead of running in another directory."
  (let* ((directory (make-temp-file "ekg-task-dir-" t))
         (note (ekg-note-create :text "Task")))
    (unwind-protect
        (dolist (value (list "" "   " (expand-file-name "missing" directory)))
          (ekg-org-set-property note "DIR" value)
          (should-error (ekg-org-working-directory note) :type 'user-error))
      (delete-directory directory))))

(ekg-deftest-with-db ekg-org-test-view-set-property-interactively ()
  "The property command prompts, saves, and preserves other properties."
  (ekg-org-add-schema)
  (let* ((id (ekg-org-test--add-task "Task"))
         (note (ekg-get-note-with-id id)))
    (ekg-org-set-property note "OWNER" "Original owner")
    (ekg-org-set-property note "CATEGORY" "work")
    (ekg-save-note note)
    (ekg-org-view)
    (with-current-buffer "*ekg-org-tasks*"
      (ekg-org-view--goto-note-id id)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (should (member "OWNER" choices))
                   "owner"))
                ((symbol-function 'read-string)
                 (lambda (_prompt initial &rest _)
                   (should (equal initial "Original owner"))
                   "New owner")))
        (call-interactively #'ekg-org-view-set-property))
      (should (equal (ekg-org-view--note-at-point) id))
      (setq note (ekg-get-note-with-id id))
      (should (equal (ekg-org-get-property note "OWNER") "New owner"))
      (should (equal (ekg-org-get-property note "CATEGORY") "work")))))

(ekg-deftest-with-db ekg-org-test-view-set-directory-interactively ()
  "DIR uses directory completion; removing it restores inheritance."
  (ekg-org-add-schema)
  (let* ((directory (make-temp-file "ekg-property-dir-" t))
         (override (expand-file-name "child" directory))
         (parent-id (ekg-org-test--add-task "Parent"))
         (id (ekg-org-test--add-task "Child" parent-id))
         (parent (ekg-get-note-with-id parent-id)))
    (unwind-protect
        (progn
          (make-directory override)
          (ekg-org-set-property parent "DIR" directory)
          (ekg-save-note parent)
          (ekg-org-view)
          (with-current-buffer "*ekg-org-tasks*"
            (ekg-org-view--goto-note-id id)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "DIR"))
                      ((symbol-function 'read-directory-name)
                       (lambda (_prompt _dir _default mustmatch &rest _)
                         (should mustmatch)
                         override)))
              (call-interactively #'ekg-org-view-set-property))
            (should (equal (ekg-org-get-property
                            (ekg-get-note-with-id id) "DIR")
                           (file-name-as-directory override)))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices _predicate mustmatch &rest _)
                         (should (equal choices '("DIR")))
                         (should mustmatch)
                         "DIR"))
                      ((symbol-function 'read-string)
                       (lambda (&rest _) (ert-fail "Unexpected value prompt")))
                      ((symbol-function 'read-directory-name)
                       (lambda (&rest _)
                         (ert-fail "Unexpected directory prompt"))))
              (let ((current-prefix-arg '(4)))
                (call-interactively #'ekg-org-view-set-property)))
            (let ((note (ekg-get-note-with-id id)))
              (should-not (ekg-org-get-property note "DIR"))
              (should (equal (ekg-org-working-directory note)
                             (file-name-as-directory directory))))))
      (delete-directory directory t))))

(ekg-deftest-with-db ekg-org-test-view-set-property-invalid ()
  "Invalid property names and directories leave the saved task unchanged."
  (ekg-org-add-schema)
  (let ((id (ekg-org-test--add-task "Task"))
        (directory (make-temp-file "ekg-property-dir-" t)))
    (unwind-protect
        (progn
          (ekg-org-view)
          (with-current-buffer "*ekg-org-tasks*"
            (ekg-org-view--goto-note-id id)
            (dolist (property '("" "bad key" "bad:key" "EKG_ID"))
              (should-error (ekg-org-view-set-property property "value")
                            :type 'user-error))
            (should-error (ekg-org-view-set-property
                           "DIR" (expand-file-name "missing" directory))
                          :type 'user-error)
            (should-not (ekg-org-properties-alist (ekg-get-note-with-id id)))))
      (delete-directory directory))))

(provide 'ekg-org-test)
