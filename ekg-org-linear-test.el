;;; ekg-org-linear-test.el --- Linear sync tests -*- lexical-binding: t -*-
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
;; Offline integration tests with mocked Linear requests.
;;; Code:
(require 'ekg-org-linear)
(require 'ekg-test-utils)

(defun ekg-org-linear-test--issue (id title &optional parent)
  "Make a Linear issue with ID, TITLE and PARENT."
  (copy-tree `((id . ,id) (identifier . ,id) (title . ,title) (description . "Body")
               (url . ,(format "https://linear.app/issue/%s" id))
               (team . ((id . "team"))) (state . ((id . "todo") (type . "unstarted")))
               (parent . ,(when parent `((id . ,parent)))))))

(ekg-deftest ekg-org-linear-test-merge ()
             (should (equal (ekg-org-linear--merge '("Local" "Body" "TODO")
                                                   '("Title" "Remote" "TODO")
                                                   '("Title" "Body" "TODO"))
                            '("Local" "Remote" "TODO")))
             (let* ((local (concat "Doc https://docs.google.com/document/d/id/edit"
                                   "\nPR https://github.com/soundcloud/mono/pull/4121"
                                   "\nIssue CORBE-500"))
                    (remote (concat "Doc [https://docs.google.com/document/d/id/edit]"
                                    "(<https://docs.google.com/document/d/id/edit>)"
                                    "\nPR [soundcloud/mono#4121]"
                                    "(https://linear.app/soundcloud/review/pr-4121)"
                                    "\nIssue [CORBE-500]"
                                    "(https://linear.app/soundcloud/issue/CORBE-500/slug)")))
               (should (equal (ekg-org-linear--normalize-markdown-links local)
                              (ekg-org-linear--normalize-markdown-links remote)))
               (should (equal (ekg-org-linear--merge
                               (list "Title" local "TODO")
                               (list "Title" remote "TODO")
                               '("Title" "Before edit" "TODO"))
                              (list "Title" remote "TODO")))
               (should-error (ekg-org-linear--merge
                              '("Title" "[link](https://example.com/local)" "TODO")
                              '("Title" "[link](https://example.com/remote)" "TODO")
                              '("Title" "Before edit" "TODO"))))
             (should-error (ekg-org-linear--merge '("Local" "" "TODO")
                                                  '("Remote" "" "TODO")
                                                  '("Base" "" "TODO"))))

(ekg-deftest ekg-org-linear-test-conflict-report ()
  (let ((buffer-name ekg-org-linear--conflict-buffer-name))
    (unwind-protect
        (let* ((issue (ekg-org-linear-test--issue "ENG-123" "Issue title"))
               (err (should-error
                     (ekg-org-linear--merge '("Title" "local body" "TODO")
                                            '("Title" "remote body" "TODO")
                                            '("Title" "base body" "TODO")
                                            issue)))
               (message (error-message-string err)))
          (should (string-match-p "Description.*ENG-123" message))
          (should-not (string-match-p "local body\\|remote body" message))
          (with-current-buffer buffer-name
            (should (string-match-p "Issue: ENG-123 — Issue title"
                                    (buffer-string)))
            (should (string-match-p "Field: Description" (buffer-string)))
            (should (string-match-p "local body" (buffer-string)))
            (should (string-match-p "remote body" (buffer-string)))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ekg-deftest ekg-org-linear-test-duplicate-state ()
             (should (equal "DONE"
                            (nth 2
                                 (ekg-org-linear--remote-values
                                  `((id . "i") (identifier . "I-1")
                                    (title . "Title") (description . "")
                                    (team . ((id . "team")))
                                    (state . ((id . "d") (type . "duplicate")))))))))

(ekg-deftest ekg-org-linear-test-state-map-extras ()
             (should (equal "TRIAGE" (cdr (assoc "triage" ekg-org-linear-state-map))))
             (should (equal "STARTED" (cdr (assoc "started" ekg-org-linear-state-map)))))

(ekg-deftest ekg-org-linear-test-remote-state-with-assignee ()
             (let ((ekg-org-linear-assignee "me")
                   (ekg-org-linear-unassigned-state "TRIAGE")
                   (ekg-org-linear-other-assignee-state "DELEGATED")
                   (issue (lambda (assignee)
                            `((id . "i") (identifier . "I-1")
                              (title . "Title") (description . "")
                              (team . ((id . "team")))
                              (state . ((id . "s") (type . "started")))
                              (assignee . ,assignee)))))
               (cl-letf (((symbol-function 'ekg-org-linear--viewer-id)
                          (lambda () "me")))
                 ;; Assigned to the configured assignee uses the workflow state.
                 (should (equal "STARTED"
                                (nth 2 (ekg-org-linear--remote-values
                                        (funcall issue '((id . "me")
                                                         (email . "me@x.com")))))))
                 ;; Unassigned issues use `ekg-org-linear-unassigned-state'.
                 (should (equal "TRIAGE"
                                (nth 2 (ekg-org-linear--remote-values
                                        (funcall issue nil)))))
                 ;; Issues assigned to someone else use the other-assignee state.
                 (should (equal "DELEGATED"
                                (nth 2 (ekg-org-linear--remote-values
                                        (funcall issue '((id . "other")
                                                         (email . "o@x.com")))))))
                 ;; Without an option, the workflow state is used regardless.
                 (let ((ekg-org-linear-unassigned-state nil)
                       (ekg-org-linear-other-assignee-state nil))
                   (should (equal "STARTED"
                                  (nth 2 (ekg-org-linear--remote-values
                                          (funcall issue nil)))))
                   (should (equal "STARTED"
                                  (nth 2 (ekg-org-linear--remote-values
                                          (funcall issue '((id . "other")
                                                           (email . "o@x.com")))))))))))

(ekg-deftest ekg-org-linear-test-assignee-matches ()
             (let ((ekg-org-linear-assignee "a@b.com"))
               (should (ekg-org-linear--issue-assignee-matches-p
                        '((assignee . ((id . "x") (email . "a@b.com"))))))
               (should-not (ekg-org-linear--issue-assignee-matches-p
                            '((assignee . ((id . "x") (email . "c@d.com"))))))
               (should-not (ekg-org-linear--issue-assignee-matches-p
                            '((assignee . nil))))
               (should (ekg-org-linear--issue-has-other-owner-p
                        '((assignee . ((id . "x") (email . "c@d.com"))))))
               (should-not (ekg-org-linear--issue-has-other-owner-p
                            '((assignee . nil))))
               (let ((ekg-org-linear-assignee nil))
                 (should (ekg-org-linear--issue-assignee-matches-p
                          '((assignee . ((id . "x") (email . "c@d.com")))))))))

(ekg-deftest ekg-org-linear-test-issue-filter ()
             (let ((ekg-org-linear-import-team-id "team")
                   (ekg-org-linear-assignee "me"))
               (should (equal (ekg-org-linear--issue-filter)
                              '((assignee . ((isMe . ((eq . t)))))
                                (team . ((id . ((eq . "team")))))))))
             (let ((ekg-org-linear-import-team-id nil)
                   (ekg-org-linear-assignee "a@b.com"))
               (should (equal (ekg-org-linear--issue-filter)
                              '((assignee . ((email . ((eq . "a@b.com")))))))))
             (let ((ekg-org-linear-import-team-id nil)
                   (ekg-org-linear-assignee nil))
               (should-error (ekg-org-linear--issue-filter)))
             (let ((ekg-org-linear-import-team-id "team")
                   (ekg-org-linear-assignee "me")
                   (ekg-org-linear-unassigned-state "TRIAGE"))
               (should (equal (ekg-org-linear--issue-filter)
                              '((or . ((assignee . ((isMe . ((eq . t)))))
                                       (assignee . ((null . t)))))
                                (team . ((id . ((eq . "team"))))))))))

(ekg-deftest ekg-org-linear-test-org-only-state ()
             (let ((issue (ekg-org-linear-test--issue "i" "Title")))
               (should-not (ekg-org-linear--state-id issue "DELEGATED"))
               ;; Org-only states survive a merge when the remote cannot express them.
               (should (equal (ekg-org-linear--merge '("T" "D" "DELEGATED")
                                                     '("T" "D" "TODO")
                                                     '("T" "D" "DELEGATED"))
                              '("T" "D" "DELEGATED")))
               (should (equal (ekg-org-linear--merge '("T" "D" "DELEGATED")
                                                     '("T" "D" "TODO")
                                                     '("T" "D" "TODO"))
                              '("T" "D" "DELEGATED")))))

(ekg-deftest ekg-org-linear-test-update-omits-org-only-state ()
             (let ((issue (ekg-org-linear-test--issue "i" "Title")) sent)
               (cl-letf (((symbol-function 'ekg-org-linear--request)
                          (lambda (_query variables)
                            (setq sent (alist-get 'input variables))
                            '((issueUpdate . ((success . t)))))))
                 (ekg-org-linear--update issue '("Title" "Body" "DELEGATED"))
                 (should-not (assq 'stateId sent))
                 (should (equal "Title" (alist-get 'title sent)))
                 (ekg-org-linear--update issue '("Title" "Body" "TODO"))
                 (should (equal "todo" (alist-get 'stateId sent))))))

(ekg-deftest-with-db ekg-org-linear-test-sync-tree-and-edits ()
                     (let* ((parent (ekg-org-linear-test--issue "p" "Parent"))
                            (child (ekg-org-linear-test--issue "c" "Child" "p"))
                            (remote (list child parent)) updates)
                       (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote))
                                 ((symbol-function 'ekg-org-linear--update)
                                  (lambda (issue values)
                                    (push values updates)
                                    (setf (alist-get 'title issue) (car values)
                                          (alist-get 'description issue) (nth 1 values)
                                          (alist-get 'state issue)
                                          '((id . "done") (type . "completed"))))))
                         (ekg-org-linear-sync)
                         (let* ((notes (ekg-org-linear--linked-notes))
                                (p (gethash "p" notes)) (c (gethash "c" notes)))
                           (should (= 2 (hash-table-count notes)))
                           (should (equal (ekg-note-id p) "https://linear.app/issue/p"))
                           (should (equal (ekg-note-id c) "https://linear.app/issue/c"))
                           (should (equal (ekg-note-parent-id c) (ekg-note-id p)))
                           (should (eq 'parallel (ekg-org-dependency-type p)))
                           (should (equal "TODO" (ekg-org-task-state c)))
                           (setf (ekg-note-text c) "Local body"
                                 (ekg-note-tags c) '("org/task" "org/state/done" "keep"))
                           (ekg-save-note c)
                           (setf (alist-get 'title child) "Remote title")
                           (ekg-org-linear-sync)
                           (setq c (gethash "c" (ekg-org-linear--linked-notes)))
                           (should (equal (car updates) '("Remote title" "Local body" "DONE")))
                           (should (equal (ekg-org--note-title c) "Remote title"))
                           (should (member "keep" (ekg-note-tags c)))
                           (setq updates nil)
                           (ekg-org-linear-sync)
                           (should-not updates)
                           (should (= 2 (hash-table-count (ekg-org-linear--linked-notes))))
                           ;; Reparenting to the root is imported too.
                           (setf (alist-get 'parent child) nil)
                           (ekg-org-linear-sync)
                           (should-not (ekg-note-parent-id
                                        (gethash "c" (ekg-org-linear--linked-notes))))))))

(ekg-deftest-with-db ekg-org-linear-test-delegated-sync ()
  (let* ((issue (ekg-org-linear-test--issue "i" "Title"))
         (remote (list issue)) updates warnings)
    (setf (alist-get 'assignee issue) '((id . "me") (email . "me@x.com")))
    (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote))
              ((symbol-function 'ekg-org-linear--viewer-id) (lambda () "me"))
              ((symbol-function 'ekg-org-linear--update)
               (lambda (_issue values) (push values updates)))
              ((symbol-function 'warn)
               (lambda (&rest args) (push args warnings))))
      (ekg-org-linear-sync)
      (let ((note (gethash "i" (ekg-org-linear--linked-notes))))
        (should (equal "TODO" (ekg-org-task-state note)))
        (setf (ekg-note-tags note)
              (cons "org/state/delegated"
                    (seq-remove (lambda (tag) (string-prefix-p "org/state/" tag))
                                (ekg-note-tags note))))
        (ekg-save-note note))
      (ekg-org-linear-sync)
      (let ((note (gethash "i" (ekg-org-linear--linked-notes))))
        (should (equal "DELEGATED" (ekg-org-task-state note))))
      (should (equal '("Title" "Body" "DELEGATED") (car updates)))
      (should warnings)
      ;; A later sync keeps the Org-only state instead of reverting to TODO.
      (setq updates nil warnings nil)
      (ekg-org-linear-sync)
      (should (equal "DELEGATED"
                     (ekg-org-task-state
                      (gethash "i" (ekg-org-linear--linked-notes)))))
      (should (equal '("Title" "Body" "DELEGATED") (car updates)))
      (should warnings))))

(ekg-deftest-with-db ekg-org-linear-test-triage-import ()
  (let* ((issue (ekg-org-linear-test--issue "i" "Title"))
         (remote (list issue)))
    (setf (alist-get 'assignee issue) nil)
    (let ((ekg-org-linear-assignee "me")
          (ekg-org-linear-unassigned-state "TRIAGE"))
      (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote))
                ((symbol-function 'ekg-org-linear--viewer-id) (lambda () "me")))
        (ekg-org-linear-sync)
        (let ((note (gethash "i" (ekg-org-linear--linked-notes))))
          (should (equal "TRIAGE" (ekg-org-task-state note)))
          (should (member "org/state/triage" (ekg-note-tags note))))
        ;; Nothing to push for an unassigned issue imported as TRIAGE.
        (cl-letf (((symbol-function 'ekg-org-linear--update)
                   (lambda (&rest _) (ert-fail "No update for unassigned import"))))
          (ekg-org-linear-sync))))))

(ekg-deftest-with-db ekg-org-linear-test-other-children-sync ()
  (let* ((parent (ekg-org-linear-test--issue "p" "Parent"))
         (child (ekg-org-linear-test--issue "c" "Child" "p"))
         (grand (ekg-org-linear-test--issue "g" "Grand" "c"))
         (remote (list parent)) updates warnings)
    (setf (alist-get 'assignee parent) '((id . "me") (email . "me@x.com"))
          (alist-get 'assignee child) '((id . "bob") (email . "bob@x.com"))
          (alist-get 'assignee grand) '((id . "carol") (email . "carol@x.com")))
    (let ((ekg-org-linear-assignee "me")
          (ekg-org-linear-other-assignee-state "DELEGATED"))
      (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote))
                ((symbol-function 'ekg-org-linear--viewer-id) (lambda () "me"))
                ((symbol-function 'ekg-org-linear--paged-issues)
                 (lambda (filter)
                   (cond ((equal "p" (alist-get 'eq (alist-get 'id (alist-get 'parent filter))))
                          (list child))
                         ((equal "c" (alist-get 'eq (alist-get 'id (alist-get 'parent filter))))
                          (list grand))
                         (t nil))))
                ((symbol-function 'ekg-org-linear--update)
                 (lambda (_issue values) (push values updates)))
                ((symbol-function 'warn)
                 (lambda (&rest args) (push args warnings))))
        (ekg-org-linear-sync)
        (let* ((notes (ekg-org-linear--linked-notes))
               (n-parent (gethash "p" notes))
               (n-child (gethash "c" notes))
               (n-grand (gethash "g" notes)))
          (should (= 3 (hash-table-count notes)))
          (should n-child)
          (should n-grand)
          (should (equal "DELEGATED" (ekg-org-task-state n-child)))
          (should (equal "DELEGATED" (ekg-org-task-state n-grand)))
          (should (equal (ekg-note-id n-parent) (ekg-note-parent-id n-child)))
          (should (equal (ekg-note-id n-child) (ekg-note-parent-id n-grand))))
        ;; No state is pushed for the imported DELEGATED issues, and since
        ;; they are owned by someone else, there is no delegation warning.
        (should-not updates)
        (should-not warnings)))))

(ekg-deftest-with-db ekg-org-linear-test-child-fetch-ignores-unassigned ()
  (let* ((parent (ekg-org-linear-test--issue "p" "Parent"))
         (child (ekg-org-linear-test--issue "c" "Child" "p"))
         (remote (list parent)))
    (setf (alist-get 'assignee parent) '((id . "me") (email . "me@x.com"))
          (alist-get 'assignee child) nil)
    (let ((ekg-org-linear-assignee "me")
          (ekg-org-linear-other-assignee-state "DELEGATED"))
      (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote))
                ((symbol-function 'ekg-org-linear--viewer-id) (lambda () "me"))
                ((symbol-function 'ekg-org-linear--paged-issues)
                 (lambda (filter) (if (assq 'parent filter) (list child) nil))))
        (ekg-org-linear-sync)
        ;; The child has no assignee, so only `ekg-org-linear-unassigned-state'
        ;; could import it; this option does not.
        (should-not (gethash "c" (ekg-org-linear--linked-notes)))))))

(ekg-deftest-with-db ekg-org-linear-test-unassigned-state-validation ()
  (let ((ekg-org-linear-assignee "me")
        (ekg-org-linear-unassigned-state "BOGUS"))
    (should-error (ekg-org-linear-sync) :type 'user-error)))

(ekg-deftest-with-db ekg-org-linear-test-other-assignee-validation ()
  (let ((ekg-org-linear-assignee "me")
        (ekg-org-linear-other-assignee-state "BOGUS"))
    (should-error (ekg-org-linear-sync) :type 'user-error)))

(ekg-deftest-with-db ekg-org-linear-test-import-marker-and-prune ()
  (let* ((issue (ekg-org-linear-test--issue "i" "Title"))
         (remote (list issue)) updates)
    (setf (alist-get 'assignee issue) '((id . "me") (email . "me@x.com")))
    (let ((ekg-org-linear-assignee "me"))
      (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote))
                ((symbol-function 'ekg-org-linear--update)
                 (lambda (_issue values) (push values updates))))
        (ekg-org-linear-sync)
        (let ((note (gethash "i" (ekg-org-linear--linked-notes))))
          (should (equal "t" (ekg-org-get-property note "LINEAR_IMPORTED")))
          (should (equal "TODO" (ekg-org-task-state note))))
        ;; Reassigned away from me: the issue falls out of the filter and the
        ;; imported task is trashed on the next sync.
        (setf (alist-get 'assignee issue) '((id . "bob") (email . "b@x.com")))
        (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () nil)))
          (ekg-org-linear-sync))
        (let ((n (ekg-get-note-with-id "https://linear.app/issue/i")))
          (should n)
          (should (member ekg-trash-tag (ekg-note-tags n))))
        ;; The next sync drops the association entirely.
        (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () nil)))
          (ekg-org-linear-sync))
        (should-not (ekg-org-get-property
                     (ekg-get-note-with-id "https://linear.app/issue/i")
                     "LINEAR_ID"))))))

(ekg-deftest-with-db ekg-org-linear-test-prune-keeps-triage-and-delegated ()
  (let* ((triage (ekg-org-linear-test--issue "t" "Triage"))
         (deleg (ekg-org-linear-test--issue "d" "Delegated")))
    (setf (alist-get 'assignee triage) nil
          (alist-get 'assignee deleg) '((id . "bob") (email . "b@x.com")))
    (let ((ekg-org-linear-assignee "me")
          (ekg-org-linear-unassigned-state "TRIAGE")
          (ekg-org-linear-other-assignee-state "DELEGATED"))
      (cl-letf (((symbol-function 'ekg-org-linear--issues)
                 (lambda () (list triage deleg)))
                ((symbol-function 'ekg-org-linear--viewer-id) (lambda () "me"))
                ((symbol-function 'ekg-org-linear--paged-issues) (lambda (&rest _) nil)))
        (ekg-org-linear-sync)
        (should (equal "TRIAGE" (ekg-org-task-state (gethash "t" (ekg-org-linear--linked-notes)))))
        (should (equal "DELEGATED" (ekg-org-task-state (gethash "d" (ekg-org-linear--linked-notes)))))
        ;; Both options off: the issues leave the scope, but the deliberately
        ;; kept TRIAGE and DELEGATED tasks are not pruned.
        (let ((ekg-org-linear-unassigned-state nil)
              (ekg-org-linear-other-assignee-state nil))
          (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () nil)))
            (ekg-org-linear-sync))
          (let ((notes (ekg-org-linear--linked-notes)))
            (should-not (member ekg-trash-tag
                                (ekg-note-tags (gethash "t" notes))))
            (should-not (member ekg-trash-tag
                                (ekg-note-tags (gethash "d" notes))))))))))

(ekg-deftest-with-db ekg-org-linear-test-prune-reparents-children ()
  (let* ((parent (ekg-org-linear-test--issue "p" "Parent"))
         (child (ekg-org-linear-test--issue "c" "Child" "p"))
         (remote (list parent child)))
    (setf (alist-get 'assignee parent) '((id . "me") (email . "me@x.com"))
          (alist-get 'assignee child) '((id . "me") (email . "me@x.com")))
    (let ((ekg-org-linear-assignee "me"))
      (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote)))
        (ekg-org-linear-sync)
        ;; The parent falls out of scope; its child is still mine.  The child
        ;; must survive on its own, not be dragged into the trash.
        (setf (alist-get 'assignee parent) '((id . "bob") (email . "b@x.com")))
        (cl-letf (((symbol-function 'ekg-org-linear--issues)
                   (lambda () (list child))))
          (ekg-org-linear-sync))
        (let ((n-parent (ekg-get-note-with-id "https://linear.app/issue/p"))
              (n-child (ekg-get-note-with-id "https://linear.app/issue/c")))
          (should (member ekg-trash-tag (ekg-note-tags n-parent)))
          (should-not (member ekg-trash-tag (ekg-note-tags n-child)))
          (should-not (ekg-note-parent-id n-child)))))))

(ekg-deftest-with-db ekg-org-linear-test-gone-issue-unlinked ()
  (let* ((issue (ekg-org-linear-test--issue "i" "Title"))
         (note (ekg-note-create :text "Local body" :tags '("org/task" "org/state/todo"))))
    (ekg-save-note note)
    (cl-letf (((symbol-function 'ekg-org-linear--issue) (lambda (_) issue)))
      (ekg-org-linear-link (ekg-note-id note) "ENG-123"))
    ;; The issue disappears from Linear entirely; the explicitly linked task
    ;; survives but is unlinked instead of aborting the sync.
    (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () nil))
              ((symbol-function 'ekg-org-linear--find-issue) (lambda (_) nil)))
      (ekg-org-linear-sync))
    (should-not (ekg-org-get-property
                 (ekg-get-note-with-id "https://linear.app/issue/i")
                 "LINEAR_ID"))))

(ekg-deftest ekg-org-linear-test-pagination ()
             (let ((ekg-org-linear-import-team-id "team") cursors)
               (cl-letf (((symbol-function 'ekg-org-linear--request)
                          (lambda (query variables)
                            (if (string-match-p "team(" query)
                                '((team . ((id . "team"))))
                              (let ((cursor (alist-get 'after variables)))
                                (push cursor cursors)
                                (if cursor
                                    '((issues . ((nodes . (second)) (pageInfo . nil))))
                                  '((issues . ((nodes . (first))
                                               (pageInfo . ((hasNextPage . t)
                                                            (endCursor . "next"))))))))))))
                 (should (equal '(first second) (ekg-org-linear--issues)))
                 (should (equal '("next" nil) cursors)))))

(ekg-deftest-with-db ekg-org-linear-test-conflict-and-failed-update ()
                     (let* ((issue (ekg-org-linear-test--issue "i" "Base"))
                            (remote (list issue)))
                       (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote)))
                         (ekg-org-linear-sync)
                         (let* ((note (gethash "i" (ekg-org-linear--linked-notes)))
                                (snapshot (ekg-org-get-property note "LINEAR_SYNC")))
                           (setf (ekg-note-text note) "Local")
                           (ekg-save-note note)
                           (setf (alist-get 'description issue) "Remote")
                           (should-error (ekg-org-linear-sync))
                           (should (equal snapshot
                                          (ekg-org-get-property
                                           (gethash "i" (ekg-org-linear--linked-notes)) "LINEAR_SYNC")))
                           (setf (alist-get 'description issue) "Body")
                           (cl-letf (((symbol-function 'ekg-org-linear--request)
                                      (lambda (&rest _) '((issueUpdate . ((success . nil)))))))
                             (should-error (ekg-org-linear-sync)))
                           (setq note (gethash "i" (ekg-org-linear--linked-notes)))
                           (should (equal "Local" (ekg-note-text note)))
                           (should (equal snapshot (ekg-org-get-property note "LINEAR_SYNC")))))))

(ekg-deftest-with-db ekg-org-linear-test-link-and-duplicate ()
                     (ekg-org-add-schema)
                     (let ((a (ekg-note-create :text "Local" :tags '("org/task")))
                           (b (ekg-note-create :text "Other" :tags '("org/task"))))
                       (ekg-save-note a)
                       (ekg-save-note b)
                       (cl-letf (((symbol-function 'ekg-org-linear--issue)
                                  (lambda (_) (ekg-org-linear-test--issue "uuid" "Remote"))))
                         (ekg-org-linear-link (ekg-note-id a) "ENG-123")
                         (should (gethash "uuid" (ekg-org-linear--linked-notes)))
                         (should (equal "https://linear.app/issue/uuid"
                                        (ekg-note-id
                                         (gethash "uuid"
                                                  (ekg-org-linear--linked-notes)))))
                         (should-error (ekg-org-linear-link (ekg-note-id b) "ENG-123")))))

(ekg-deftest ekg-org-linear-test-request-errors ()
             (let ((ekg-org-linear-api-key "test-key"))
               (dolist (response '("{\"errors\":[{\"message\":\"denied\"}]}" "{}"))
                 (cl-letf (((symbol-function 'url-retrieve-synchronously)
                            (lambda (&rest _)
                              (let ((buffer (generate-new-buffer " *linear-test*")))
                                (with-current-buffer buffer
                                  (setq-local url-http-response-status 200)
                                  (insert "HTTP/1.1 200 OK\n\n")
                                  (setq-local url-http-end-of-headers (point-marker))
                                  (insert response))
                                buffer))))
                   (should-error (ekg-org-linear--request "query" nil))))))

(ekg-deftest ekg-org-linear-test-request-decodes-utf-8 ()
             ;; `url-retrieve-synchronously' can return a unibyte buffer
             ;; holding raw UTF-8 bytes; `ekg-org-linear--request' must
             ;; decode them so non-ASCII text survives.
             (let* ((ekg-org-linear-api-key "test-key")
                    (expected (format "a %s b" (string ?\u2192)))
                    (payload (format "{\"data\":{\"issue\":{\"description\":\"a %s b\"}}}"
                                     (string ?\u2192))))
               (cl-letf (((symbol-function 'url-retrieve-synchronously)
                          (lambda (&rest _)
                            (let ((buffer (generate-new-buffer " *linear-test*")))
                              (with-current-buffer buffer
                                (set-buffer-multibyte nil)
                                (setq-local url-http-response-status 200)
                                (insert "HTTP/1.1 200 OK\n\n")
                                (setq-local url-http-end-of-headers (point-marker))
                                (insert (encode-coding-string payload 'utf-8)))
                              buffer))))
                 (should (equal expected
                                (alist-get 'description
                                           (alist-get 'issue
                                                      (ekg-org-linear--request
                                                       "query" nil))))))))

(ekg-deftest ekg-org-linear-test-workflow-state-resolution ()
             (let ((issue (ekg-org-linear-test--issue "i" "Title")))
               (should (equal "todo" (ekg-org-linear--state-id issue "TODO")))
               (cl-letf (((symbol-function 'ekg-org-linear--request)
                          (lambda (&rest _)
                            '((team . ((states . ((nodes . (((id . "done")
                                                             (type . "completed"))))))))))))
                 (should (equal "done" (ekg-org-linear--state-id issue "DONE")))
                 (should-error (ekg-org-linear--state-id issue "UNMAPPED")))))

(ekg-deftest-with-db ekg-org-linear-test-parent-scope-and-cycle ()
  (let ((child (ekg-org-linear-test--issue "c" "Child" "p"))
        (parent (ekg-org-linear-test--issue "p" "Parent")))
    ;; A parent that never came into scope is not fetched: the tree starts
    ;; at the top in-scope task.
    (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () (list child)))
              ((symbol-function 'ekg-org-linear--issue)
               (lambda (_id) (ert-fail "Out-of-scope parent must not be fetched"))))
      (ekg-org-linear-sync)
      (should (= 1 (hash-table-count (ekg-org-linear--linked-notes))))
      (should-not (ekg-note-parent-id
                   (gethash "c" (ekg-org-linear--linked-notes)))))
    ;; Once the parent comes into scope, the child re-parents beneath it.
    (cl-letf (((symbol-function 'ekg-org-linear--issues)
               (lambda () (list child parent))))
      (ekg-org-linear-sync)
      (let* ((notes (ekg-org-linear--linked-notes))
             (p (gethash "p" notes))
             (c (gethash "c" notes)))
        (should (= 2 (hash-table-count notes)))
        (should (equal (ekg-note-id p) (ekg-note-parent-id c)))))
    ;; Cycles are still detected when both are in scope.
    (setf (alist-get 'parent parent) '((id . "c")))
    (cl-letf (((symbol-function 'ekg-org-linear--issues)
               (lambda () (list child parent))))
      (should-error (ekg-org-linear-sync)))))

(defun ekg-org-linear-test--note-by-title (title)
  "Return the sole note with TITLE."
  (car (ekg-get-notes-with-title title)))

(defun ekg-org-linear-test--task (title &optional parent)
  "Save and return a local task with TITLE and optional PARENT."
  (let ((note (ekg-note-create
               :text "Local body" :tags '("org/task" "org/state/todo")
               :properties (append (list :titled/title (list title))
                                   (when parent
                                     (list :hierarchy/parent
                                           (ekg-note-id parent)))))))
    (ekg-save-note note)
    note))

(ekg-deftest-with-db ekg-org-linear-test-create-marked-tree ()
                     (ekg-org-add-schema)
                     (let* ((ekg-org-linear-creation-team-id "team")
                            (parent (ekg-org-linear-test--task "Parent"))
                            (child (ekg-org-linear-test--task "Child" parent))
                            (unmarked (ekg-org-linear-test--task "Leave local"))
                            remote inputs)
                       (ekg-org-linear-mark-for-sync (ekg-note-id child))
                       (ekg-org-linear-mark-for-sync (ekg-note-id parent))
                       (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote))
                                 ((symbol-function 'ekg-org-linear--state-id)
                                  (lambda (_issue keyword) (should (equal keyword "TODO")) "todo"))
                                 ((symbol-function 'ekg-org-linear--request)
                                  (lambda (query variables)
                                    (should (string-match-p "issueCreate" query))
                                    (let* ((input (alist-get 'input variables))
                                           (issue (ekg-org-linear-test--issue
                                                   (alist-get 'id input) (alist-get 'title input)
                                                   (alist-get 'parentId input))))
                                      (setf (alist-get 'description issue)
                                            (alist-get 'description input))
                                      (push input inputs)
                                      (push issue remote)
                                      `((issueCreate . ((success . t) (issue . ,issue))))))))
                         (ekg-org-linear-sync)
                         (let* ((p (ekg-org-linear-test--note-by-title "Parent"))
                                (c (ekg-org-linear-test--note-by-title "Child"))
                                (parent-id (ekg-org-get-property p "LINEAR_ID")))
                           (should (= 2 (length inputs)))
                           (should-not (alist-get 'parentId (cadr inputs)))
                           (should (equal parent-id (alist-get 'parentId (car inputs))))
                           (should (equal "team" (alist-get 'teamId (car inputs))))
                           (should (equal "Local body" (ekg-note-text c)))
                           (should (equal (ekg-note-id p) (ekg-note-parent-id c)))
                           (should (string-prefix-p "https://linear.app/issue/"
                                                    (ekg-note-id p)))
                           (should-not (ekg-org-get-property unmarked "LINEAR_ID"))
                           (ekg-org-linear-sync)
                           (should (= 2 (length inputs)))))))

(ekg-deftest-with-db ekg-org-linear-test-create-parent-validation ()
                     (ekg-org-add-schema)
                     (let* ((parent (ekg-org-linear-test--task "Parent"))
                            (child (ekg-org-linear-test--task "Child" parent)))
                       (ekg-org-linear-mark-for-sync (ekg-note-id child))
                       (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () nil))
                                 ((symbol-function 'ekg-org-linear--create)
                                  (lambda (&rest _) (ert-fail "Must validate before creating"))))
                         (should-error (ekg-org-linear-sync))
                         (ekg-org-linear-mark-for-sync (ekg-note-id child) t)
                         (ekg-org-linear-sync))))

(ekg-deftest-with-db ekg-org-linear-test-create-linked-parent ()
                     (ekg-org-add-schema)
                     (let* ((parent (ekg-org-linear-test--task "Parent"))
                            (child (ekg-org-linear-test--task "Child" parent))
                            (remote-parent (ekg-org-linear-test--issue "p" "Parent")))
                       (ekg-org-set-property parent "LINEAR_ID" "p")
                       (ekg-save-note parent)
                       (ekg-org-linear-mark-for-sync (ekg-note-id child))
                       (cl-letf (((symbol-function 'ekg-org-linear--issues)
                                  (lambda () (list remote-parent)))
                                 ((symbol-function 'ekg-org-linear--create)
                                  (lambda (_note parent-id id)
                                    (should (equal parent-id "p"))
                                    (let ((issue (ekg-org-linear-test--issue id "Child" "p")))
                                      (setf (alist-get 'description issue) "Local body")
                                      issue))))
                         (ekg-org-linear-sync)
                         (should (equal "https://linear.app/issue/p"
                                        (ekg-note-id
                                         (ekg-org-linear-test--note-by-title "Parent"))))
                         (should (ekg-org-get-property
                                  (ekg-org-linear-test--note-by-title "Child")
                                  "LINEAR_ID")))))

(ekg-deftest-with-db ekg-org-linear-test-recover-ambiguous-creation ()
                     (ekg-org-add-schema)
                     (let ((task (ekg-org-linear-test--task "Title")) remote pushed)
                       (ekg-org-linear-mark-for-sync (ekg-note-id task))
                       (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote))
                                 ((symbol-function 'ekg-org-linear--create)
                                  (lambda (_note _parent id)
                                    (let ((issue (ekg-org-linear-test--issue id "Title")))
                                      (setf (alist-get 'description issue) "Local body")
                                      (push issue remote))
                                    (error "Connection lost after creation")))
                                 ((symbol-function 'ekg-org-linear--update)
                                  (lambda (_issue values) (setq pushed values))))
                         (should-error (ekg-org-linear-sync))
                         (setq task (ekg-get-note-with-id (ekg-note-id task)))
                         (should-not (ekg-org-get-property task "LINEAR_ID"))
                         (should (ekg-org-get-property task "LINEAR_CREATE_ID"))
                         ;; Both sides can change after an uncertain result, without losing edits.
                         (setf (ekg-note-text task) "Edited locally")
                         (ekg-save-note task)
                         (setf (alist-get 'title (car remote)) "Edited remotely")
                         (ekg-org-linear-sync)
                         (should (equal pushed '("Edited remotely" "Edited locally" "TODO")))
                         (should (= 1 (length remote)))
                         (should (= 1 (hash-table-count (ekg-org-linear--linked-notes)))))))

(ekg-deftest-with-db ekg-org-linear-test-mark-saves-edited-body ()
  (ekg-org-add-schema)
  (let ((task (ekg-org-linear-test--task "Edited task")))
    (with-temp-buffer
      (text-mode)
      (ekg-edit-mode 1)
      (setq-local ekg-note (ekg-get-note-with-id (ekg-note-id task)))
      (insert "Latest unsaved body")
      (should (buffer-modified-p))
      (ekg-org-linear-mark-for-sync (ekg-note-id task))
      (should-not (buffer-modified-p))
      (should (equal "Latest unsaved body"
                     (ekg-note-text (ekg-get-note-with-id (ekg-note-id task)))))
      (should (equal "Latest unsaved body" (buffer-string))))))

(ekg-deftest-with-db ekg-org-linear-test-sync-refreshes-open-editor ()
  (ekg-org-add-schema)
  (let ((task (ekg-org-linear-test--task "Title")) remote (creates 0))
    (with-temp-buffer
      (text-mode)
      (ekg-edit-mode 1)
      (setq-local ekg-note (ekg-get-note-with-id (ekg-note-id task)))
      (insert "Local body")
      (set-buffer-modified-p nil)
      (ekg-org-linear-mark-for-sync (ekg-note-id task))
      (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () remote))
                ((symbol-function 'ekg-org-linear--create)
                 (lambda (_note _parent id)
                   (cl-incf creates)
                   (let ((issue (ekg-org-linear-test--issue id "Title")))
                     (setf (alist-get 'description issue) "Local body")
                     (push issue remote)
                     issue))))
        (ekg-org-linear-sync)
        (should (ekg-org-get-property ekg-note "LINEAR_ID"))
        (should (ekg-org-get-property ekg-note "LINEAR_CREATE_ID"))
        (should (ekg-org-get-property ekg-note "LINEAR_SYNC"))
        (ekg--save-note-in-buffer)
        (ekg-org-linear-sync)
        (should (= 1 creates))
        (should (= 1 (hash-table-count (ekg-org-linear--linked-notes))))
        ;; Remote text changes must refresh the editor as well as the database.
        (setf (alist-get 'description (car remote)) "Remote body")
        (ekg-org-linear-sync)
        (should (equal "Remote body" (buffer-string)))
        (should (equal "Remote body" (ekg-note-text ekg-note)))
        (should-not (buffer-modified-p))
        (ekg--save-note-in-buffer)
        (should (equal "Remote body"
                       (ekg-note-text
                        (ekg-get-note-with-id (ekg-note-id ekg-note)))))))))

(ekg-deftest-with-db ekg-org-linear-test-sync-refuses-unsaved-task-editor ()
  (ekg-org-add-schema)
  (let ((task (ekg-org-linear-test--task "Title")) requested)
    (ekg-org-linear-mark-for-sync (ekg-note-id task))
    (with-temp-buffer
      (text-mode)
      (ekg-edit-mode 1)
      (setq-local ekg-note (ekg-get-note-with-id (ekg-note-id task)))
      (insert "Unsaved body")
      (cl-letf (((symbol-function 'ekg-org-linear--issues)
                 (lambda () (setq requested t) nil)))
        (should-error (ekg-org-linear-sync) :type 'user-error)
        (should (buffer-modified-p))
        (should (equal "Unsaved body" (buffer-string)))
        (should-not requested)
        ;; Title changes may update the note object without a dirty buffer.
        (set-buffer-modified-p nil)
        (setf (ekg-note-properties ekg-note)
              (plist-put (ekg--plist-without-key
                          (ekg-note-properties ekg-note) :titled/title)
                         :titled/title '("Unsaved title")))
        (should-error (ekg-org-linear-sync) :type 'user-error)
        (should-not requested)))))

(ekg-deftest-with-db ekg-org-linear-test-sync-preserves-unrelated-dirty-buffer ()
  (with-temp-buffer
    (insert "Unrelated unsaved text")
    (cl-letf (((symbol-function 'ekg-org-linear--issues)
               (lambda () (list (ekg-org-linear-test--issue "i" "Title")))))
      (ekg-org-linear-sync)
      (should (buffer-modified-p))
      (should (equal "Unrelated unsaved text" (buffer-string))))))

(ekg-deftest-with-db ekg-org-linear-test-recover-creation-outside-team ()
  (ekg-org-add-schema)
  (let ((task (ekg-org-linear-test--task "Title")) created (creates 0))
    (ekg-org-linear-mark-for-sync (ekg-note-id task))
    (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () nil))
              ((symbol-function 'ekg-org-linear--create)
               (lambda (_note _parent id)
                 (cl-incf creates)
                 (setq created (ekg-org-linear-test--issue id "Title"))
                 (setf (alist-get 'description created) "Local body"
                       (alist-get 'team created) '((id . "another-team")))
                 (error "Response lost")))
              ((symbol-function 'ekg-org-linear--request)
               (lambda (query variables)
                 (should (string-match-p "includeArchived: true" query))
                 (should (equal (alist-get 'id created) (alist-get 'id variables)))
                 `((issues . ((nodes . (,created))))))))
      (should-error (ekg-org-linear-sync))
      (ekg-org-linear-sync)
      (should (= 1 creates))
      (should (equal (alist-get 'id created)
                     (ekg-org-get-property
                      (ekg-org-linear-test--note-by-title "Title") "LINEAR_ID"))))))

(ekg-deftest-with-db ekg-org-linear-test-recovery-errors-do-not-create ()
  (ekg-org-add-schema)
  (let ((task (ekg-org-linear-test--task "Title")))
    (ekg-org-set-property task "LINEAR_CREATE_ID" "pending-uuid")
    (ekg-save-note task)
    (ekg-org-linear-mark-for-sync (ekg-note-id task))
    (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () nil))
              ((symbol-function 'ekg-org-linear--request)
               (lambda (&rest _) (error "Access denied")))
              ((symbol-function 'ekg-org-linear--create)
               (lambda (&rest _) (ert-fail "Must not create after lookup failure"))))
      (should-error (ekg-org-linear-sync)))))

(ekg-deftest-with-db ekg-org-linear-test-retry-uncreated-uuid ()
  (ekg-org-add-schema)
  (let ((task (ekg-org-linear-test--task "Title")))
    (ekg-org-set-property task "LINEAR_CREATE_ID" "pending-uuid")
    (ekg-org-set-property task "LINEAR_SYNC" "[\"Title\",\"Old body\",\"TODO\"]")
    (ekg-save-note task)
    (ekg-org-linear-mark-for-sync (ekg-note-id task))
    (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () nil))
              ((symbol-function 'ekg-org-linear--request)
               (lambda (&rest _) '((issues . ((nodes . nil))))))
              ((symbol-function 'ekg-org-linear--create)
               (lambda (_note _parent id)
                 (should (equal id "pending-uuid"))
                 (let ((issue (ekg-org-linear-test--issue id "Title")))
                   (setf (alist-get 'description issue) "Local body")
                   issue))))
      (ekg-org-linear-sync)
      (should (equal "pending-uuid"
                     (ekg-org-get-property
                      (ekg-org-linear-test--note-by-title "Title") "LINEAR_ID"))))))

(ekg-deftest-with-db ekg-org-linear-test-mark-from-another-buffer ()
  (ekg-org-add-schema)
  (let ((task (ekg-org-linear-test--task "Title")))
    (ekg-org-set-property task "DIR" "/tmp")
    (ekg-save-note task)
    (with-temp-buffer
      (text-mode)
      (ekg-edit-mode 1)
      (setq-local ekg-note (ekg-get-note-with-id (ekg-note-id task)))
      (insert "Unsaved editor text")
      (with-temp-buffer
        (should-error (ekg-org-linear-mark-for-sync (ekg-note-id task))
                      :type 'user-error))
      (should (buffer-modified-p))
      (should-not (ekg-org-get-property
                   (ekg-get-note-with-id (ekg-note-id task)) "LINEAR_SYNC_ENABLED"))
      ;; Once saved, marking elsewhere refreshes the clean editor's metadata.
      (ekg--save-note-in-buffer)
      (with-temp-buffer
        (ekg-org-linear-mark-for-sync (ekg-note-id task)))
      (should (equal "t" (ekg-org-get-property ekg-note "LINEAR_SYNC_ENABLED")))
      (ekg--save-note-in-buffer)
      (should (equal "t" (ekg-org-get-property
                          (ekg-get-note-with-id (ekg-note-id task))
                          "LINEAR_SYNC_ENABLED"))))))

(provide 'ekg-org-linear-test)
;;; ekg-org-linear-test.el ends here
