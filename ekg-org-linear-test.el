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
               (team . ((id . "team"))) (state . ((id . "todo") (type . "unstarted")))
               (parent . ,(when parent `((id . ,parent)))))))

(ekg-deftest ekg-org-linear-test-merge ()
             (should (equal (ekg-org-linear--merge '("Local" "Body" "TODO")
                                                   '("Title" "Remote" "TODO")
                                                   '("Title" "Body" "TODO"))
                            '("Local" "Remote" "TODO")))
             (should-error (ekg-org-linear--merge '("Local" "" "TODO")
                                                  '("Remote" "" "TODO")
                                                  '("Base" "" "TODO"))))

(ekg-deftest ekg-org-linear-test-duplicate-state ()
             (should (equal "DONE"
                            (nth 2
                                 (ekg-org-linear--remote-values
                                  `((id . "i") (identifier . "I-1")
                                    (title . "Title") (description . "")
                                    (team . ((id . "team")))
                                    (state . ((id . "d") (type . "duplicate")))))))))

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
               (should-error (ekg-org-linear--issue-filter))))

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

(ekg-deftest ekg-org-linear-test-request-uses-unibyte-parts ()
             ;; A multibyte credential or body would make `url-http' reject
             ;; the concatenated request and print it, secrets included.
             (let ((ekg-org-linear-api-key (string-to-multibyte "test-key"))
                   data headers)
               (cl-letf (((symbol-function 'url-retrieve-synchronously)
                          (lambda (&rest _)
                            (setq data url-request-data
                                  headers url-request-extra-headers)
                            (let ((buffer (generate-new-buffer " *linear-test*")))
                              (with-current-buffer buffer
                                (setq-local url-http-response-status 200)
                                (insert "HTTP/1.1 200 OK\n\n")
                                (setq-local url-http-end-of-headers (point-marker))
                                (insert "{\"data\":{\"ok\":true}}"))
                              buffer))))
                 (ekg-org-linear--request "query" `((text . ,(string ?\u2192))))
                 (should-not (multibyte-string-p data))
                 (dolist (header headers)
                   (should-not (multibyte-string-p (car header)))
                   (should-not (multibyte-string-p (cdr header)))))))

(ekg-deftest ekg-org-linear-test-request-error-does-not-leak-key ()
             (let ((ekg-org-linear-api-key "super-secret"))
               (cl-letf (((symbol-function 'url-retrieve-synchronously)
                          (lambda (&rest _)
                            (error (concat "Request rejected: POST /graphql "
                                           "Authorization: "
                                           ekg-org-linear-api-key)))))
                 (let ((err (should-error (ekg-org-linear--request "query" nil))))
                   (should-not (string-match-p "super-secret"
                                               (error-message-string err)))))))

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

(ekg-deftest-with-db ekg-org-linear-test-missing-ancestor-and-cycle ()
                     (let ((child (ekg-org-linear-test--issue "c" "Child" "p"))
                           (parent (ekg-org-linear-test--issue "p" "Parent")))
                       (cl-letf (((symbol-function 'ekg-org-linear--issues) (lambda () (list child)))
                                 ((symbol-function 'ekg-org-linear--issue) (lambda (_) parent)))
                         (ekg-org-linear-sync)
                         (should (= 2 (hash-table-count (ekg-org-linear--linked-notes))))
                         (setf (alist-get 'parent parent) '((id . "c")))
                         (should-error (ekg-org-linear-sync)))))

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
                         (let* ((p (ekg-get-note-with-id (ekg-note-id parent)))
                                (c (ekg-get-note-with-id (ekg-note-id child)))
                                (parent-id (ekg-org-get-property p "LINEAR_ID")))
                           (should (= 2 (length inputs)))
                           (should-not (alist-get 'parentId (cadr inputs)))
                           (should (equal parent-id (alist-get 'parentId (car inputs))))
                           (should (equal "team" (alist-get 'teamId (car inputs))))
                           (should (equal "Local body" (ekg-note-text c)))
                           (should (equal (ekg-note-id p) (ekg-note-parent-id c)))
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
                         (should (ekg-org-get-property
                                  (ekg-get-note-with-id (ekg-note-id child)) "LINEAR_ID")))))

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
                       (ekg-note-text (ekg-get-note-with-id (ekg-note-id task)))))))))

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
                      (ekg-get-note-with-id (ekg-note-id task)) "LINEAR_ID"))))))

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
                      (ekg-get-note-with-id (ekg-note-id task)) "LINEAR_ID"))))))

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
