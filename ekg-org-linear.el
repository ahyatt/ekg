;;; ekg-org-linear.el --- Sync EKG tasks with Linear  -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Keywords: outlines, hypermedia
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
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
;; Bidirectional, explicit sync of task titles, descriptions and workflow
;; states.  Linear owns the imported parent/sub-issue hierarchy.  See the
;; Linear synchronization section of doc/ekg.org for setup and conflicts.

;;; Code:

(require 'ekg-org)
(require 'json)
(require 'org-id)
(require 'url)
(require 'url-http)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defgroup ekg-org-linear nil
  "Synchronize EKG Org tasks with Linear."
  :group 'ekg)

(defcustom ekg-org-linear-api-key nil
  "Linear API key, or a function returning one.
A function can retrieve the key from `auth-source' without storing it
in your configuration.  The key is sent only to Linear's API."
  :type '(choice (const nil) string function)
  :group 'ekg-org-linear)

(defcustom ekg-org-linear-creation-team-id nil
  "Linear team UUID in which marked EKG tasks are created.
Required to create new issues; importing does not use this option."
  :type '(choice (const nil) string)
  :group 'ekg-org-linear)

(defcustom ekg-org-linear-import-team-id nil
  "Optional Linear team UUID to restrict imported issues to.
When nil, issues are imported from every team, so set
`ekg-org-linear-assignee' or this option to limit the import."
  :type '(choice (const nil) string)
  :group 'ekg-org-linear)

(defcustom ekg-org-linear-assignee "me"
  "Limit imported Linear issues to a single assignee.
The value `\"me\"' imports only issues assigned to the API key's user,
a string is treated as an assignee email, and nil imports every issue
matching the team filter."
  :type '(choice (const :tag "Assigned to me" "me")
                 (const :tag "Everyone" nil)
                 (string :tag "Assignee email"))
  :group 'ekg-org-linear)

(defcustom ekg-org-linear-state-map
  '(("triage" . "TODO") ("backlog" . "TODO") ("unstarted" . "TODO")
    ("started" . "TODO") ("completed" . "DONE") ("canceled" . "DONE")
    ("duplicate" . "DONE"))
  "Map Linear workflow state types to Org TODO keywords.
When several Linear states map to the same keyword, retain the issue's
current state if possible, otherwise use the first matching team state.
Customize this to distinguish in-progress and canceled tasks."
  :type '(alist :key-type string :value-type string)
  :group 'ekg-org-linear)

(defconst ekg-org-linear--fields
  "id identifier title description state { id type } team { id } parent { id }"
  "GraphQL fields needed for an issue.")

(defun ekg-org-linear--request (query variables)
  "Execute Linear GraphQL QUERY with VARIABLES, returning its data.
Signal on transport, HTTP or GraphQL errors; never treat errors as success."
  (let* ((key (if (functionp ekg-org-linear-api-key)
                  (funcall ekg-org-linear-api-key) ekg-org-linear-api-key))
         (_ (unless (and (stringp key) (not (string-empty-p key)))
              (user-error "Configure ekg-org-linear-api-key first")))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,key) ("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode `((query . ,query) (variables . ,variables))) 'utf-8))
         (buffer (url-retrieve-synchronously "https://api.linear.app/graphql"
                                             t t 60)))
    (unless buffer (error "Linear request failed"))
    (unwind-protect
        (with-current-buffer buffer
          (unless (and (integerp url-http-response-status)
                       (<= 200 url-http-response-status 299))
            (error "Linear HTTP error: %s" url-http-response-status))
          (goto-char url-http-end-of-headers)
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (json-key-type 'symbol) (json-null nil) (json-false nil)
                 (response (json-read)))
            (when (alist-get 'errors response)
              (error "Linear GraphQL error: %s"
                     (mapcar (lambda (e) (alist-get 'message e))
                             (alist-get 'errors response))))
            (or (alist-get 'data response) (error "Linear returned no data"))))
      (kill-buffer buffer))))

(defun ekg-org-linear--editor-p ()
  "Return non-nil in a live EKG note editor."
  (and (or (bound-and-true-p ekg-edit-mode)
           (bound-and-true-p ekg-capture-mode))
       (bound-and-true-p ekg-note)))

(defun ekg-org-linear--check-editors (&optional note-id except-buffer)
  "Check task editors for unsaved edits before syncing.
When NOTE-ID is provided, check only that task, even if unlinked.
Skip EXCEPT-BUFFER when marking from the editor that is being saved."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (not (eq buffer except-buffer))
                 (ekg-org-linear--editor-p)
                 (if note-id
                     (equal note-id (ekg-note-id ekg-note))
                   (or (ekg-org-get-property ekg-note "LINEAR_ID")
                       (ekg-org-get-property ekg-note "LINEAR_SYNC_ENABLED"))))
        (let ((stored (ekg-get-note-with-id (ekg-note-id ekg-note))))
          (when (or (buffer-modified-p)
                    (not (equal (ekg-org-linear--local-values ekg-note)
                                (ekg-org-linear--local-values stored)))
                    (not (equal (ekg-note-tags ekg-note) (ekg-note-tags stored)))
                    (not (equal (sort (ekg-org-properties-alist ekg-note)
                                      (lambda (a b) (string< (car a) (car b))))
                                (sort (ekg-org-properties-alist stored)
                                      (lambda (a b) (string< (car a) (car b)))))))
            (user-error "Save or close task editor %s before Linear sync"
                        (buffer-name))))))))

(defun ekg-org-linear--save-note (note)
  "Save NOTE without changing the caller's buffer modification flag.
Refresh matching clean task editors, including their link metadata, so a
subsequent editor save cannot restore stale content or remove the link."
  (with-temp-buffer (ekg-save-note note))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (ekg-org-linear--editor-p)
                 (equal (ekg-note-id ekg-note) (ekg-note-id note))
                 (not (buffer-modified-p)))
        (let ((text (ekg-insert-inlines-representation
                     (ekg-note-text note) (ekg-note-inlines note)))
              (position (point))
              (inhibit-read-only t))
          (unless (equal text (buffer-substring-no-properties
                               (ekg--note-text-beginning) (point-max)))
            (delete-region (ekg--note-text-beginning) (point-max))
            (goto-char (point-max))
            (insert text)
            (goto-char (min position (point-max))))
          ;; Read back the saved representation, including normalized tags.
          (setq ekg-note (ekg-get-note-with-id (ekg-note-id note))
                ekg-note-orig-note (copy-tree ekg-note)
                ekg-note-orig-id (ekg-note-id note)
                header-line-format (ekg--header-line-format))
          (set-buffer-modified-p nil))))))

(defun ekg-org-linear--find-issue (id)
  "Find issue UUID ID across teams, including archived issues.
Return nil only for an empty successful lookup, propagating API errors."
  (let* ((data (ekg-org-linear--request
                (format (concat "query($id: ID!) { issues(first: 1,"
                                " includeArchived: true, filter: {id: {eq: $id}})"
                                " { nodes { %s } } }")
                        ekg-org-linear--fields)
                `((id . ,id))))
         (connection (or (alist-get 'issues data)
                         (error "Linear returned no issue lookup connection"))))
    (car (alist-get 'nodes connection))))

(defun ekg-org-linear--issue (id)
  "Fetch issue ID (a UUID or identifier such as ENG-123)."
  (or (alist-get 'issue
                 (ekg-org-linear--request
                  (format "query($id: String!) { issue(id: $id) { %s } }"
                          ekg-org-linear--fields)
                  `((id . ,id))))
      (error "Linear issue not found: %s" id)))

(defun ekg-org-linear--check-team (team-id)
  "Signal a clear error unless TEAM-ID names an existing Linear team.
Linear's issue filter returns an empty result for an unknown team, so a
missing or mistyped team would otherwise make sync silently do nothing."
  (condition-case err
      (unless (alist-get 'team
                         (ekg-org-linear--request
                          "query($id: String!) { team(id: $id) { id } }"
                          `((id . ,team-id))))
        (user-error "Linear team %s not found; check `ekg-org-linear-import-team-id'"
                    team-id))
    (error
     (user-error "Linear team %s not found; check `ekg-org-linear-import-team-id' (%s)"
                 team-id (error-message-string err)))))

(defun ekg-org-linear--issue-filter ()
  "Build the GraphQL issue filter from the configured options."
  (let (filter)
    (when ekg-org-linear-import-team-id
      (setq filter (cons `(team . ((id . ((eq . ,ekg-org-linear-import-team-id)))))
                         filter)))
    (when ekg-org-linear-assignee
      (setq filter
            (cons `(assignee . ,(if (equal ekg-org-linear-assignee "me")
                                    '((isMe . ((eq . t))))
                                  `((email . ((eq . ,ekg-org-linear-assignee))))))
                  filter)))
    (unless filter
      (user-error "Configure `ekg-org-linear-import-team-id' or `ekg-org-linear-assignee'
to limit sync"))
    filter))

(defun ekg-org-linear--issues ()
  "Fetch all pages of issues matching the configured team and assignee."
  (when ekg-org-linear-import-team-id
    (ekg-org-linear--check-team ekg-org-linear-import-team-id))
  (let ((filter (ekg-org-linear--issue-filter))
        cursor issues more)
    (setq more t)
    (while more
      (let* ((data (ekg-org-linear--request
                    (format (concat "query($filter: IssueFilter!, $after: String) {"
                                    " issues(first: 100, after: $after, filter: $filter) {"
                                    " nodes { %s }"
                                    " pageInfo { hasNextPage endCursor } } }")
                            ekg-org-linear--fields)
                    `((filter . ,filter) (after . ,cursor))))
             (connection (or (alist-get 'issues data)
                             (error "Linear returned no issues connection")))
             (page (alist-get 'pageInfo connection)))
        (setq issues (append issues (alist-get 'nodes connection))
              more (alist-get 'hasNextPage page))
        (when more
          (let ((next (alist-get 'endCursor page)))
            (unless (and next (not (equal cursor next)))
              (error "Linear pagination did not advance"))
            (setq cursor next)))))
    issues))

(defun ekg-org-linear--remote-values (issue)
  "Return synchronized title, description and Org state for ISSUE."
  (list (alist-get 'title issue) (or (alist-get 'description issue) "")
        (or (cdr (assoc (alist-get 'type (alist-get 'state issue))
                        ekg-org-linear-state-map))
            (error "Unmapped Linear state for %s" (alist-get 'identifier issue)))))

(defun ekg-org-linear--local-values (note)
  "Return synchronized values from NOTE."
  (list (ekg-org--note-title note) (or (ekg-note-text note) "")
        (ekg-org-task-state note)))

(defun ekg-org-linear--merge (local remote base)
  "Merge LOCAL and REMOTE against BASE, signaling on conflicting fields."
  (if (null base) remote
    (cl-mapcar (lambda (l r b)
                 (cond ((equal l r) l) ((equal l b) r) ((equal r b) l)
                       (t (error "Linear sync conflict; local: %S; remote: %S"
                                 l r))))
               local remote base)))

(defun ekg-org-linear--state-id (issue keyword)
  "Resolve Org KEYWORD to a workflow state ID for ISSUE's team."
  (if (and (alist-get 'state issue)
           (equal keyword (nth 2 (ekg-org-linear--remote-values issue))))
      (alist-get 'id (alist-get 'state issue))
    (let* ((team (alist-get 'team
                            (ekg-org-linear--request
                             (concat "query($id: String!) { team(id: $id) {"
                                     " states { nodes { id type } } } }")
                             `((id . ,(alist-get 'id (alist-get 'team issue)))))))
           (state (seq-find
                   (lambda (s)
                     (equal keyword (cdr (assoc (alist-get 'type s)
                                                ekg-org-linear-state-map))))
                   (alist-get 'nodes (alist-get 'states team)))))
      (or (alist-get 'id state)
          (error "No Linear workflow state maps to %s" keyword)))))

(defun ekg-org-linear--update (issue values)
  "Update ISSUE with synchronized VALUES, checking mutation success."
  (let ((result
         (alist-get 'issueUpdate
                    (ekg-org-linear--request
                     (concat "mutation($id: String!, $input: IssueUpdateInput!) {"
                             " issueUpdate(id: $id, input: $input) { success } }")
                     `((id . ,(alist-get 'id issue))
                       (input . ((title . ,(nth 0 values))
                                 (description . ,(nth 1 values))
                                 (stateId . ,(ekg-org-linear--state-id
                                              issue (nth 2 values))))))))))
    (unless (eq t (alist-get 'success result))
      (error "Linear issue update failed"))))

(defun ekg-org-linear--linked-notes ()
  "Return a hash table mapping Linear UUIDs to EKG notes."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row (triples-db-select ekg-db nil 'org/property nil))
      (let ((property (read (nth 2 row))))
        (when (equal (car-safe property) "LINEAR_ID")
          (when (gethash (cdr property) table)
            (error "Multiple EKG notes linked to Linear issue %s" (cdr property)))
          (puthash (cdr property) (ekg-get-note-with-id (car row)) table))))
    table))

;;;###autoload
(defun ekg-org-linear-link (note-id issue-id)
  "Link existing EKG task NOTE-ID to Linear ISSUE-ID.
The first sync imports Linear's text and state.  No issue is created."
  (interactive (list (read-number "EKG task ID: ")
                     (read-string "Linear issue ID (e.g. ENG-123): ")))
  (ekg-connect)
  (ekg-org-add-schema)
  (ekg-org-linear--check-editors note-id)
  (let* ((note (ekg-get-note-with-id note-id))
         (issue (ekg-org-linear--issue issue-id))
         (id (alist-get 'id issue))
         (other (gethash id (ekg-org-linear--linked-notes))))
    (unless (and note (member ekg-org-task-tag (ekg-note-tags note)))
      (user-error "Not an EKG Org task: %s" note-id))
    (when (and other (not (equal note-id (ekg-note-id other))))
      (user-error "Linear issue is already linked to task %s" (ekg-note-id other)))
    (ekg-org-set-property note "LINEAR_ID" id)
    (ekg-org-remove-property note "LINEAR_SYNC")
    (ekg-org-linear--save-note note)))

;;;###autoload
(defun ekg-org-linear-mark-for-sync (note-id &optional unmark)
  "Mark task NOTE-ID for creation in Linear on the next sync.
With UNMARK (interactively, a prefix argument), remove the mark.
A parent must already be linked or also marked.  Marking does not
perform network requests.  Existing links continue syncing if unmarked."
  (interactive
   (list (or (and (derived-mode-p 'ekg-org-view-mode)
                  (ekg-org-view--note-at-point))
             (and (bound-and-true-p ekg-note) (ekg-note-id ekg-note))
             (read-number "EKG task ID: "))
         current-prefix-arg))
  (ekg-connect)
  (ekg-org-add-schema)
  (let ((note (if (and (bound-and-true-p ekg-note)
                       (equal note-id (ekg-note-id ekg-note)))
                  ekg-note
                (ekg-get-note-with-id note-id))))
    (unless (and note (member ekg-org-task-tag (ekg-note-tags note)))
      (user-error "Not an EKG Org task: %s" note-id))
    (ekg-org-linear--check-editors
     note-id (when (and (ekg-org-linear--editor-p) (eq note ekg-note))
               (current-buffer)))
    (when (and (ekg-org-linear--editor-p) (eq note ekg-note))
      (ekg-note-update-from-buffer))
    (if unmark
        (ekg-org-remove-property note "LINEAR_SYNC_ENABLED")
      (ekg-org-set-property note "LINEAR_SYNC_ENABLED" "t"))
    (ekg-org-linear--save-note note)
    (when (and (ekg-org-linear--editor-p) (eq note ekg-note))
      (set-buffer-modified-p nil))))

(defun ekg-org-linear--create (note parent-id id)
  "Create a Linear issue for NOTE under PARENT-ID, using stable UUID ID."
  (unless ekg-org-linear-creation-team-id
    (user-error "Configure `ekg-org-linear-creation-team-id' to create Linear issues"))
  (let* ((values (ekg-org-linear--local-values note))
         (state-id (ekg-org-linear--state-id
                    `((team . ((id . ,ekg-org-linear-creation-team-id)))) (nth 2 values)))
         (result
          (alist-get
           'issueCreate
           (ekg-org-linear--request
            (format (concat "mutation($input: IssueCreateInput!) {"
                            " issueCreate(input: $input) { success issue { %s } } }")
                    ekg-org-linear--fields)
            `((input . ((id . ,id) (teamId . ,ekg-org-linear-creation-team-id)
                        (title . ,(car values)) (description . ,(nth 1 values))
                        (stateId . ,state-id) (parentId . ,parent-id))))))))
    (unless (and (eq t (alist-get 'success result))
                 (equal id (alist-get 'id (alist-get 'issue result))))
      (error "Linear issue creation failed for task %s" (ekg-note-id note)))
    (alist-get 'issue result)))

(defun ekg-org-linear--export-marked (notes issues)
  "Create marked local tasks, updating linked NOTES and remote ISSUES tables.
Validate ancestry before creating anything, then create parents first.
Persist a client UUID before creation so retries cannot create duplicates."
  (let ((pending (make-hash-table :test 'equal))
        (checked (make-hash-table :test 'equal)))
    (dolist (row (triples-db-select ekg-db nil 'org/property nil))
      (when (equal (read (nth 2 row)) '("LINEAR_SYNC_ENABLED" . "t"))
        (let ((note (ekg-get-note-with-id (car row))))
          (when (and (member ekg-org-task-tag (ekg-note-tags note))
                     (ekg-org--note-active-p note)
                     (not (ekg-org-get-property note "LINEAR_ID")))
            (puthash (ekg-note-id note) note pending)))))
    (cl-labels
        ((validate
           (note)
           (let ((id (ekg-note-id note)))
             (when (eq (gethash id checked) 'visiting)
               (user-error "Cycle in EKG task parents: %s" id))
             (unless (eq (gethash id checked) 'done)
               (puthash id 'visiting checked)
               (unless (and (ekg-org--note-title note) (ekg-org-task-state note))
                 (user-error "Task %s needs a title and TODO state" id))
               (when-let* ((parent-id (ekg-note-parent-id note)))
                 (let ((parent (ekg-get-note-with-id parent-id)))
                   (unless parent (user-error "Missing parent task %s" parent-id))
                   (unless (ekg-org-get-property parent "LINEAR_ID")
                     (unless (gethash parent-id pending)
                       (user-error "Link or mark parent task %s for Linear first"
                                   parent-id))
                     (validate parent))))
               (puthash id 'done checked))))
         (export-one
           (note)
           (or (ekg-org-get-property note "LINEAR_ID")
               (let* ((parent (when-let* ((id (ekg-note-parent-id note)))
                                (ekg-get-note-with-id id)))
                      (parent-id (when parent
                                   (or (ekg-org-get-property parent "LINEAR_ID")
                                       (export-one
                                        (gethash (ekg-note-id parent) pending)))))
                      (pending-id (ekg-org-get-property note "LINEAR_CREATE_ID"))
                      (id (or pending-id (org-id-uuid)))
                      (existing (or (gethash id issues)
                                    (and pending-id
                                         (ekg-org-linear--find-issue id))))
                      (values (ekg-org-linear--local-values note)))
                 (unless (ekg-org-get-property note "LINEAR_CREATE_ID")
                   (ekg-org-set-property note "LINEAR_CREATE_ID" id)
                   (ekg-org-set-property note "LINEAR_SYNC" (json-encode values))
                   (ekg-org-linear--save-note note))
                 (let ((issue (or existing
                                  (ekg-org-linear--create note parent-id id))))
                   (ekg-org-set-property note "LINEAR_ID" id)
                   ;; Retain the pre-creation snapshot on retries so remote
                   ;; edits after an ambiguous failure still merge normally.
                   (unless existing
                     (ekg-org-set-property note "LINEAR_SYNC"
                                           (json-encode values)))
                   (ekg-org-linear--save-note note)
                   (puthash id note notes)
                   (puthash id issue issues))
                 id))))
      (maphash (lambda (_id note) (validate note)) pending)
      (maphash (lambda (_id note) (export-one note)) pending))))

;;;###autoload
(defun ekg-org-linear-sync ()
  "Sync linked tasks and import new Linear issues with their parent tree.
Merge title, description and state independently against the last sync.
Conflicting edits signal an error without overwriting that issue.  Earlier
issues may already have synced; rerunning is safe.  Linear owns hierarchy.
Marked local tasks are created in Linear, with their appropriate parent."
  (interactive)
  (ekg-connect)
  (ekg-org-add-schema)
  (ekg-org-linear--check-editors)
  (let ((notes (ekg-org-linear--linked-notes))
        (issues (make-hash-table :test 'equal))
        (visiting (make-hash-table :test 'equal))
        (ekg-org--inhibit-view-refresh t))
    (dolist (issue (ekg-org-linear--issues))
      (puthash (alist-get 'id issue) issue issues))
    ;; Keep explicitly linked issues in scope even outside the import team.
    (maphash (lambda (id _note)
               (unless (gethash id issues)
                 (puthash id (ekg-org-linear--issue id) issues))) notes)
    (unwind-protect
        (progn
          (ekg-org-linear--export-marked notes issues)
          (cl-labels
              ((sync-one
                 (id)
                 (when (eq (gethash id visiting) 'visiting)
                   (error "Cycle in Linear issue parents: %s" id))
                 (unless (eq (gethash id visiting) 'done)
                   (puthash id 'visiting visiting)
                   (let* ((issue (or (gethash id issues)
                                     (ekg-org-linear--issue id)))
                          (parent-id (alist-get 'id (alist-get 'parent issue)))
                          (parent (when parent-id (sync-one parent-id)))
                          (note (gethash id notes))
                          (remote (ekg-org-linear--remote-values issue))
                          (snapshot (and note (ekg-org-get-property note "LINEAR_SYNC")))
                          (base (when snapshot
                                  (let ((json-array-type 'list))
                                    (json-read-from-string snapshot))))
                          (values (ekg-org-linear--merge
                                   (when note (ekg-org-linear--local-values note))
                                   remote base))
                          (save-needed
                           (or (null note)
                               (not (equal values
                                           (ekg-org-linear--local-values note)))
                               (not (equal snapshot (json-encode values)))
                               (not (equal (ekg-note-parent-id note)
                                           (when parent (ekg-note-id parent)))))))
                     (unless (equal values remote)
                       (ekg-org-linear--update issue values))
                     (unless note
                       (setq note
                             (ekg-note-create
                              :text ""
                              :mode (if (fboundp 'markdown-mode)
                                        'markdown-mode 'text-mode)
                              :tags (list ekg-org-task-tag)))
                       (ekg-org-set-dependency-type note 'parallel))
                     (setf (ekg-note-text note) (nth 1 values)
                           (ekg-note-tags note)
                           (cons (concat ekg-org-state-tag-prefix
                                         (downcase (nth 2 values)))
                                 (seq-remove
                                  (lambda (tag)
                                    (string-prefix-p ekg-org-state-tag-prefix tag))
                                  (ekg-note-tags note))))
                     (dolist (pair `((:titled/title . ,(list (car values)))
                                     (:hierarchy/parent
                                      . ,(when parent (ekg-note-id parent)))))
                       (setf (ekg-note-properties note)
                             (plist-put (ekg--plist-without-key
                                         (ekg-note-properties note) (car pair))
                                        (car pair) (cdr pair))))
                     (ekg-org-set-property note "LINEAR_ID" id)
                     (ekg-org-set-property note "LINEAR_SYNC" (json-encode values))
                     (when save-needed (ekg-org-linear--save-note note))
                     (puthash id note notes)
                     (puthash id 'done visiting)))
                 (gethash id notes)))
            (maphash (lambda (id _issue) (sync-one id)) issues)))
      (ekg-org-view--refresh-all))))

(provide 'ekg-org-linear)
;;; ekg-org-linear.el ends here
