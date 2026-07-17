;;; ekg-agent-test.el --- Tests for ekg-agent  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Andrew Hyatt <ahyatt@gmail.com>

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

;; Tests for ekg-agent functionality.

;;; Code:

(require 'ert)
(require 'ekg-agent)
(require 'ekg-test-utils)

;; Indentation adjustment

(ekg-deftest ekg-agent-test-adjust-indentation-strip-extra ()
             "Extra leading whitespace in the replacement is stripped."
             (should (equal "    (defun foo ()\n      (bar))"
                            (ekg-agent--adjust-indentation
                             "        (defun foo ()\n          (bar))" 4))))

(ekg-deftest ekg-agent-test-adjust-indentation-no-change ()
             "Replacement already at the correct indentation is unchanged."
             (should (equal "    (defun foo ()\n      (bar))"
                            (ekg-agent--adjust-indentation
                             "    (defun foo ()\n      (bar))" 4))))

(ekg-deftest ekg-agent-test-adjust-indentation-add-whitespace ()
             "Whitespace is added when the replacement is under-indented."
             (should (equal "    (defun foo ()\n      (bar))"
                            (ekg-agent--adjust-indentation
                             "  (defun foo ()\n    (bar))" 4))))

(ekg-deftest ekg-agent-test-adjust-indentation-empty-lines ()
             "Empty lines in the replacement are preserved as-is."
             (should (equal "    line1\n\n    line3"
                            (ekg-agent--adjust-indentation
                             "        line1\n\n        line3" 4))))

(ekg-deftest ekg-agent-test-adjust-indentation-zero-target ()
             "Adjustment to column zero strips all common indentation."
             (should (equal "(foo)\n  (bar)"
                            (ekg-agent--adjust-indentation
                             "    (foo)\n      (bar)" 0))))

(ekg-deftest ekg-agent-test-adjust-indentation-single-line ()
             "A single line replacement is adjusted correctly."
             (should (equal "  hello"
                            (ekg-agent--adjust-indentation "      hello" 2))))

;; Error-as-text macro

(ekg-deftest ekg-agent-test-error-as-text-on-error ()
             "Errors are caught and returned as a descriptive string."
             (should (equal "Error: something broke"
                            (ekg-agent--with-error-as-text
                              (error "something broke")))))

(ekg-deftest ekg-agent-test-error-as-text-on-success ()
             "Successful results pass through unchanged."
             (should (equal 42
                            (ekg-agent--with-error-as-text
                              (+ 40 2)))))

(ekg-deftest ekg-agent-test-current-buffer-context-is-lightweight ()
             "Current buffer context includes metadata but not full contents."
             (let ((buf (get-buffer-create "*ekg-agent-test-context*")))
               (unwind-protect
                   (with-current-buffer buf
                     (erase-buffer)
                     (emacs-lisp-mode)
                     (insert "SECRET-CONTENT-SHOULD-NOT-BE-IN-CONTEXT\n")
                     (let ((context (ekg-agent--current-buffer-context)))
                       (should (string-match-p "Current buffer:" context))
                       (should (string-match-p "name: \\*ekg-agent-test-context\\*"
                                               context))
                       (should (string-match-p "major mode: emacs-lisp-mode" context))
                       (should (string-match-p "search_buffer" context))
                       (should-not (string-match-p "SECRET-CONTENT" context))))
                 (kill-buffer buf))))

(ekg-deftest-with-db ekg-agent-test-latest-note-previews-are-bounded ()
                     "Default ask context includes bounded latest-note previews."
                     (let ((ekg-agent-ask-latest-note-preview-words 5))
                       (ekg-save-note
                        (ekg-note-create
                         :text "one two three four five six seven eight nine ten"
                         :mode 'text-mode
                         :tags '("preview-test")))
                       (let ((context (ekg-agent--latest-note-previews-context)))
                         (should (string-match-p "first 5 words" context))
                         (should (string-match-p "one two three four five" context))
                         (should-not (string-match-p "six seven eight" context)))))

(ekg-deftest ekg-agent-test-ask-defers-startup ()
             "`ekg-agent-ask' schedules startup so preprocessing is not immediate."
             (let (scheduled ask-called)
               (cl-letf (((symbol-function 'run-at-time)
                          (lambda (&rest args)
                            (setq scheduled args)
                            'ekg-agent-test-timer))
                         ((symbol-function 'ekg-agent--read-provider-for-prefix)
                          (lambda (_arg) 'provider))
                         ((symbol-function 'ekg-agent--ask)
                          (lambda (&rest _args)
                            (setq ask-called t))))
                 (ekg-agent-ask "Question?" nil)
                 (should scheduled)
                 (should-not ask-called))))

(ekg-deftest ekg-agent-test-prompt-id-async-renames-log-buffer ()
             "Prompt IDs use a fallback immediately and LLM naming runs async."
             (let* ((prompt (llm-make-chat-prompt "Explain org sync behavior"))
                    (log-buf (get-buffer-create "*ekg agent log: explain-org-sync-behavior*"))
                    (async-called nil))
               (unwind-protect
                   (progn
                     (cl-letf (((symbol-function 'llm-chat)
                                (lambda (&rest _args)
                                  (error "llm-chat should not be called"))))
                       (should (equal "explain-org-sync-behavior"
                                      (ekg-agent--prompt-id prompt))))
                     (cl-letf (((symbol-function 'llm-chat-async)
                                (lambda (_provider name-prompt response-callback
                                                   _error-callback &optional _multi-output)
                                  (setq async-called t)
                                  (let ((tool (car (llm-chat-prompt-tools name-prompt))))
                                    (funcall (llm-tool-function tool) "better-run-name"))
                                  (funcall response-callback nil)
                                  'mock-name-request)))
                       (ekg-agent--prompt-id-async prompt log-buf (make-llm-fake)))
                     (should async-called)
                     (should (get-buffer "*ekg agent log: better-run-name*")))
                 (dolist (name '("*ekg agent log: explain-org-sync-behavior*"
                                 "*ekg agent log: better-run-name*"))
                   (when-let* ((buf (get-buffer name)))
                     (kill-buffer buf))))))

(ekg-deftest ekg-agent-test-prompt-id-async-not-scheduled-by-default ()
             "LLM naming is opt-in so startup does not start an extra request."
             (let ((ekg-agent-llm-name-log-buffer nil)
                   (prompt (llm-make-chat-prompt "Explain org sync behavior"))
                   (log-buf (get-buffer-create "*ekg-agent-test-name-log*")))
               (unwind-protect
                   (cl-letf (((symbol-function 'run-at-time)
                              (lambda (&rest _args)
                                (error "run-at-time should not be called"))))
                     (should-not (ekg-agent--schedule-prompt-id-async
                                  prompt log-buf (make-llm-fake))))
                 (kill-buffer log-buf))))

(ekg-deftest ekg-agent-test-prompt-id-async-schedules-when-enabled ()
             "Optional LLM naming is scheduled instead of run during setup."
             (let ((ekg-agent-llm-name-log-buffer t)
                   (prompt (llm-make-chat-prompt "Explain org sync behavior"))
                   (log-buf (get-buffer-create "*ekg-agent-test-name-log*"))
                   scheduled)
               (unwind-protect
                   (cl-letf (((symbol-function 'run-at-time)
                              (lambda (&rest args)
                                (setq scheduled args)
                                'mock-timer)))
                     (should (ekg-agent--schedule-prompt-id-async
                              prompt log-buf (make-llm-fake)))
                     (should scheduled)
                     (should (= 0 (car scheduled))))
                 (kill-buffer log-buf))))

(ekg-deftest ekg-agent-test-status-reminder-adds-prompt-when-overdue ()
             "An overdue session gets a prompt reminder to summarize state."
             (let ((buf (get-buffer-create "*ekg-agent-test-reminder*"))
                   (prompt (llm-make-chat-prompt "Work on the task."))
                   (ekg-agent-status-reminder-seconds 60)
                   (ekg-agent--current-log-buffer nil))
               (unwind-protect
                   (let ((ekg-agent--current-log-buffer buf))
                     (with-current-buffer buf
                       (setq ekg-agent--running-p t)
                       (setq ekg-agent--last-status-update-time 100.0)
                       (setq ekg-agent--last-status-reminder-time nil))
                     (cl-letf (((symbol-function 'float-time)
                                (lambda (&optional _time) 161.0)))
                       (should (ekg-agent--maybe-remind-status-update prompt)))
                     (with-current-buffer buf
                       (should (= 161.0 ekg-agent--last-status-reminder-time)))
                     (let* ((interactions (llm-chat-prompt-interactions prompt))
                            (last-message
                             (llm-chat-prompt-interaction-content (car (last interactions)))))
                       (should (string-match-p "summarize_state" last-message))
                       (should (string-match-p "what you finished" last-message))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-status-reminder-skips-recent-status ()
             "A recent status update does not add another reminder."
             (let ((buf (get-buffer-create "*ekg-agent-test-no-reminder*"))
                   (prompt (llm-make-chat-prompt "Work on the task."))
                   (ekg-agent-status-reminder-seconds 60)
                   (ekg-agent--current-log-buffer nil))
               (unwind-protect
                   (let ((ekg-agent--current-log-buffer buf))
                     (with-current-buffer buf
                       (setq ekg-agent--running-p t)
                       (setq ekg-agent--last-status-update-time 100.0)
                       (setq ekg-agent--last-status-reminder-time nil))
                     (cl-letf (((symbol-function 'float-time)
                                (lambda (&optional _time) 150.0)))
                       (should-not (ekg-agent--maybe-remind-status-update prompt)))
                     (should (= 1 (length (llm-chat-prompt-interactions prompt))))
                     (with-current-buffer buf
                       (should-not ekg-agent--last-status-reminder-time)))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-status-reminder-repeats-if-ignored ()
             "If the model ignores a reminder, another one is sent later."
             (let ((buf (get-buffer-create "*ekg-agent-test-repeat-reminder*"))
                   (prompt (llm-make-chat-prompt "Work on the task."))
                   (ekg-agent-status-reminder-seconds 60)
                   (ekg-agent--current-log-buffer nil))
               (unwind-protect
                   (let ((ekg-agent--current-log-buffer buf))
                     (with-current-buffer buf
                       (setq ekg-agent--running-p t)
                       (setq ekg-agent--last-status-update-time 100.0)
                       (setq ekg-agent--last-status-reminder-time 161.0))
                     (cl-letf (((symbol-function 'float-time)
                                (lambda (&optional _time) 221.0)))
                       (should (ekg-agent--maybe-remind-status-update prompt)))
                     (should (= 2 (length (llm-chat-prompt-interactions prompt))))
                     (with-current-buffer buf
                       (should (= 221.0 ekg-agent--last-status-reminder-time))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-repeat-read-only-guard-removed ()
             "Repeated read-only calls are no longer short-circuited by the wrapper."
             (should-not (fboundp 'ekg-agent--repeat-read-only-result))
             (should-not (boundp 'ekg-agent--repeatable-read-only-tools))
             (let ((log-buf (get-buffer-create "*ekg-agent-test-repeat-read-log*"))
                   (origin-buf (get-buffer-create "*ekg-agent-test-repeat-read-origin*"))
                   (calls 0))
               (unwind-protect
                   (let* ((tool (make-llm-tool
                                 :function (lambda (&rest _args)
                                             (cl-incf calls)
                                             (format "call-%d" calls))
                                 :name "read_buffer"
                                 :description "Test read tool."
                                 :args '()))
                          (wrapped (ekg-agent--wrap-tool-function
                                    tool log-buf origin-buf))
                          (fn (llm-tool-function wrapped)))
                     (with-current-buffer log-buf
                       (setq ekg-agent--tool-call-history nil))
                     (should (equal "call-1" (funcall fn)))
                     (should (equal "call-2" (funcall fn)))
                     (should (= calls 2)))
                 (kill-buffer log-buf)
                 (kill-buffer origin-buf))))

(ekg-deftest ekg-agent-test-export-debug-json-includes-conversation ()
             "The debug export includes prompt interactions and tool history."
             (let ((buf (get-buffer-create "*ekg-agent-test-debug-json*"))
                   (file (make-temp-file "ekg-agent-debug-test" nil ".json")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "2026-01-01 00:00:00 STARTED read_file\n")
                       (setq-local ekg-agent--prompt
                                   (llm-make-chat-prompt
                                    "Inspect the file."
                                    :context "Agent instructions."
                                    :tools (list
                                            (make-llm-tool
                                             :function (lambda (_path) "contents")
                                             :name "read_file"
                                             :description "Read a file."
                                             :args '((:name "path" :type string))))
                                    :tool-options
                                    (make-llm-tool-options :tool-choice 'any)))
                       (setf (llm-chat-prompt-interactions ekg-agent--prompt)
                             (append
                              (llm-chat-prompt-interactions ekg-agent--prompt)
                              (list
                               (make-llm-chat-prompt-interaction
                                :role 'assistant
                                :content
                                (list
                                 (make-llm-provider-utils-tool-use
                                  :id "call-1"
                                  :name "read_file"
                                  :args '((path . "/tmp/example.txt")))))
                               (make-llm-chat-prompt-interaction
                                :role 'tool-results
                                :tool-results
                                (list
                                 (make-llm-chat-prompt-tool-result
                                  :call-id "call-1"
                                  :tool-name "read_file"
                                  :result "file contents"))))))
                       (setq-local ekg-agent--end-tools '("end"))
                       (setq-local ekg-agent--tool-call-history
                                   (list (list :name "read_file"
                                               :args '("/tmp/example.txt")
                                               :result "file contents"
                                               :time 1700000000.0)))
                       (ekg-agent-export-debug-json file))
                     (let* ((json-object-type 'hash-table)
                            (json-array-type 'list)
                            (json-key-type 'string)
                            (data (json-read-file file))
                            (prompt (gethash "prompt" data))
                            (session (gethash "session" data))
                            (interactions (gethash "interactions" prompt))
                            (tools (gethash "tools" prompt))
                            (assistant (nth 1 interactions))
                            (tool-results (nth 2 interactions))
                            (history (gethash "tool_call_history" data)))
                       (should (equal "ekg-agent-debug-session"
                                      (gethash "schema" data)))
                       (should (= 1 (gethash "schema_version" data)))
                       (should (equal '("end") (gethash "end_tools" session)))
                       (should (equal "Inspect the file."
                                      (gethash "user_text" prompt)))
                       (should (equal "tool_uses"
                                      (gethash "content_type" assistant)))
                       (should (equal "read_file"
                                      (gethash
                                       "name"
                                       (car (gethash "tool_uses" assistant)))))
                       (should (equal "path"
                                      (gethash "name"
                                               (car (gethash "args" (car tools))))))
                       (should (equal "file contents"
                                      (gethash
                                       "result"
                                       (car (gethash "tool_results" tool-results)))))
                       (should (equal "read_file"
                                      (gethash "name" (car history))))))
                 (when (buffer-live-p buf)
                   (kill-buffer buf))
                 (when (file-exists-p file)
                   (delete-file file)))))

(ekg-deftest ekg-agent-test-log-mode-debug-export-keybinding ()
             "The agent log exposes the debug export command."
             (should (eq (lookup-key ekg-agent-log-mode-map (kbd "C-c C-d"))
                         #'ekg-agent-export-debug-json)))

;; Line ID generation

(ekg-deftest ekg-agent-test-line-ids-unique ()
             "Line IDs for different lines of the same file are unique."
             (let ((ids (mapcar (lambda (n) (ekg-agent--line-id "/tmp/test.txt" n))
                                (number-sequence 1 100))))
               (should (= (length ids)
                          (length (delete-dups (copy-sequence ids)))))))

(ekg-deftest ekg-agent-test-line-ids-three-chars ()
             "Every line ID is exactly 3 characters."
             (let ((ids (mapcar (lambda (n) (ekg-agent--line-id "/tmp/test.txt" n))
                                (number-sequence 1 50))))
               (should (cl-every (lambda (id) (= 3 (length id))) ids))))

(ekg-deftest ekg-agent-test-line-ids-deterministic ()
             "The same path and line number always produce the same ID."
             (should (equal (ekg-agent--line-id "/tmp/test.txt" 42)
                            (ekg-agent--line-id "/tmp/test.txt" 42))))

;; File read/edit round-trip

(ekg-deftest ekg-agent-test-read-file-with-line-ids ()
             "Reading a file returns content prefixed with 3-char identifiers."
             (let ((path (make-temp-file "ekg-agent-test")))
               (unwind-protect
                   (progn
                     (with-temp-file path
                       (insert "alpha\nbeta\ngamma\n"))
                     (let ((result (ekg-agent--read-file path)))
                       (should (stringp result))
                       (dolist (line (split-string result "\n"))
                         (when (not (string-empty-p line))
                           (should (string-match "\\`...: " line))))))
                 (delete-file path))))

(ekg-deftest ekg-agent-test-read-file-from-buffer ()
             "Reading a file that is open in a buffer reflects buffer content."
             (let ((path (make-temp-file "ekg-agent-test")))
               (unwind-protect
                   (progn
                     (with-temp-file path (insert "disk content\n"))
                     (let ((buf (find-file-noselect path)))
                       (unwind-protect
                           (progn
                             (with-current-buffer buf
                               (erase-buffer)
                               (insert "buffer content\n"))
                             (let ((result (ekg-agent--read-file path)))
                               (should (string-match-p "buffer content" result))))
                         (kill-buffer buf))))
                 (delete-file path))))

(ekg-deftest ekg-agent-test-read-file-empty-string-args ()
             "Empty strings for begin/end/range-type are treated as nil."
             (let ((path (make-temp-file "ekg-agent-test")))
               (unwind-protect
                   (progn
                     (with-temp-file path
                       (insert "alpha\nbeta\ngamma\n"))
                     (let ((full (ekg-agent--read-file path))
                           (empty-args (ekg-agent--read-file path "" "" "line_number")))
                       (should (string= full empty-args))
                       (should (string-match-p "alpha" full))
                       (should (string-match-p "gamma" full))))
                 (delete-file path))))

(ekg-deftest ekg-agent-test-write-file-creates-new ()
             "write_file creates a new file and returns content with line ids."
             (let ((path (concat (make-temp-file "ekg-agent-test" t) "/new-file.txt")))
               (unwind-protect
                   (progn
                     (should-not (file-exists-p path))
                     (let ((result (ekg-agent--write-file path "hello\nworld\n")))
                       (should (file-exists-p path))
                       (should (string-match-p "hello" result))
                       (should (string-match-p "world" result))
                       (should (equal "hello\nworld\n"
                                      (with-temp-buffer
                                        (insert-file-contents path)
                                        (buffer-string))))))
                 (when (file-exists-p path)
                   (delete-file path)))))

(ekg-deftest ekg-agent-test-write-file-overwrites-existing ()
             "write_file replaces the content of an existing file."
             (let ((path (make-temp-file "ekg-agent-test")))
               (unwind-protect
                   (progn
                     (with-temp-file path (insert "old content\n"))
                     (ekg-agent--write-file path "new content\n")
                     (should (equal "new content\n"
                                    (with-temp-buffer
                                      (insert-file-contents path)
                                      (buffer-string)))))
                 (delete-file path))))

(ekg-deftest ekg-agent-test-write-file-updates-buffer ()
             "write_file updates an open buffer instead of writing to disk."
             (let ((path (make-temp-file "ekg-agent-test")))
               (unwind-protect
                   (let ((buf (find-file-noselect path)))
                     (unwind-protect
                         (progn
                           (ekg-agent--write-file path "buffer content\n")
                           (should (equal "buffer content\n"
                                          (with-current-buffer buf
                                            (buffer-substring-no-properties
                                             (point-min) (point-max))))))
                       (kill-buffer buf)))
                 (delete-file path))))

(ekg-deftest ekg-agent-test-edit-file-round-trip ()
             "Editing a file replaces the identified region and returns context."
             (let ((path (make-temp-file "ekg-agent-test")))
               (unwind-protect
                   (progn
                     (with-temp-file path
                       (insert "line one\nline two\nline three\n"))
                     (let* ((output (ekg-agent--read-file path))
                            (lines (split-string output "\n"))
                            (id1 (substring (nth 0 lines) 0 3))
                            (id2 (substring (nth 1 lines) 0 3)))
                       (ekg-agent--edit-file path id1 "line one" id2 "line two" "replaced")
                       (with-temp-buffer
                         (insert-file-contents path)
                         (should (string-match-p "replaced" (buffer-string)))
                         (should-not (string-match-p "line one" (buffer-string)))
                         (should-not (string-match-p "line two" (buffer-string)))
                         (should (string-match-p "line three" (buffer-string))))))
                 (delete-file path))))

(ekg-deftest ekg-agent-test-edit-file-adjusts-indentation ()
             "Editing a file corrects over-indented replacement text."
             (let ((path (make-temp-file "ekg-agent-test")))
               (unwind-protect
                   (progn
                     (with-temp-file path
                       (insert "  hello world\n  goodbye world\n"))
                     (let* ((output (ekg-agent--read-file path))
                            (lines (split-string output "\n"))
                            (id1 (substring (nth 0 lines) 0 3)))
                       ;; Replace with over-indented text (8 spaces instead of 2)
                       (ekg-agent--edit-file path id1 "hello world" id1 "hello world"
                                             "        new content")
                       (with-temp-buffer
                         (insert-file-contents path)
                         ;; Should be adjusted to 2-space indent
                         (should (string-match-p "^  new content" (buffer-string))))))
                 (delete-file path))))

(ekg-deftest ekg-agent-test-edit-file-tool-large-output-hidden-buffer ()
             "Large edit_file tool output is truncated into a hidden buffer."
             (let ((path (make-temp-file "ekg-agent-test"))
                   (log-buf (get-buffer-create "*ekg-agent-test-large-edit-log*"))
                   (ekg-agent-tool-result-max-output-chars 80))
               (unwind-protect
                   (let ((ekg-agent--current-log-buffer log-buf))
                     (with-temp-file path
                       (dotimes (i 80)
                         (insert (format "line-%02d with enough text for output\n" i))))
                     (with-current-buffer log-buf
                       (setq ekg-agent--hidden-result-buffers nil))
                     (let* ((truepath (file-truename path))
                            (id1 (ekg-agent--line-id truepath 1))
                            (id60 (ekg-agent--line-id truepath 60))
                            (result (ekg-agent--edit-file-tool
                                     path id1 "line-00" id60 "line-59"
                                     "replacement")))
                       (should (string-match-p "edit_file output truncated at 80"
                                               result))
                       (should (string-match "hidden buffer `\\([^`]+\\)`" result))
                       (let ((buffer-name (match-string 1 result)))
                         (should (get-buffer buffer-name))
                         (ekg-agent--cleanup-hidden-result-buffers log-buf)
                         (should-not (get-buffer buffer-name)))))
                 (when (file-exists-p path)
                   (delete-file path))
                 (when (buffer-live-p log-buf)
                   (kill-buffer log-buf)))))

;; Agent integration tests
;;
;; These simulate the agent loop by mocking llm-chat-async to make
;; predetermined tool calls, then verify that tools execute correctly
;; and the agent loop terminates as expected.

(require 'llm-fake)

(defun ekg-agent-test--extract-id (read-output line-index)
  "Extract the 3-char line identifier from READ-OUTPUT at LINE-INDEX."
  (substring (nth line-index (split-string read-output "\n")) 0 3))

(defun ekg-agent-test--make-tool-response (tool-calls)
  "Create a mock llm-chat-async that executes TOOL-CALLS in sequence.
TOOL-CALLS is a list of lists, each being a sequence of (tool-name . args)
pairs for one iteration.  Each call to the mock pops the next iteration
and executes the tool functions found in the prompt."
  (let ((remaining (copy-sequence tool-calls)))
    (lambda (_provider prompt response-callback _error-callback &optional _multi-output)
      (let* ((calls (pop remaining))
             (tools (llm-chat-prompt-tools prompt))
             (results
              (mapcar
               (lambda (call)
                 (let* ((name (car call))
                        (args (cdr call))
                        (tool (seq-find (lambda (tl)
                                          (equal (llm-tool-name tl) name))
                                        tools)))
                   (unless tool
                     (error "Tool %s not found in prompt" name))
                   (let ((result (apply (llm-tool-function tool) args)))
                     (cons name (if (stringp result) result
                                  (format "%s" result))))))
               calls)))
        (funcall response-callback (list :tool-results results))))))

(defmacro ekg-agent-test--with-mock-agent (tool-call-sequence &rest body)
  "Run BODY with the agent loop mocked to execute TOOL-CALL-SEQUENCE.
Each element of TOOL-CALL-SEQUENCE is one iteration's worth of
tool calls.  `ekg-agent--prompt-id' is stubbed to return a fixed
name.  The provider is set to a fake.  The test should wait on
the `done-flag' variable which is set to the status callback
result when the agent finishes."
  (declare (indent 1) (debug t))
  `(let ((ekg-llm-provider (make-llm-fake))
         (done-flag nil)
         (mock-fn (ekg-agent-test--make-tool-response ,tool-call-sequence)))
     (cl-letf (((symbol-function 'ekg-agent--prompt-id)
                (lambda (_) "test-agent"))
               ((symbol-function 'ekg-agent--prompt-id-async)
                #'ignore)
               ((symbol-function 'llm-chat-async)
                mock-fn)
               ((symbol-function 'ekg-agent--ensure-log-window)
                #'ignore))
       ,@body)
     ;; Clean up log buffer
     (when-let ((buf (get-buffer (format ekg-agent-log-buffer-name-format
                                         "test-agent"))))
       (kill-buffer buf))))

(defun ekg-agent-test--dummy-tool (name)
  "Return a no-op test tool named NAME."
  (make-llm-tool :function (lambda (&rest _args) "ok")
                 :name name
                 :description "Test-only tool."
                 :args '()))

(ekg-deftest ekg-agent-test-clean-orphaned-tool-interaction ()
             "Trailing assistant tool-use interactions are removed for recovery."
             (let ((prompt (llm-make-chat-prompt "Do work.")))
               (setf (llm-chat-prompt-interactions prompt)
                     (append
                      (llm-chat-prompt-interactions prompt)
                      (list
                       (make-llm-chat-prompt-interaction
                        :role 'assistant
                        :content
                        (list
                         (make-llm-provider-utils-tool-use
                          :id "call-1"
                          :name "add_to_load_path"
                          :args '((directory . "/tmp/pkg"))))))))
               (should (ekg-agent--clean-orphaned-tool-interactions prompt))
               (should (= 1 (length (llm-chat-prompt-interactions prompt))))
               (should-not (ekg-agent--clean-orphaned-tool-interactions prompt))))

(ekg-deftest ekg-agent-test-recovers-from-unknown-tool-error ()
             "Unknown tool provider errors are converted into a continuation."
             (let ((ekg-llm-provider (make-llm-fake))
                   (prompt (llm-make-chat-prompt
                            "Recover from a bad tool call."
                            :tools (list ekg-agent-tool-end
                                         ekg-agent-tool-run-elisp)
                            :tool-options (make-llm-tool-options :tool-choice 'any)))
                   (done-flag nil)
                   (calls 0))
               (cl-letf (((symbol-function 'ekg-agent--prompt-id)
                          (lambda (_) "test-agent"))
                         ((symbol-function 'ekg-agent--prompt-id-async)
                          #'ignore)
                         ((symbol-function 'llm-chat-async)
                          (lambda (_provider active-prompt response-callback
                                             error-callback &optional _multi-output)
                            (cl-incf calls)
                            (if (= calls 1)
                                (progn
                                  (setf (llm-chat-prompt-interactions active-prompt)
                                        (append
                                         (llm-chat-prompt-interactions active-prompt)
                                         (list
                                          (make-llm-chat-prompt-interaction
                                           :role 'assistant
                                           :content
                                           (list
                                            (make-llm-provider-utils-tool-use
                                             :id "call-bad"
                                             :name "add_to_load_path"
                                             :args '((directory . "/tmp/pkg"))))))))
                                  (funcall error-callback
                                           'error
                                           "Unknown tool 'add_to_load_path' called"))
                              (funcall response-callback
                                       (list :tool-results
                                             (list (cons "end" "recovered")))))
                            nil))
                         ((symbol-function 'ekg-agent--ensure-log-window)
                          #'ignore))
                 (ekg-agent--iterate prompt
                                     0
                                     (lambda (status) (setq done-flag status))
                                     '("end")))
               (should (equal "recovered" done-flag))
               (should (= 2 calls))
               (let* ((interactions (llm-chat-prompt-interactions prompt))
                      (last-message
                       (llm-chat-prompt-interaction-content (car (last interactions)))))
                 (should (string-match-p "unavailable tool `add_to_load_path'"
                                         last-message))
                 (should (string-match-p "run_elisp" last-message))
                 (should-not
                  (seq-some
                   (lambda (interaction)
                     (and (eq (llm-chat-prompt-interaction-role interaction)
                              'assistant)
                          (not (stringp
                                (llm-chat-prompt-interaction-content interaction)))))
                   interactions))))
             (let ((buf (get-buffer (format ekg-agent-log-buffer-name-format
                                            "test-agent"))))
               (when buf
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-run-elisp-description-documents-state-changes ()
             "The run_elisp description advertises key recoverable uses."
             (let ((description (llm-tool-description ekg-agent-tool-run-elisp)))
               (should (string-match-p "test out elisp" description))
               (should (string-match-p "load-path" description))
               (should (string-match-p "requiring libraries" description))
               (should (string-match-p "void-function" description))
               (should (string-match-p "locate-library" description))))

(ekg-deftest ekg-agent-test-run-elisp-does-not-finish-before-end-tool ()
             "A diagnostic tool result should not stop the loop before an end tool."
             (let ((run-elisp-calls 0)
                   status)
               (ekg-agent-test--with-mock-agent
                (list
                 (list (cons "run_elisp" nil))
                 (list (cons "end" nil)))
                (ekg-agent--iterate
                 (llm-make-chat-prompt
                  "Verify with run_elisp."
                  :tools (list
                          (make-llm-tool
                           :function (lambda ()
                                       (cl-incf run-elisp-calls)
                                       "ok")
                           :name "run_elisp"
                           :description "Test-only diagnostic tool."
                           :args '())
                          ekg-agent-tool-end)
                  :tool-options (make-llm-tool-options :tool-choice 'any))
                 0
                 (lambda (status) (setq done-flag status))
                 '("end")
                 nil
                 nil)
                (setq status done-flag))
               (should (= run-elisp-calls 1))
               (should (equal status "done"))))

(ekg-deftest ekg-agent-test-agent-reads-and-edits-file ()
             "The agent loop reads a file, edits it, and ends."
             (let ((path (make-temp-file "ekg-agent-int-test")))
               (unwind-protect
                   (progn
                     (with-temp-file path
                       (insert "def greet():\n    print(\"hello\")\n    return True\n"))
                     ;; Pre-compute the line IDs so we know what the agent
                     ;; would see after calling read_file.
                     (let* ((read-output (ekg-agent--read-file path))
                            (id1 (ekg-agent-test--extract-id read-output 0))
                            (id2 (ekg-agent-test--extract-id read-output 1)))
                       ;; The agent will: 1) read the file, 2) edit line 2, 3) end.
                       (ekg-agent-test--with-mock-agent
                        (list
                         ;; Iteration 1: read the file
                         (list (cons "read_file" (list path)))
                         ;; Iteration 2: edit the greeting
                         (list (cons "edit_file"
                                     (list path id2 "print(\"hello\")"
                                           id2 "print(\"hello\")"
                                           "    print(\"goodbye\")")))
                         ;; Iteration 3: done
                         (list (cons "end" nil)))
                        (ekg-agent--iterate
                         (llm-make-chat-prompt
                          "Test: read and edit a file."
                          :tools (append ekg-agent-base-tools
                                         (list ekg-agent-tool-end))
                          :tool-options (make-llm-tool-options :tool-choice 'any))
                         0
                         (lambda (status) (setq done-flag status))
                         '("end")))
                       ;; Verify the edit happened
                       (with-temp-buffer
                         (insert-file-contents path)
                         (should (string-match-p "goodbye" (buffer-string)))
                         (should-not (string-match-p "\"hello\"" (buffer-string)))
                         (should (string-match-p "def greet" (buffer-string)))
                         (should (string-match-p "return True" (buffer-string))))))
                 (delete-file path))))

(ekg-deftest-with-db ekg-agent-test-agent-creates-note ()
                     (ekg-agent-test--with-mock-agent
                      (list
                       ;; Iteration 1: create a note
                       (list (cons "create_note"
                                   (list ["test-tag" "agent"] "Test note content" "org-mode")))
                       ;; Iteration 2: done
                       (list (cons "end" nil)))
                      (ekg-agent--iterate
                       (llm-make-chat-prompt
                        "Test: create a note."
                        :tools (append ekg-agent-base-tools
                                       (list ekg-agent-tool-end))
                        :tool-options (make-llm-tool-options :tool-choice 'any))
                       0
                       (lambda (status) (setq done-flag status))
                       '("end")))
                     (let ((notes (ekg-get-notes-with-tags '("test-tag"))))
                       (should (= 1 (length notes)))
                       (should (string-match-p "Test note content" (ekg-note-text (car notes))))))

(ekg-deftest-with-db ekg-agent-test-append-to-note ()
                     "Appending to a note adds content without log contamination."
                     (let* ((note (ekg-note-create :text "Original content."
                                                   :mode 'org-mode
                                                   :tags '("append-test")))
                            (_ (ekg-save-note note))
                            (note-id (format "%s" (ekg-note-id note))))
                       (ekg-agent-test--with-mock-agent
                        (list
                         ;; Iteration 1: append to the note
                         (list (cons "append_to_note"
                                     (list note-id "Appended paragraph.")))
                         ;; Iteration 2: append more
                         (list (cons "append_to_note"
                                     (list note-id "Second appended paragraph.")))
                         ;; Iteration 3: done
                         (list (cons "end" nil)))
                        (ekg-agent--iterate
                         (llm-make-chat-prompt
                          "Test: append to a note."
                          :tools (append ekg-agent-base-tools
                                         (list ekg-agent-tool-end))
                          :tool-options (make-llm-tool-options :tool-choice 'any))
                         0
                         (lambda (status) (setq done-flag status))
                         '("end")))
                       (let* ((updated (ekg-get-note-with-id (ekg-note-id note)))
                              (text (ekg-note-text updated)))
                         (should (string-match-p "Original content" text))
                         (should (string-match-p "Appended paragraph" text))
                         (should (string-match-p "Second appended paragraph" text))
                         ;; Agent log artifacts must not leak into the note.
                         (should-not (string-match-p "STARTED" text))
                         (should-not (string-match-p " DONE " text))
                         (should-not (string-match-p "Waiting for LLM" text))
                         (should-not (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" text)))))

(ekg-deftest ekg-agent-test-agent-run-command ()
             "The run_command tool executes a shell command and returns output."
             (let (result)
               (ekg-agent--run-command (lambda (r) (setq result r)) "echo hello-world")
               (while (not result) (accept-process-output nil 0.1))
               (should (string-match-p "Exit code: 0" result))
               (should (string-match-p "hello-world" result))))

(ekg-deftest ekg-agent-test-agent-run-command-failure ()
             "The run_command tool reports non-zero exit codes."
             (let (result)
               (ekg-agent--run-command (lambda (r) (setq result r)) "exit 42")
               (while (not result) (accept-process-output nil 0.1))
               (should (string-match-p "Exit code: 42" result))))

(ekg-deftest ekg-agent-test-agent-run-command-timeout ()
             "The run_command tool times out long-running commands."
             (let ((ekg-agent-run-command-timeout-seconds 0.1)
                   result)
               (ekg-agent--run-command (lambda (r) (setq result r)) "sleep 2")
               (while (not result) (accept-process-output nil 0.1))
               (should (string-match-p "Command timed out" result))))

(ekg-deftest ekg-agent-test-agent-run-command-large-output-hidden-buffer ()
             "Large run_command output is truncated and stored in a hidden buffer."
             (let ((log-buf (get-buffer-create "*ekg-agent-test-hidden-log*"))
                   (ekg-agent-run-command-max-output-chars 50)
                   result)
               (unwind-protect
                   (let ((ekg-agent--current-log-buffer log-buf))
                     (with-current-buffer log-buf
                       (setq ekg-agent--hidden-result-buffers nil))
                     (ekg-agent--run-command
                      (lambda (r) (setq result r))
                      "printf '%0200d' 0")
                     (while (not result) (accept-process-output nil 0.1))
                     (should (string-match-p "output truncated at 50" result))
                     (should (string-match "hidden buffer `\\([^`]+\\)`" result))
                     (let* ((buffer-name (match-string 1 result))
                            (hidden-buf (get-buffer buffer-name)))
                       (should hidden-buf)
                       (with-current-buffer hidden-buf
                         (should (> (buffer-size) 200)))
                       (let ((status (ekg-agent--status-with-hidden-buffer-cleanup
                                      "done")))
                         (should (string-match-p "Deleted hidden result buffers"
                                                 status))
                         (should-not (get-buffer buffer-name)))))
                 (when (buffer-live-p log-buf)
                   (kill-buffer log-buf)))))

;; Buffer line ID generation

(ekg-deftest ekg-agent-test-buffer-line-ids-unique ()
             "Buffer line IDs for different lines of the same buffer are unique."
             (let ((ids (mapcar (lambda (n) (ekg-agent--buffer-line-id "*test-buf*" n))
                                (number-sequence 1 100))))
               (should (= (length ids)
                          (length (delete-dups (copy-sequence ids)))))))

(ekg-deftest ekg-agent-test-buffer-line-ids-three-chars ()
             "Every buffer line ID is exactly 3 characters."
             (let ((ids (mapcar (lambda (n) (ekg-agent--buffer-line-id "*test-buf*" n))
                                (number-sequence 1 50))))
               (should (cl-every (lambda (id) (= 3 (length id))) ids))))

(ekg-deftest ekg-agent-test-buffer-line-ids-deterministic ()
             "The same buffer name and line number always produce the same ID."
             (should (equal (ekg-agent--buffer-line-id "*test*" 42)
                            (ekg-agent--buffer-line-id "*test*" 42))))

(ekg-deftest ekg-agent-test-buffer-line-ids-differ-from-file ()
             "Buffer line IDs differ from file line IDs for the same line number."
             (should-not (equal (ekg-agent--buffer-line-id "/tmp/test.txt" 1)
                                (ekg-agent--line-id "/tmp/test.txt" 1))))

;; List buffers

(ekg-deftest ekg-agent-test-list-buffers ()
             "Listing buffers returns visible buffers with metadata."
             (let ((buf (get-buffer-create "*ekg-agent-test-list*")))
               (unwind-protect
                   (let ((result (ekg-agent--list-buffers)))
                     (should (stringp result))
                     (should (string-match-p "\\*ekg-agent-test-list\\*" result)))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-list-buffers-with-filter ()
             "Listing buffers with a regex filter returns only matching buffers."
             (let ((buf1 (get-buffer-create "*ekg-agent-test-alpha*"))
                   (buf2 (get-buffer-create "*ekg-agent-test-beta*")))
               (unwind-protect
                   (let ((result (ekg-agent--list-buffers "alpha")))
                     (should (string-match-p "alpha" result))
                     (should-not (string-match-p "beta" result)))
                 (kill-buffer buf1)
                 (kill-buffer buf2))))

(ekg-deftest ekg-agent-test-list-buffers-excludes-internal ()
             "Internal buffers (names starting with space) are excluded."
             (let ((buf (get-buffer-create " *ekg-agent-internal*")))
               (unwind-protect
                   (let ((result (ekg-agent--list-buffers "ekg-agent-internal")))
                     (should (equal "No buffers found." result)))
                 (kill-buffer buf))))

;; Read buffer

(ekg-deftest ekg-agent-test-read-buffer-with-line-ids ()
             "Reading a buffer returns content with identifiers and range metadata."
             (let ((buf (get-buffer-create "*ekg-agent-test-read*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "alpha\nbeta\ngamma\n"))
                     (let ((result (ekg-agent--read-buffer "*ekg-agent-test-read*")))
                       (should (stringp result))
                       (should (string-match-p "^begin_percent: 0.0%" result))
                       (should (string-match-p "end_percent: 100.0%" result))
                       (dolist (line (cdr (split-string result "\n")))
                         (when (not (string-empty-p line))
                           (should (string-match "\\`...: " line))))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-read-buffer-range-by-line-number ()
             "Reading a buffer with line number range returns only that range."
             (let ((buf (get-buffer-create "*ekg-agent-test-range*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "line1\nline2\nline3\nline4\n"))
                     (let ((result (ekg-agent--read-buffer "*ekg-agent-test-range*"
                                                           "2" "3" "line_number")))
                       (should (string-match-p "line2" result))
                       (should (string-match-p "line3" result))
                       (should-not (string-match-p "line1" result))
                       (should-not (string-match-p "line4" result))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-read-buffer-range-by-identifier ()
             "Reading a buffer with identifier range returns the correct range."
             (let ((buf (get-buffer-create "*ekg-agent-test-id-range*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "aaa\nbbb\nccc\nddd\n"))
                     (let* ((full (ekg-agent--read-buffer "*ekg-agent-test-id-range*"))
                            (content-lines (cdr (split-string full "\n")))
                            (id2 (substring (nth 1 content-lines) 0 3))
                            (id3 (substring (nth 2 content-lines) 0 3))
                            (result (ekg-agent--read-buffer "*ekg-agent-test-id-range*"
                                                            id2 id3 "identifier")))
                       (should (string-match-p "bbb" result))
                       (should (string-match-p "ccc" result))
                       (should-not (string-match-p "aaa" result))
                       (should-not (string-match-p "ddd" result))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-read-buffer-returns-range-metadata ()
             "The percentages in read_buffer output match the buffer range."
             (let ((buf (get-buffer-create "*ekg-agent-test-pos*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "hello\nworld\n"))
                     (let ((result (ekg-agent--read-buffer "*ekg-agent-test-pos*"
                                                           "2" "2" "line_number")))
                       (should (string-match-p "begin_percent: 50.0%" result))
                       (should (string-match-p "end_percent: 91.7%" result))
                       (should-not (string-match-p "begin_pos:" result))
                       (should-not (string-match-p "end_pos:" result))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-read-empty-buffer-returns-percentages ()
             "Reading an empty buffer reports defined percentages."
             (let ((buf (get-buffer-create "*ekg-agent-test-empty-read*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer))
                     (let ((result (ekg-agent--read-buffer
                                    "*ekg-agent-test-empty-read*")))
                       (should (string-match-p "begin_percent: 0.0%" result))
                       (should (string-match-p "end_percent: 0.0%" result))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-search-buffer-finds-targeted-lines ()
             "Searching a buffer returns matching lines without dumping the buffer."
             (let ((buf (get-buffer-create "*ekg-agent-test-search*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "alpha\nneedle one\nbeta\nneedle two\ngamma\n"))
                     (let ((result (ekg-agent--search-buffer
                                    "*ekg-agent-test-search*" "needle" 10 0)))
                       (should (string-match-p "Found 2 matches" result))
                       (should (string-match-p ">...: needle one" result))
                       (should (string-match-p ">...: needle two" result))
                       (should-not (string-match-p "alpha" result))
                       (should-not (string-match-p "gamma" result))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-read-buffer-large-output-line-truncation ()
             "Large read_buffer tool output is truncated by line count."
             (let ((buf (get-buffer-create "*ekg-agent-test-large-read*"))
                   (log-buf (get-buffer-create "*ekg-agent-test-large-read-log*"))
                   (ekg-agent-read-buffer-max-lines 3))
               (unwind-protect
                   (let ((ekg-agent--current-log-buffer log-buf))
                     (with-current-buffer buf
                       (erase-buffer)
                       (dotimes (i 30)
                         (insert (format "line-%02d with enough text\n" i))))
                     (with-current-buffer log-buf
                       (setq ekg-agent--hidden-result-buffers nil))
                     (let ((result (ekg-agent--read-buffer-tool
                                    "*ekg-agent-test-large-read*")))
                       (should (string-match-p "read_buffer output truncated: showing lines 1-3"
                                               result))
                       (should (string-match-p "line-00" result))
                       (should (string-match-p "line-02" result))
                       (should-not (string-match-p "line-03" result))
                       (should-not (string-match-p "hidden buffer" result))
                       (should-not ekg-agent--hidden-result-buffers)))
                 (when (buffer-live-p buf)
                   (kill-buffer buf))
                 (when (buffer-live-p log-buf)
                   (kill-buffer log-buf)))))

(ekg-deftest ekg-agent-test-read-buffer-nonexistent ()
             "Reading a nonexistent buffer returns an error string."
             (let ((result (ekg-agent--read-buffer "*no-such-buffer-exists*")))
               (should (string-match-p "Error:" result))))

;; Edit buffer

(ekg-deftest ekg-agent-test-edit-buffer-round-trip ()
             "Editing a buffer replaces the identified region and returns context."
             (let ((buf (get-buffer-create "*ekg-agent-test-edit*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "line one\nline two\nline three\n"))
                     (let* ((output (ekg-agent--read-buffer "*ekg-agent-test-edit*"))
                            (content-lines (cdr (split-string output "\n")))
                            (id1 (substring (nth 0 content-lines) 0 3))
                            (id2 (substring (nth 1 content-lines) 0 3)))
                       (ekg-agent--edit-buffer "*ekg-agent-test-edit*"
                                               id1 "line one" id2 "line two" "replaced")
                       (with-current-buffer buf
                         (should (string-match-p "replaced" (buffer-string)))
                         (should-not (string-match-p "line one" (buffer-string)))
                         (should-not (string-match-p "line two" (buffer-string)))
                         (should (string-match-p "line three" (buffer-string))))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-edit-buffer-adjusts-indentation ()
             "Editing a buffer corrects over-indented replacement text."
             (let ((buf (get-buffer-create "*ekg-agent-test-edit-indent*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "  hello world\n  goodbye world\n"))
                     (let* ((output (ekg-agent--read-buffer "*ekg-agent-test-edit-indent*"))
                            (content-lines (cdr (split-string output "\n")))
                            (id1 (substring (nth 0 content-lines) 0 3)))
                       (ekg-agent--edit-buffer "*ekg-agent-test-edit-indent*"
                                               id1 "hello world" id1 "hello world"
                                               "        new content")
                       (with-current-buffer buf
                         (should (string-match-p "^  new content" (buffer-string))))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-edit-buffer-nonexistent ()
             "Editing a nonexistent buffer returns an error string."
             (let ((result (ekg-agent--edit-buffer "*no-such-buffer*"
                                                   "abc" "text" "abc" "text" "new")))
               (should (string-match-p "Error:" result))))

(ekg-deftest ekg-agent-test-edit-buffer-error-shows-current-line ()
             "Boundary mismatch errors include the current line text for recovery."
             (let ((buf (get-buffer-create "*ekg-agent-test-edit-error-line*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "alpha\nactual end line\n"))
                     (let* ((output (ekg-agent--read-buffer
                                     "*ekg-agent-test-edit-error-line*"))
                            (content-lines (cdr (split-string output "\n")))
                            (id1 (substring (nth 0 content-lines) 0 3))
                            (id2 (substring (nth 1 content-lines) 0 3))
                            (result (ekg-agent--edit-buffer
                                     "*ekg-agent-test-edit-error-line*"
                                     id1 "alpha" id2 "missing end text" "new")))
                       (should (string-match-p "End text not found" result))
                       (should (string-match-p "actual end line" result))))
                 (kill-buffer buf))))

;; Run interactive command

(ekg-deftest ekg-agent-test-run-interactive-command-with-point ()
             "Running an interactive command at a point position works."
             (let ((buf (get-buffer-create "*ekg-agent-test-cmd*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "hello world"))
                     (let ((result (ekg-agent--run-interactive-command
                                    "*ekg-agent-test-cmd*" "upcase-word"
                                    "1" nil nil)))
                       (should (stringp result))
                       (with-current-buffer buf
                         (should (string-match-p "HELLO" (buffer-string))))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-run-interactive-command-with-line-id ()
             "Running an interactive command via line identifier works."
             (let ((buf (get-buffer-create "*ekg-agent-test-cmd-id*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "first line\nsecond line\n"))
                     (let* ((output (ekg-agent--read-buffer "*ekg-agent-test-cmd-id*"))
                            (content-lines (cdr (split-string output "\n")))
                            (id2 (substring (nth 1 content-lines) 0 3)))
                       (ekg-agent--run-interactive-command
                        "*ekg-agent-test-cmd-id*" "upcase-word"
                        nil id2 "second")
                       (with-current-buffer buf
                         (should (string-match-p "SECOND" (buffer-string)))
                         (should (string-match-p "first" (buffer-string))))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-run-interactive-command-returns-context ()
             "The interactive command tool returns buffer content around the point."
             (let ((buf (get-buffer-create "*ekg-agent-test-cmd-ctx*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "line1\nline2\nline3\n"))
                     (let ((result (ekg-agent--run-interactive-command
                                    "*ekg-agent-test-cmd-ctx*" "forward-char"
                                    "1" nil nil)))
                       (should (string-match-p "begin_percent:" result))
                       (should (string-match-p "end_percent:" result))
                       (should-not (string-match-p "begin_pos:" result))
                       (should-not (string-match-p "end_pos:" result))
                       (should (string-match-p "line1" result))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-run-interactive-command-post-move-context ()
             "Context is centered on the post-command point, not the original position."
             (let ((buf (get-buffer-create "*ekg-agent-test-cmd-move*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       ;; 20 lines so we can distinguish which lines are returned.
                       (dotimes (i 20)
                         (insert (format "line-%02d\n" (1+ i)))))
                     ;; Start at line 1 (point 1), run forward-line which moves to line 2.
                     ;; With 20 lines, if context is centered on post-command point
                     ;; (line 2), we should NOT see line-15 through line-20.
                     (let ((result (ekg-agent--run-interactive-command
                                    "*ekg-agent-test-cmd-move*" "end-of-buffer"
                                    "1" nil nil)))
                       ;; end-of-buffer moves to the end; context should include
                       ;; the last lines, not the first ones.
                       (should (string-match-p "line-20" result))
                       ;; Line 1 should NOT be in a 10-line window around line 20.
                       (should-not (string-match-p "line-01" result))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-run-interactive-command-nonexistent-buffer ()
             "Running a command in a nonexistent buffer returns an error string."
             (let ((result (ekg-agent--run-interactive-command
                            "*no-such-buffer*" "forward-char" "1" nil nil)))
               (should (string-match-p "Error:" result))))

(ekg-deftest ekg-agent-test-run-interactive-command-bad-command ()
             "Running a non-interactive function returns an error string."
             (let ((buf (get-buffer-create "*ekg-agent-test-bad-cmd*")))
               (unwind-protect
                   (let ((result (ekg-agent--run-interactive-command
                                  "*ekg-agent-test-bad-cmd*" "not-a-real-command"
                                  "1" nil nil)))
                     (should (string-match-p "Error:" result)))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-resolve-buffer-point-precedence ()
             "When both point and line_id are given, point takes precedence."
             (let ((buf (get-buffer-create "*ekg-agent-test-precedence*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "abcdef\nghijkl\n"))
                     (let* ((output (ekg-agent--read-buffer "*ekg-agent-test-precedence*"))
                            (content-lines (cdr (split-string output "\n")))
                            (id2 (substring (nth 1 content-lines) 0 3))
                            ;; Point 3 is in "abcdef" (line 1), id2 is line 2.
                            ;; Point should win.
                            (resolved (ekg-agent--resolve-buffer-point
                                       "*ekg-agent-test-precedence*" "3" id2 nil)))
                       (should (= 3 resolved))))
                 (kill-buffer buf))))

;; Agent integration: buffer read + edit

(ekg-deftest ekg-agent-test-agent-reads-and-edits-buffer ()
             "The agent loop reads a buffer, edits it, and ends."
             (let ((buf (get-buffer-create "*ekg-agent-test-buf-int*")))
               (unwind-protect
                   (progn
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "def greet():\n    print(\"hello\")\n    return True\n"))
                     (let* ((read-output (ekg-agent--read-buffer "*ekg-agent-test-buf-int*"))
                            (content-lines (cdr (split-string read-output "\n")))
                            (id2 (substring (nth 1 content-lines) 0 3)))
                       (ekg-agent-test--with-mock-agent
                        (list
                         (list (cons "read_buffer" (list "*ekg-agent-test-buf-int*")))
                         (list (cons "edit_buffer"
                                     (list "*ekg-agent-test-buf-int*"
                                           id2 "print(\"hello\")"
                                           id2 "print(\"hello\")"
                                           "    print(\"goodbye\")")))
                         (list (cons "end" nil)))
                        (ekg-agent--iterate
                         (llm-make-chat-prompt
                          "Test: read and edit a buffer."
                          :tools (append ekg-agent-base-tools
                                         (list ekg-agent-tool-end))
                          :tool-options (make-llm-tool-options :tool-choice 'any))
                         0
                         (lambda (status) (setq done-flag status))
                         '("end")))
                       (with-current-buffer buf
                         (should (string-match-p "goodbye" (buffer-string)))
                         (should-not (string-match-p "\"hello\"" (buffer-string)))
                         (should (string-match-p "def greet" (buffer-string))))))
                 (kill-buffer buf))))

(ekg-deftest ekg-agent-test-iterate-uses-bound-log-buffer ()
             "A non-initial iteration logs to the bound log buffer, not current buffer."
             (let ((origin-buf (get-buffer-create "*ekg-agent-test-origin*"))
                   (log-buf (get-buffer-create "*ekg-agent-test-log*"))
                   (done-flag nil))
               (unwind-protect
                   (progn
                     (with-current-buffer origin-buf
                       (erase-buffer)
                       (insert "origin content\n"))
                     (with-current-buffer log-buf
                       (erase-buffer)
                       (setq ekg-agent--running-p t)
                       (setq ekg-agent--cancelled-p nil))
                     (let ((ekg-llm-provider (make-llm-fake))
                           (ekg-agent--current-log-buffer log-buf))
                       (cl-letf (((symbol-function 'llm-chat-async)
                                  (lambda (_provider _prompt response-callback
                                                     _error-callback &optional _multi-output)
                                    (funcall response-callback
                                             (list :tool-results
                                                   (list (cons "end" "ok"))))
                                    'mock-request)))
                         (with-current-buffer origin-buf
                           (ekg-agent--iterate
                            (llm-make-chat-prompt
                             "Test: continue from a non-log buffer."
                             :tools (list ekg-agent-tool-end)
                             :tool-options (make-llm-tool-options :tool-choice 'any))
                            1
                            (lambda (status) (setq done-flag status))
                            '("end")))))
                     (should (equal done-flag "ok"))
                     (with-current-buffer origin-buf
                       (should-not (string-match-p "Waiting for LLM response"
                                                   (buffer-string))))
                     (with-current-buffer log-buf
                       (should (string-match-p "Waiting for LLM response"
                                               (buffer-string)))))
                 (kill-buffer origin-buf)
                 (kill-buffer log-buf))))

;; Web rendering / browsing / search

(ekg-deftest ekg-agent-test-web-render-html-basic ()
             "Rendering simple HTML produces readable text."
             (let ((result (ekg-agent--web-render-html
                            "<html><body><p>Hello world</p></body></html>"
                            "https://example.com")))
               (should (stringp result))
               (should (string-match-p "Hello world" result))))

(ekg-deftest ekg-agent-test-web-render-html-truncation ()
             "Content exceeding `ekg-agent-web-max-chars' is truncated."
             (let* ((ekg-agent-web-max-chars 50)
                    (long-text (make-string 200 ?x))
                    (html (format "<html><body><p>%s</p></body></html>" long-text))
                    (result (ekg-agent--web-render-html html "https://example.com")))
               (should (<= (length result) (+ 50 100)))  ; truncated text + notice
               (should (string-match-p "\\[Content truncated" result))))

(ekg-deftest ekg-agent-test-web-render-html-empty ()
             "Rendering empty HTML returns a non-erroring result."
             (let ((result (ekg-agent--web-render-html
                            "<html><body></body></html>"
                            "https://example.com")))
               (should (stringp result))))

(ekg-deftest ekg-agent-test-web-browse-rejects-non-http ()
             "Non-http(s) URLs are rejected with an error message."
             (let (result)
               (ekg-agent--web-browse (lambda (r) (setq result r)) "ftp://example.com/file")
               (should (string-match-p "Error:.*Only http" result))))

(ekg-deftest ekg-agent-test-web-browse-rejects-file-url ()
             "file:// URLs are rejected."
             (let (result)
               (ekg-agent--web-browse (lambda (r) (setq result r)) "file:///etc/passwd")
               (should (string-match-p "Error:.*Only http" result))))

(ekg-deftest ekg-agent-test-web-browse-success ()
             "A successful fetch renders and returns page content."
             (let (result)
               (cl-letf (((symbol-function 'url-retrieve)
                          (lambda (url callback &optional _cbargs _silent)
                            ;; Simulate a successful HTTP response in a temp buffer.
                            (let ((buf (generate-new-buffer " *test-url-retrieve*")))
                              (with-current-buffer buf
                                (insert "HTTP/1.1 200 OK\nContent-Type: text/html\n\n"
                                        "<html><body><p>Search result</p></body></html>"))
                              (with-current-buffer buf
                                (funcall callback nil))
                              buf))))
                 (ekg-agent--web-browse (lambda (r) (setq result r))
                                        "https://example.com/test"))
               (should (stringp result))
               (should (string-match-p "Search result" result))
               (should (string-match-p "Content from https://example.com/test" result))))

(ekg-deftest ekg-agent-test-web-browse-http-error ()
             "HTTP errors from url-retrieve are reported."
             (let (result)
               (cl-letf (((symbol-function 'url-retrieve)
                          (lambda (_url callback &optional _cbargs _silent)
                            (let ((buf (generate-new-buffer " *test-url-error*")))
                              (with-current-buffer buf
                                (funcall callback (list :error '(error http 404))))
                              buf))))
                 (ekg-agent--web-browse (lambda (r) (setq result r))
                                        "https://example.com/missing"))
               (should (string-match-p "Error fetching URL" result))))

(ekg-deftest ekg-agent-test-web-search-constructs-url ()
             "Web search passes the correctly constructed search URL to web-browse."
             (let (captured-url result)
               (cl-letf (((symbol-function 'ekg-agent--web-browse)
                          (lambda (callback url)
                            (setq captured-url url)
                            (funcall callback (format "Content from %s:\n\nresults" url)))))
                 (ekg-agent--web-search (lambda (r) (setq result r)) "emacs lisp"))
               (should (string-match-p "duckduckgo" captured-url))
               (should (string-match-p "emacs" captured-url))
               (should (string-match-p "lisp" captured-url))
               (should (stringp result))))

(ekg-deftest ekg-agent-test-web-search-hexifies-query ()
             "Web search properly hex-encodes special characters in the query."
             (let (captured-url)
               (cl-letf (((symbol-function 'ekg-agent--web-browse)
                          (lambda (callback url)
                            (setq captured-url url)
                            (funcall callback "ok"))))
                 (ekg-agent--web-search #'ignore "hello world & more"))
               ;; Space should be %20, & should be %26
               (should (string-match-p "%20" captured-url))
               (should (string-match-p "%26" captured-url))
               (should-not (string-match-p " " (replace-regexp-in-string
                                                "https?://[^?]*\\?" ""
                                                captured-url)))))

(ekg-deftest ekg-agent-test-web-search-custom-prefix ()
             "Web search respects a custom `eww-search-prefix'."
             (let ((eww-search-prefix "https://google.com/search?q=")
                   captured-url)
               (cl-letf (((symbol-function 'ekg-agent--web-browse)
                          (lambda (callback url)
                            (setq captured-url url)
                            (funcall callback "ok"))))
                 (ekg-agent--web-search #'ignore "test"))
               (should (string-match-p "^https://google.com/search\\?q=" captured-url))))

;; Emacs help / Info tools

(ekg-deftest ekg-agent-test-emacs-help-symbol-cl-defstruct ()
             "The Emacs help symbol tool returns useful help for `cl-defstruct'."
             (let ((result (ekg-agent--emacs-help-symbol "cl-defstruct")))
               (should (stringp result))
               (should (string-match-p "cl-defstruct" result))
               (should (string-match-p "Define a struct type" result))
               (should (string-match-p ":include" result))))

(ekg-deftest ekg-agent-test-emacs-help-symbol-bad-kind ()
             "The Emacs help symbol tool reports bad help kinds as text."
             (let ((result (ekg-agent--emacs-help-symbol "cl-defstruct" "bogus")))
               (should (string-match-p "Error:" result))
               (should (string-match-p "Unknown help kind" result))))

(ekg-deftest ekg-agent-test-emacs-help-search-finds-defstruct ()
             "The Emacs help search tool can discover `cl-defstruct'."
             (let ((result (ekg-agent--emacs-help-search "cl-defstruct" "no" 5)))
               (should (stringp result))
               (should (string-match-p "cl-defstruct" result))))

(ekg-deftest ekg-agent-test-emacs-info-node-structures ()
             "The Emacs Info node tool can read the CL Structures node."
             (let ((result (ekg-agent--emacs-info-node "(cl)Structures")))
               (should (stringp result))
               (should (string-match-p "Manual: cl" result))
               (should (string-match-p "Node: Structures" result))
               (should (string-match-p ":include" result))))

(ekg-deftest ekg-agent-test-emacs-info-search-finds-include ()
             "The Emacs Info search tool can find struct include information."
             (let ((result (ekg-agent--emacs-info-search ":include" "cl" 3)))
               (should (stringp result))
               (should (string-match-p "Manual: cl" result))
               (should (string-match-p "Node: Structures" result))
               (should (string-match-p ":include" result))))

(ekg-deftest ekg-agent-test-emacs-reference-tools-in-base-tools ()
             "Emacs help and Info tools are part of the base agent tool set."
             (let ((names (mapcar #'llm-tool-name ekg-agent-base-tools)))
               (dolist (name '("emacs_help_symbol"
                               "emacs_help_search"
                               "emacs_info_node"
                               "emacs_info_search"))
                 (should (member name names)))))

(provide 'ekg-agent-test)
;;; ekg-agent-test.el ends here
