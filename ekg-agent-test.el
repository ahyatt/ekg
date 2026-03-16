;;; ekg-agent-test.el --- Tests for ekg-agent  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Andrew Hyatt <ahyatt@gmail.com>

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

;; Indentation adjustment

(ert-deftest ekg-agent-test-adjust-indentation-strip-extra ()
  "Extra leading whitespace in the replacement is stripped."
  (should (equal "    (defun foo ()\n      (bar))"
                 (ekg-agent--adjust-indentation
                  "        (defun foo ()\n          (bar))" 4))))

(ert-deftest ekg-agent-test-adjust-indentation-no-change ()
  "Replacement already at the correct indentation is unchanged."
  (should (equal "    (defun foo ()\n      (bar))"
                 (ekg-agent--adjust-indentation
                  "    (defun foo ()\n      (bar))" 4))))

(ert-deftest ekg-agent-test-adjust-indentation-add-whitespace ()
  "Whitespace is added when the replacement is under-indented."
  (should (equal "    (defun foo ()\n      (bar))"
                 (ekg-agent--adjust-indentation
                  "  (defun foo ()\n    (bar))" 4))))

(ert-deftest ekg-agent-test-adjust-indentation-empty-lines ()
  "Empty lines in the replacement are preserved as-is."
  (should (equal "    line1\n\n    line3"
                 (ekg-agent--adjust-indentation
                  "        line1\n\n        line3" 4))))

(ert-deftest ekg-agent-test-adjust-indentation-zero-target ()
  "Adjustment to column zero strips all common indentation."
  (should (equal "(foo)\n  (bar)"
                 (ekg-agent--adjust-indentation
                  "    (foo)\n      (bar)" 0))))

(ert-deftest ekg-agent-test-adjust-indentation-single-line ()
  "A single line replacement is adjusted correctly."
  (should (equal "  hello"
                 (ekg-agent--adjust-indentation "      hello" 2))))

;; Error-as-text macro

(ert-deftest ekg-agent-test-error-as-text-on-error ()
  "Errors are caught and returned as a descriptive string."
  (should (equal "Error: something broke"
                 (ekg-agent--with-error-as-text
                   (error "something broke")))))

(ert-deftest ekg-agent-test-error-as-text-on-success ()
  "Successful results pass through unchanged."
  (should (equal 42
                 (ekg-agent--with-error-as-text
                   (+ 40 2)))))

;; Line ID generation

(ert-deftest ekg-agent-test-line-ids-unique ()
  "Line IDs for different lines of the same file are unique."
  (let ((ids (mapcar (lambda (n) (ekg-agent--line-id "/tmp/test.txt" n))
                     (number-sequence 1 100))))
    (should (= (length ids)
               (length (delete-dups (copy-sequence ids)))))))

(ert-deftest ekg-agent-test-line-ids-three-chars ()
  "Every line ID is exactly 3 characters."
  (let ((ids (mapcar (lambda (n) (ekg-agent--line-id "/tmp/test.txt" n))
                     (number-sequence 1 50))))
    (should (cl-every (lambda (id) (= 3 (length id))) ids))))

(ert-deftest ekg-agent-test-line-ids-deterministic ()
  "The same path and line number always produce the same ID."
  (should (equal (ekg-agent--line-id "/tmp/test.txt" 42)
                 (ekg-agent--line-id "/tmp/test.txt" 42))))

;; File read/edit round-trip

(ert-deftest ekg-agent-test-read-file-with-line-ids ()
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

(ert-deftest ekg-agent-test-read-file-from-buffer ()
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

(ert-deftest ekg-agent-test-edit-file-round-trip ()
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

(ert-deftest ekg-agent-test-edit-file-adjusts-indentation ()
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

;; Agent integration tests
;;
;; These simulate the agent loop by mocking llm-chat-async to make
;; predetermined tool calls, then verify that tools execute correctly
;; and the agent loop terminates as expected.

(require 'ekg-test-utils)
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
               ((symbol-function 'llm-chat-async)
                mock-fn)
               ((symbol-function 'ekg-agent--ensure-log-window)
                #'ignore))
       ,@body)
     ;; Clean up log buffer
     (when-let ((buf (get-buffer (format ekg-agent-log-buffer-name-format
                                         "test-agent"))))
       (kill-buffer buf))))

(ert-deftest ekg-agent-test-agent-reads-and-edits-file ()
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

(ekg-deftest ekg-agent-test-agent-creates-note ()
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

(ert-deftest ekg-agent-test-agent-run-command ()
  "The run_command tool executes a shell command and returns output."
  (let (result)
    (ekg-agent--run-command (lambda (r) (setq result r)) "echo hello-world")
    (while (not result) (accept-process-output nil 0.1))
    (should (string-match-p "Exit code: 0" result))
    (should (string-match-p "hello-world" result))))

(ert-deftest ekg-agent-test-agent-run-command-failure ()
  "The run_command tool reports non-zero exit codes."
  (let (result)
    (ekg-agent--run-command (lambda (r) (setq result r)) "exit 42")
    (while (not result) (accept-process-output nil 0.1))
    (should (string-match-p "Exit code: 42" result))))

;; Buffer line ID generation

(ert-deftest ekg-agent-test-buffer-line-ids-unique ()
  "Buffer line IDs for different lines of the same buffer are unique."
  (let ((ids (mapcar (lambda (n) (ekg-agent--buffer-line-id "*test-buf*" n))
                     (number-sequence 1 100))))
    (should (= (length ids)
               (length (delete-dups (copy-sequence ids)))))))

(ert-deftest ekg-agent-test-buffer-line-ids-three-chars ()
  "Every buffer line ID is exactly 3 characters."
  (let ((ids (mapcar (lambda (n) (ekg-agent--buffer-line-id "*test-buf*" n))
                     (number-sequence 1 50))))
    (should (cl-every (lambda (id) (= 3 (length id))) ids))))

(ert-deftest ekg-agent-test-buffer-line-ids-deterministic ()
  "The same buffer name and line number always produce the same ID."
  (should (equal (ekg-agent--buffer-line-id "*test*" 42)
                 (ekg-agent--buffer-line-id "*test*" 42))))

(ert-deftest ekg-agent-test-buffer-line-ids-differ-from-file ()
  "Buffer line IDs differ from file line IDs for the same line number."
  (should-not (equal (ekg-agent--buffer-line-id "/tmp/test.txt" 1)
                     (ekg-agent--line-id "/tmp/test.txt" 1))))

;; List buffers

(ert-deftest ekg-agent-test-list-buffers ()
  "Listing buffers returns visible buffers with metadata."
  (let ((buf (get-buffer-create "*ekg-agent-test-list*")))
    (unwind-protect
        (let ((result (ekg-agent--list-buffers)))
          (should (stringp result))
          (should (string-match-p "\\*ekg-agent-test-list\\*" result)))
      (kill-buffer buf))))

(ert-deftest ekg-agent-test-list-buffers-with-filter ()
  "Listing buffers with a regex filter returns only matching buffers."
  (let ((buf1 (get-buffer-create "*ekg-agent-test-alpha*"))
        (buf2 (get-buffer-create "*ekg-agent-test-beta*")))
    (unwind-protect
        (let ((result (ekg-agent--list-buffers "alpha")))
          (should (string-match-p "alpha" result))
          (should-not (string-match-p "beta" result)))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest ekg-agent-test-list-buffers-excludes-internal ()
  "Internal buffers (names starting with space) are excluded."
  (let ((buf (get-buffer-create " *ekg-agent-internal*")))
    (unwind-protect
        (let ((result (ekg-agent--list-buffers "ekg-agent-internal")))
          (should (equal "No buffers found." result)))
      (kill-buffer buf))))

;; Read buffer

(ert-deftest ekg-agent-test-read-buffer-with-line-ids ()
  "Reading a buffer returns content prefixed with 3-char identifiers and positions."
  (let ((buf (get-buffer-create "*ekg-agent-test-read*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (erase-buffer)
            (insert "alpha\nbeta\ngamma\n"))
          (let ((result (ekg-agent--read-buffer "*ekg-agent-test-read*")))
            (should (stringp result))
            (should (string-match-p "^begin_pos:" result))
            (should (string-match-p "end_pos:" result))
            (dolist (line (cdr (split-string result "\n")))
              (when (not (string-empty-p line))
                (should (string-match "\\`...: " line))))))
      (kill-buffer buf))))

(ert-deftest ekg-agent-test-read-buffer-range-by-line-number ()
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

(ert-deftest ekg-agent-test-read-buffer-range-by-identifier ()
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

(ert-deftest ekg-agent-test-read-buffer-returns-positions ()
  "The begin_pos and end_pos in read_buffer output match actual buffer positions."
  (let ((buf (get-buffer-create "*ekg-agent-test-pos*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (erase-buffer)
            (insert "hello\nworld\n"))
          (let ((result (ekg-agent--read-buffer "*ekg-agent-test-pos*"
                                                "2" "2" "line_number")))
            (should (string-match "begin_pos: \\([0-9]+\\)" result))
            (let ((begin-pos (string-to-number (match-string 1 result))))
              ;; "hello\n" is 6 chars, so line 2 starts at position 7.
              (should (= 7 begin-pos)))))
      (kill-buffer buf))))

(ert-deftest ekg-agent-test-read-buffer-nonexistent ()
  "Reading a nonexistent buffer returns an error string."
  (let ((result (ekg-agent--read-buffer "*no-such-buffer-exists*")))
    (should (string-match-p "Error:" result))))

;; Edit buffer

(ert-deftest ekg-agent-test-edit-buffer-round-trip ()
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

(ert-deftest ekg-agent-test-edit-buffer-adjusts-indentation ()
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

(ert-deftest ekg-agent-test-edit-buffer-nonexistent ()
  "Editing a nonexistent buffer returns an error string."
  (let ((result (ekg-agent--edit-buffer "*no-such-buffer*"
                                       "abc" "text" "abc" "text" "new")))
    (should (string-match-p "Error:" result))))

;; Run interactive command

(ert-deftest ekg-agent-test-run-interactive-command-with-point ()
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

(ert-deftest ekg-agent-test-run-interactive-command-with-line-id ()
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

(ert-deftest ekg-agent-test-run-interactive-command-returns-context ()
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
            (should (string-match-p "begin_pos:" result))
            (should (string-match-p "end_pos:" result))
            (should (string-match-p "line1" result))))
      (kill-buffer buf))))

(ert-deftest ekg-agent-test-run-interactive-command-post-move-context ()
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

(ert-deftest ekg-agent-test-run-interactive-command-nonexistent-buffer ()
  "Running a command in a nonexistent buffer returns an error string."
  (let ((result (ekg-agent--run-interactive-command
                 "*no-such-buffer*" "forward-char" "1" nil nil)))
    (should (string-match-p "Error:" result))))

(ert-deftest ekg-agent-test-run-interactive-command-bad-command ()
  "Running a non-interactive function returns an error string."
  (let ((buf (get-buffer-create "*ekg-agent-test-bad-cmd*")))
    (unwind-protect
        (let ((result (ekg-agent--run-interactive-command
                       "*ekg-agent-test-bad-cmd*" "not-a-real-command"
                       "1" nil nil)))
          (should (string-match-p "Error:" result)))
      (kill-buffer buf))))

(ert-deftest ekg-agent-test-resolve-buffer-point-precedence ()
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

(ert-deftest ekg-agent-test-agent-reads-and-edits-buffer ()
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

(provide 'ekg-agent-test)
;;; ekg-agent-test.el ends here
