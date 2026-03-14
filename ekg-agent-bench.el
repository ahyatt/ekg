;;; ekg-agent-bench.el --- Benchmark suite for ekg-agent effectiveness -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Package-Requires: ((emacs "28.1") (llm-test "0.1.0") (yaml "0.5.0"))
;; Keywords: testing, tools
;; Version: 0.1.0
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
;; Metric-driven benchmark suite for the ekg-agent.  Runs predefined
;; tasks in a fresh Emacs subprocess, measures success/failure,
;; iteration count, tool usage, and wall-clock time.
;;
;; Each task tests the agent along three axes:
;;   - Task completion: did the agent accomplish the goal?
;;   - Skill adherence: did the agent follow instructions stored in ekg?
;;   - Memory discipline: did the agent store/retrieve knowledge in ekg?
;;
;; Usage:
;;   (require 'ekg-agent-bench)
;;   (setq ekg-agent-bench-provider-form
;;         '(make-llm-claude :key "sk-..." :chat-model "..."))
;;   (ekg-agent-bench-run)

;;; Code:

(require 'cl-lib)
(require 'llm-test)
(require 'yaml)
(require 'ert)

(defgroup ekg-agent-bench nil
  "Benchmark suite for ekg-agent."
  :group 'ekg)

(defcustom ekg-agent-bench-provider-form nil
  "A form that constructs the LLM provider in the test subprocess.
This is evaluated inside the fresh Emacs to create `ekg-llm-provider'.

Example:
  (make-llm-claude :key \"sk-...\" :chat-model \"claude-sonnet-4-20250514\")"
  :type 'sexp
  :group 'ekg-agent-bench)

(defcustom ekg-agent-bench-poll-interval 2
  "Seconds between polls when waiting for agent completion."
  :type 'number
  :group 'ekg-agent-bench)

(defcustom ekg-agent-bench-default-timeout 180
  "Default timeout in seconds for a single benchmark task."
  :type 'integer
  :group 'ekg-agent-bench)

(defcustom ekg-agent-bench-extra-load-paths nil
  "Additional directories to add to the test subprocess load-path.
Use this for provider-specific dependencies not auto-detected."
  :type '(repeat directory)
  :group 'ekg-agent-bench)

;;; Data Structures

(cl-defstruct ekg-agent-bench-task
  "A single benchmark task."
  name description setup trigger
  verify-task verify-skill verify-memory
  max-iterations timeout)

(cl-defstruct ekg-agent-bench-group
  "A group of benchmark tasks with shared setup."
  name setup tasks)

(cl-defstruct ekg-agent-bench-result
  "Result of running a single benchmark task."
  name
  task-passed skill-passed memory-passed
  iterations tools-used wall-time status error-message)

;;; YAML Parsing

(defun ekg-agent-bench--parse-yaml-file (file)
  "Parse a benchmark YAML FILE into an `ekg-agent-bench-group'."
  (let* ((text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (parsed (yaml-parse-string text))
         (group-name (gethash 'group parsed))
         (setup (or (gethash 'setup parsed) ""))
         (tasks-array (gethash 'tasks parsed)))
    (unless group-name
      (error "Benchmark YAML missing required 'group' key in %s" file))
    (unless tasks-array
      (error "Benchmark YAML missing required 'tasks' key in %s" file))
    (make-ekg-agent-bench-group
     :name group-name
     :setup setup
     :tasks (mapcar
             (lambda (task-hash)
               (make-ekg-agent-bench-task
                :name (gethash 'name task-hash)
                :description (or (gethash 'description task-hash) "")
                :setup (gethash 'setup task-hash)
                :trigger (gethash 'trigger task-hash)
                :verify-task (gethash 'verify-task task-hash)
                :verify-skill (gethash 'verify-skill task-hash)
                :verify-memory (gethash 'verify-memory task-hash)
                :max-iterations (gethash 'max-iterations task-hash)
                :timeout (gethash 'timeout task-hash)))
             (append tasks-array nil)))))

(defun ekg-agent-bench--load-directory (directory)
  "Load all benchmark YAML files from DIRECTORY.
Returns a list of `ekg-agent-bench-group' structs."
  (let ((files (append (directory-files directory t "\\.yaml\\'")
                       (directory-files directory t "\\.yml\\'"))))
    (mapcar #'ekg-agent-bench--parse-yaml-file files)))

;;; Subprocess Management

(defconst ekg-agent-bench--required-libraries
  '("ekg" "ekg-agent" "ekg-llm" "ekg-embedding" "ekg-org"
    "llm" "llm-openai" "llm-claude" "llm-vertex" "llm-ollama"
    "llm-prompt"
    "plz" "plz-media-type" "plz-event-source"
    "triples" "triples-backups"
    "websocket" "async" "compat" "futur" "vui")
  "Libraries whose directories are added to the subprocess load-path.
Libraries not found are silently skipped.")

(defun ekg-agent-bench--compute-load-paths ()
  "Compute load-path entries needed by the test subprocess."
  (let ((paths (copy-sequence ekg-agent-bench-extra-load-paths)))
    (dolist (lib ekg-agent-bench--required-libraries)
      (let ((file (locate-library lib)))
        (when file
          (push (file-name-directory file) paths))))
    (delete-dups paths)))

(defun ekg-agent-bench--init-forms (error-file)
  "Return init forms for the benchmark subprocess.
Sets up a temp database and configures the LLM provider.
ERROR-FILE is a path where init errors will be written for diagnosis."
  `(;; Wrap all init in condition-case so we get error diagnostics.
    (condition-case err
        (progn
          ;; Prefer .el source over stale .elc.
          (setq load-prefer-newer t)
          (setq ekg-db-file (make-temp-file "ekg-bench-db"))
          (require 'ekg)
          (require 'ekg-agent)
          (require 'ekg-llm)
          (require 'ekg-org)
          ,@(when ekg-agent-bench-provider-form
              `((setq ekg-llm-provider ,ekg-agent-bench-provider-form)))
          ;; Connect to the temp database.
          (ekg-connect)
          ;; Suppress log window display in the daemon.
          (defun ekg-agent--ensure-log-window ()
            (let ((buf (current-buffer)))
              (unless (get-buffer-window buf t)
                (set-window-buffer (selected-window) buf)))))
      (error
       (with-temp-file ,error-file
         (insert (format "Init error: %S\n" err))
         (insert (format "Load path:\n%s\n"
                         (mapconcat #'identity load-path "\n"))))))))

(defvar ekg-agent-bench--server-counter 0
  "Counter for generating unique server names.")

(defun ekg-agent-bench--start-emacs ()
  "Start a fresh Emacs subprocess for benchmarking.
Like `llm-test--start-emacs' but with better error reporting when
the daemon fails to start."
  (let* ((load-paths (ekg-agent-bench--compute-load-paths))
         (error-file (make-temp-file "ekg-bench-init-error-"))
         (init-forms (ekg-agent-bench--init-forms error-file))
         (server-name (format "ekg-bench-%d-%d"
                              (emacs-pid)
                              (cl-incf ekg-agent-bench--server-counter)))
         (socket-dir (make-temp-file "ekg-bench-socket-" t))
         (init-file (make-temp-file "ekg-bench-init-" nil ".el"))
         (buf-name (format " *ekg-bench-emacs-%s*" server-name)))
    (with-temp-file init-file
      (insert (format "(setq server-socket-dir %S server-name %S)\n"
                      socket-dir server-name))
      (dolist (dir load-paths)
        (insert (format "(add-to-list 'load-path %S)\n" dir)))
      (dolist (form init-forms)
        (insert (format "%S\n" form))))
    (let ((process (start-process
                    (format "ekg-bench-emacs-%s" server-name)
                    buf-name
                    llm-test-emacs-executable
                    "-Q"
                    "-l" init-file
                    (format "--daemon=%s" server-name)))
          (socket-file (expand-file-name server-name socket-dir))
          (deadline (+ (float-time) llm-test-timeout)))
      ;; Wait for the daemon to be ready.
      (while (and (< (float-time) deadline)
                  (process-live-p process)
                  (not (file-exists-p socket-file)))
        (sit-for 0.1))
      (unless (file-exists-p socket-file)
        ;; Collect diagnostic info before signaling.
        (let ((proc-output (when (get-buffer buf-name)
                             (with-current-buffer buf-name
                               (buffer-string))))
              (init-error (when (and (file-exists-p error-file)
                                     (> (file-attribute-size
                                         (file-attributes error-file))
                                        0))
                            (with-temp-buffer
                              (insert-file-contents error-file)
                              (buffer-string))))
              (alive (process-live-p process)))
          (when alive (kill-process process))
          (ignore-errors (delete-directory socket-dir t))
          (ignore-errors (delete-file init-file))
          (ignore-errors (delete-file error-file))
          (error "Daemon failed to start.%s\nProcess %s, output:\n%s"
                 (if init-error
                     (format "\n%s" init-error)
                   "\nNo error file written (init may have hung).")
                 (if alive "still running (timeout)" "exited early")
                 (or proc-output "(no output)"))))
      ;; Daemon started — check if init actually succeeded by reading
      ;; the error file.  The condition-case in init-forms writes here
      ;; on failure, but the daemon still starts because server vars
      ;; are set before the condition-case.
      (when (and (file-exists-p error-file)
                 (> (file-attribute-size (file-attributes error-file)) 0))
        (let ((init-error (with-temp-buffer
                            (insert-file-contents error-file)
                            (buffer-string))))
          (ignore-errors (kill-process process))
          (ignore-errors (delete-directory socket-dir t))
          (ignore-errors (delete-file init-file))
          (ignore-errors (delete-file error-file))
          (error "Daemon started but init failed:\n%s" init-error)))
      (ignore-errors (delete-file error-file))
      (let ((info (list :process process
                        :server-name server-name
                        :socket-dir socket-dir
                        :init-file init-file)))
        (llm-test--eval-in-emacs
         info
         (format "(set-frame-size (selected-frame) %d %d)"
                 llm-test-frame-width llm-test-frame-height))
        info))))

;;; Agent Polling and Metric Extraction

(defun ekg-agent-bench--find-log-buffer (emacs-info)
  "Find the ekg agent log buffer name in the subprocess.
Returns the buffer name string, or nil if not found."
  (let ((result (llm-test--eval-in-emacs
                 emacs-info
                 "(car (seq-filter
                        (lambda (b) (string-match-p \"\\\\*ekg agent log:\" (buffer-name b)))
                        (buffer-list)))")))
    (unless (or (equal result "nil") (string-empty-p result))
      ;; Result is printed as #<buffer NAME>, extract the name.
      (when (string-match "\\*ekg agent log:[^*]+\\*" result)
        (match-string 0 result)))))

(defun ekg-agent-bench--agent-running-p (emacs-info log-buffer-name)
  "Check if the agent is still running in LOG-BUFFER-NAME."
  (let ((result (llm-test--eval-in-emacs
                 emacs-info
                 (format "(if (and (get-buffer %S)
                                   (buffer-local-value 'ekg-agent--running-p
                                                       (get-buffer %S)))
                              \"running\" \"stopped\")"
                         log-buffer-name log-buffer-name))))
    (equal result "\"running\"")))

(defun ekg-agent-bench--poll-until-done (emacs-info timeout)
  "Poll the subprocess until the agent finishes or TIMEOUT seconds elapse.
Returns `done' if the agent finished, `timeout' if it timed out."
  (let ((deadline (+ (float-time) timeout))
        (log-buf nil))
    ;; First, wait for the log buffer to appear.
    (while (and (< (float-time) deadline)
                (not log-buf))
      (sit-for ekg-agent-bench-poll-interval)
      (setq log-buf (ekg-agent-bench--find-log-buffer emacs-info)))
    (if (not log-buf)
        'timeout
      ;; Now poll until agent stops.
      (while (and (< (float-time) deadline)
                  (ekg-agent-bench--agent-running-p emacs-info log-buf))
        (sit-for ekg-agent-bench-poll-interval))
      (if (< (float-time) deadline) 'done 'timeout))))

(defun ekg-agent-bench--extract-metrics (emacs-info)
  "Extract iteration count and tools used from the agent log buffer.
Returns a plist (:iterations N :tools-used (TOOL ...))."
  (let* ((log-content
          (llm-test--eval-in-emacs
           emacs-info
           "(let ((buf (car (seq-filter
                             (lambda (b) (string-match-p \"\\\\*ekg agent log:\" (buffer-name b)))
                             (buffer-list)))))
              (if buf
                  (with-current-buffer buf
                    (buffer-substring-no-properties (point-min) (point-max)))
                \"\"))"))
         ;; Strip outer quotes from emacsclient result.
         (content (if (and (> (length log-content) 1)
                           (eq (aref log-content 0) ?\"))
                      (read log-content)
                    log-content))
         (lines (split-string content "\n"))
         (iterations 0)
         (tools nil))
    (dolist (line lines)
      (when (string-match "Tools: \\(.*\\)" line)
        (cl-incf iterations)
        (let ((tools-str (match-string 1 line)))
          (dolist (segment (split-string tools-str ", "))
            (when (string-match "Tool: \\([^ ]+\\)" segment)
              (push (match-string 1 segment) tools))))))
    (list :iterations iterations
          :tools-used (delete-dups (nreverse tools)))))

(defun ekg-agent-bench--eval-verify (emacs-info verify-expr)
  "Evaluate VERIFY-EXPR in the subprocess and return t if it's truthy.
Returns nil if VERIFY-EXPR is nil (meaning not applicable)."
  (when verify-expr
    (let ((result (llm-test--eval-in-emacs
                   emacs-info
                   (format "(if (progn %s) \"pass\" \"fail\")" verify-expr))))
      (equal result "\"pass\""))))

;;; Task Runner

(defun ekg-agent-bench--run-task (emacs-info group-setup task)
  "Run a single benchmark TASK in EMACS-INFO with GROUP-SETUP.
Returns an `ekg-agent-bench-result'."
  (let ((start-time (float-time))
        (timeout (or (ekg-agent-bench-task-timeout task)
                     ekg-agent-bench-default-timeout))
        (task-name (or (ekg-agent-bench-task-name task) "unnamed"))
        status error-msg)
    (message "bench: running %s..." task-name)
    ;; Run group setup.
    (when (and group-setup (not (string-empty-p group-setup)))
      (condition-case err
          (llm-test--eval-in-emacs
           emacs-info
           (format "(progn %s nil)" group-setup))
        (error (setq error-msg (format "Group setup failed: %s"
                                       (error-message-string err))))))
    ;; Run task-specific setup.
    (when (and (not error-msg)
               (ekg-agent-bench-task-setup task)
               (not (string-empty-p (ekg-agent-bench-task-setup task))))
      (condition-case err
          (llm-test--eval-in-emacs
           emacs-info
           (format "(progn %s nil)" (ekg-agent-bench-task-setup task)))
        (error (setq error-msg (format "Task setup failed: %s"
                                       (error-message-string err))))))
    ;; Trigger the agent.  We schedule it via run-at-time so that
    ;; emacsclient returns immediately — the agent's iteration-0
    ;; does a synchronous LLM call (prompt-id) that would otherwise
    ;; block the emacsclient eval.
    (when (not error-msg)
      (condition-case err
          (llm-test--eval-in-emacs
           emacs-info
           (format "(progn (run-at-time 0 nil (lambda () %s)) nil)"
                   (ekg-agent-bench-task-trigger task)))
        (error (setq error-msg (format "Trigger failed: %s"
                                       (error-message-string err))))))
    ;; Poll for completion.
    (if error-msg
        (setq status 'error)
      (setq status (ekg-agent-bench--poll-until-done emacs-info timeout)))
    ;; Collect metrics.
    (let* ((wall-time (- (float-time) start-time))
           (metrics (unless (eq status 'error)
                      (ekg-agent-bench--extract-metrics emacs-info)))
           (task-passed (unless (eq status 'error)
                          (condition-case err
                              (ekg-agent-bench--eval-verify
                               emacs-info
                               (ekg-agent-bench-task-verify-task task))
                            (error
                             (setq error-msg
                                   (format "verify-task error: %s"
                                           (error-message-string err)))
                             nil))))
           (skill-passed (unless (eq status 'error)
                           (condition-case err
                               (ekg-agent-bench--eval-verify
                                emacs-info
                                (ekg-agent-bench-task-verify-skill task))
                             (error
                              (message "bench: verify-skill error for %s: %s"
                                       task-name (error-message-string err))
                              nil))))
           (memory-passed (unless (eq status 'error)
                            (condition-case err
                                (ekg-agent-bench--eval-verify
                                 emacs-info
                                 (ekg-agent-bench-task-verify-memory task))
                              (error
                               (message "bench: verify-memory error for %s: %s"
                                        task-name (error-message-string err))
                               nil)))))
      (make-ekg-agent-bench-result
       :name task-name
       :task-passed task-passed
       :skill-passed skill-passed
       :memory-passed memory-passed
       :iterations (or (plist-get metrics :iterations) 0)
       :tools-used (or (plist-get metrics :tools-used) nil)
       :wall-time wall-time
       :status status
       :error-message error-msg))))

;;; Results Display

(defun ekg-agent-bench--format-pass (val)
  "Format a pass/fail/nil VAL as a short display string."
  (cond
   ((null val) "n/a")
   (val "PASS")
   (t "FAIL")))

(defun ekg-agent-bench--display-results (results provider-desc)
  "Display benchmark RESULTS in a buffer with PROVIDER-DESC."
  (let ((buf (get-buffer-create "*ekg-agent-bench*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "EKG Agent Benchmark — %s  Provider: %s\n"
                        (format-time-string "%F %T")
                        (or provider-desc "unknown")))
        (insert (make-string 72 ?═) "\n")
        (insert (format "%-30s %5s %5s %6s %5s %6s  %s\n"
                        "Task" "Task" "Skill" "Memory" "Iters" "Time" "Status"))
        (insert (make-string 72 ?─) "\n")
        (let ((task-total 0) (task-pass 0)
              (skill-total 0) (skill-pass 0)
              (memory-total 0) (memory-pass 0)
              (total-iters 0) (total-time 0.0))
          (dolist (r results)
            (let ((tp (ekg-agent-bench-result-task-passed r))
                  (sp (ekg-agent-bench-result-skill-passed r))
                  (mp (ekg-agent-bench-result-memory-passed r)))
              (unless (null tp) (cl-incf task-total) (when tp (cl-incf task-pass)))
              (unless (null sp) (cl-incf skill-total) (when sp (cl-incf skill-pass)))
              (unless (null mp) (cl-incf memory-total) (when mp (cl-incf memory-pass))))
            (cl-incf total-iters (ekg-agent-bench-result-iterations r))
            (setq total-time (+ total-time (ekg-agent-bench-result-wall-time r)))
            (insert (format "%-30s %5s %5s %6s %5d %5.0fs  %s\n"
                            (ekg-agent-bench-result-name r)
                            (ekg-agent-bench--format-pass
                             (ekg-agent-bench-result-task-passed r))
                            (ekg-agent-bench--format-pass
                             (ekg-agent-bench-result-skill-passed r))
                            (ekg-agent-bench--format-pass
                             (ekg-agent-bench-result-memory-passed r))
                            (ekg-agent-bench-result-iterations r)
                            (ekg-agent-bench-result-wall-time r)
                            (ekg-agent-bench-result-status r)))
            (when (ekg-agent-bench-result-error-message r)
              (insert (format "  ERROR: %s\n"
                              (ekg-agent-bench-result-error-message r)))))
          (insert (make-string 72 ?─) "\n")
          (insert (format "Task: %d/%d  Skill: %d/%d  Memory: %d/%d  Avg iters: %.1f  Avg time: %.1fs\n"
                          task-pass task-total
                          skill-pass skill-total
                          memory-pass memory-total
                          (if (> (length results) 0)
                              (/ (float total-iters) (length results))
                            0.0)
                          (if (> (length results) 0)
                              (/ total-time (length results))
                            0.0)))
          ;; Tools summary.
          (insert "\nTools used across all tasks:\n")
          (let ((all-tools nil))
            (dolist (r results)
              (dolist (tool (ekg-agent-bench-result-tools-used r))
                (unless (member tool all-tools)
                  (push tool all-tools))))
            (dolist (tool (sort all-tools #'string<))
              (insert (format "  %s\n" tool)))))))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (special-mode)))

;;; ERT Registration

;;; Diagnostics

;;;###autoload
(defun ekg-agent-bench-diagnose ()
  "Run a diagnostic check to verify the benchmark subprocess can start.
Reports which libraries load successfully and whether ekg connects."
  (interactive)
  (unless ekg-agent-bench-provider-form
    (user-error "Set `ekg-agent-bench-provider-form' first"))
  (message "ekg-agent-bench: computing load paths...")
  (let ((paths (ekg-agent-bench--compute-load-paths)))
    (message "  %d directories on load-path" (length paths))
    (dolist (p paths)
      (message "    %s" p)))
  (message "ekg-agent-bench: starting subprocess (timeout %ds)..."
           llm-test-timeout)
  (let ((info (ekg-agent-bench--start-emacs)))
    (unwind-protect
        (progn
          (message "  daemon started OK")
          (dolist (lib '("ekg" "ekg-agent" "ekg-llm" "ekg-org" "ekg-embedding"
                         "llm" "triples" "futur" "vui"))
            (let ((result (llm-test--eval-in-emacs
                           info
                           (format "(if (featurep '%s) \"loaded\" \"NOT loaded\")"
                                   lib))))
              (message "  %s: %s" lib result)))
          (message "  ekg-db: %s"
                   (llm-test--eval-in-emacs info "(if ekg-db \"connected\" \"nil\")"))
          (message "  provider: %s"
                   (llm-test--eval-in-emacs info "(type-of ekg-llm-provider)"))
          (message "  ekg-db-file: %s"
                   (llm-test--eval-in-emacs info "ekg-db-file"))
          (message "ekg-agent-bench: diagnosis PASSED"))
      (llm-test--stop-emacs info))))

;;; ERT Registration

(defun ekg-agent-bench-register-ert-tests (directory)
  "Register benchmark tasks from DIRECTORY as ERT tests."
  (let ((groups (ekg-agent-bench--load-directory directory)))
    (dolist (group groups)
      (let ((group-setup (ekg-agent-bench-group-setup group)))
        (dolist (task (ekg-agent-bench-group-tasks group))
          (let ((test-name (intern (format "ekg-agent-bench/%s"
                                           (ekg-agent-bench-task-name task))))
                (the-task task)
                (the-setup group-setup))
            (ert-set-test
             test-name
             (make-ert-test
              :name test-name
              :documentation (format "Agent bench: %s\n%s"
                                     (ekg-agent-bench-task-name task)
                                     (ekg-agent-bench-task-description task))
              :body (lambda ()
                      (unless ekg-agent-bench-provider-form
                        (ert-skip "ekg-agent-bench-provider-form not set"))
                      (let ((emacs-info (ekg-agent-bench--start-emacs)))
                        (unwind-protect
                            (let ((result (ekg-agent-bench--run-task
                                           emacs-info the-setup the-task)))
                              (unless (ekg-agent-bench-result-task-passed result)
                                (ert-fail
                                 (format "Task failed: %s (status: %s, iters: %d%s)"
                                         (ekg-agent-bench-result-name result)
                                         (ekg-agent-bench-result-status result)
                                         (ekg-agent-bench-result-iterations result)
                                         (if (ekg-agent-bench-result-error-message result)
                                             (format ", error: %s"
                                                     (ekg-agent-bench-result-error-message result))
                                           "")))))
                          (llm-test--stop-emacs emacs-info))))))))))))

;;; Entry Points

;;;###autoload
(defun ekg-agent-bench-run (&optional directory)
  "Run all benchmark tasks from DIRECTORY and display results.
DIRECTORY defaults to the benchmarks/ subdirectory next to this file."
  (interactive)
  (unless ekg-agent-bench-provider-form
    (user-error "Set `ekg-agent-bench-provider-form' before running benchmarks"))
  (let* ((dir (or directory
                   (expand-file-name "benchmarks"
                                     (file-name-directory
                                      (or load-file-name
                                          (locate-library "ekg-agent-bench"))))))
         (groups (ekg-agent-bench--load-directory dir))
         (all-results nil))
    (dolist (group groups)
      (let ((group-setup (ekg-agent-bench-group-setup group)))
        (dolist (task (ekg-agent-bench-group-tasks group))
          (let ((emacs-info (ekg-agent-bench--start-emacs)))
            (unwind-protect
                (push (ekg-agent-bench--run-task emacs-info group-setup task)
                      all-results)
              (llm-test--stop-emacs emacs-info))))))
    (setq all-results (nreverse all-results))
    (ekg-agent-bench--display-results
     all-results
     (format "%S" ekg-agent-bench-provider-form))
    all-results))

;;;###autoload
(defun ekg-agent-bench-run-one (task-name &optional directory)
  "Run a single benchmark task by TASK-NAME and display results.
DIRECTORY defaults to the benchmarks/ subdirectory."
  (interactive "sTask name: ")
  (unless ekg-agent-bench-provider-form
    (user-error "Set `ekg-agent-bench-provider-form' before running benchmarks"))
  (let* ((dir (or directory
                   (expand-file-name "benchmarks"
                                     (file-name-directory
                                      (or load-file-name
                                          (locate-library "ekg-agent-bench"))))))
         (groups (ekg-agent-bench--load-directory dir))
         (found nil))
    (catch 'found
      (dolist (group groups)
        (dolist (task (ekg-agent-bench-group-tasks group))
          (when (equal (ekg-agent-bench-task-name task) task-name)
            (setq found (cons group task))
            (throw 'found nil)))))
    (unless found
      (user-error "Task %s not found" task-name))
    (let* ((group (car found))
           (task (cdr found))
           (emacs-info (ekg-agent-bench--start-emacs)))
      (unwind-protect
          (let ((result (ekg-agent-bench--run-task
                         emacs-info
                         (ekg-agent-bench-group-setup group)
                         task)))
            (ekg-agent-bench--display-results
             (list result)
             (format "%S" ekg-agent-bench-provider-form))
            result)
        (llm-test--stop-emacs emacs-info)))))

(provide 'ekg-agent-bench)

;;; ekg-agent-bench.el ends here
