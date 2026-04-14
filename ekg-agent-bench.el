;;; ekg-agent-bench.el --- Benchmark suite for ekg-agent effectiveness -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Package-Requires: ((emacs "28.1"))
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
;;   (ekg-agent-bench-run)

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'subr-x)
;; Disable futur's background thread before loading: on macOS NS port,
;; `message' (or any redisplay) from a non-main thread causes an
;; NSException → GIL deadlock.  With this nil, futur dispatches all
;; callbacks via `run-with-timer' on the main thread instead.
(defvar futur-use-threads)
(setq futur-use-threads nil)
(require 'futur)

(defgroup ekg-agent-bench nil
  "Benchmark suite for ekg-agent."
  :group 'ekg)

(defcustom ekg-agent-bench-poll-interval 2
  "Seconds between polls when waiting for agent completion."
  :type 'number
  :group 'ekg-agent-bench)

(defcustom ekg-agent-bench-frame-width 80
  "Default frame width for the benchmark Emacs subprocess."
  :type 'integer
  :group 'ekg-agent-bench)

(defcustom ekg-agent-bench-frame-height 40
  "Default frame height for the benchmark Emacs subprocess."
  :type 'integer
  :group 'ekg-agent-bench)

(defcustom ekg-agent-bench-default-timeout 180
  "Default timeout in seconds for a single benchmark task."
  :type 'integer
  :group 'ekg-agent-bench)

(defcustom ekg-agent-bench-extra-load-paths nil
  "Additional directories to add to the test subprocess `load-path'.
Use this for provider-specific dependencies not auto-detected."
  :type '(repeat directory)
  :group 'ekg-agent-bench)

(defun ekg-agent-bench--emacs-executable ()
  "Return the path to the Emacs binary that should run the subprocess.
Uses the same binary as the current Emacs process to avoid version
mismatches between the host and the daemon."
  (expand-file-name invocation-name invocation-directory))

;;; Data Structures

(cl-defstruct ekg-agent-bench-task
  "A single benchmark task."
  name description setup trigger
  verify-task verify-skill verify-memory
  max-iterations timeout max-status-update-gap)

(cl-defstruct ekg-agent-bench-group
  "A group of benchmark tasks with shared setup."
  name setup tasks)

(cl-defstruct ekg-agent-bench-result
  "Result of running a single benchmark task."
  name
  task-passed skill-passed memory-passed
  iterations tools-used wall-time
  status-update-count max-status-update-gap status-update-gap-target
  status error-message
  agent-log)

;;; ELD Parsing

(defun ekg-agent-bench--parse-eld-file (file)
  "Parse a benchmark ELD FILE into an `ekg-agent-bench-group'."
  (let* ((parsed (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (read (current-buffer))))
         (group-name (plist-get parsed :group))
         (setup (plist-get parsed :setup))
         (tasks (plist-get parsed :tasks)))
    (unless group-name
      (error "Benchmark ELD missing required ':group' key in %s" file))
    (unless tasks
      (error "Benchmark ELD missing required ':tasks' key in %s" file))
    (make-ekg-agent-bench-group
     :name group-name
     :setup (if setup (prin1-to-string setup) "")
     :tasks (mapcar
             (lambda (task-plist)
               (make-ekg-agent-bench-task
                :name (plist-get task-plist :name)
                :description (or (plist-get task-plist :description) "")
                :setup (when-let ((val (plist-get task-plist :setup)))
                         (prin1-to-string val))
                :trigger (when-let ((val (plist-get task-plist :trigger)))
                           (prin1-to-string val))
                :verify-task (when-let ((val (plist-get task-plist :verify-task)))
                               (prin1-to-string val))
                :verify-skill (when-let ((val (plist-get task-plist :verify-skill)))
                                (prin1-to-string val))
                :verify-memory (when-let ((val (plist-get task-plist :verify-memory)))
                                 (prin1-to-string val))
                :max-iterations (plist-get task-plist :max-iterations)
                :timeout (plist-get task-plist :timeout)
                :max-status-update-gap
                (plist-get task-plist :max-status-update-gap)))
             tasks))))

(defun ekg-agent-bench--load-directory (directory)
  "Load all benchmark ELD files from DIRECTORY.
Returns a list of `ekg-agent-bench-group' structs."
  (let ((files (directory-files directory t "\\.eld\\'")))
    (mapcar #'ekg-agent-bench--parse-eld-file files)))

;;; Subprocess Management

(defconst ekg-agent-bench--required-libraries
  '("ekg" "ekg-agent" "ekg-llm" "ekg-embedding" "ekg-org"
    "llm" "llm-openai" "llm-claude" "llm-vertex" "llm-gemini" "llm-ollama"
    "llm-prompt"
    "plz" "plz-media-type" "plz-event-source"
    "triples" "triples-backups"
    "websocket" "async" "compat" "futur" "vui")
  "Libraries whose directories are added to the subprocess `load-path'.
Libraries not found are silently skipped.")

(defun ekg-agent-bench--emacs-builtin-p (dir)
  "Return non-nil if DIR is inside an Emacs installation Lisp tree.
The subprocess already has the same built-in Lisp dirs on its
default `load-path', so adding them explicitly is redundant."
  (let ((expanded (expand-file-name dir)))
    (or (string-match-p "/Emacs\\.app/.*/lisp" expanded)
        (string-match-p "/share/emacs/[0-9]" expanded))))

(defun ekg-agent-bench--compute-load-paths ()
  "Compute `load-path' entries needed by the test subprocess.
Includes directories for required libraries found via `locate-library',
but excludes Emacs built-in Lisp directories to avoid version mismatches
between the host Emacs and the subprocess daemon."
  (let ((paths (copy-sequence ekg-agent-bench-extra-load-paths)))
    (dolist (lib ekg-agent-bench--required-libraries)
      (let ((file (locate-library lib)))
        (when file
          (let ((dir (file-name-directory file)))
            (unless (ekg-agent-bench--emacs-builtin-p dir)
              (push dir paths))))))
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
          ;; Connect to the temp database.
          (ekg-connect)
          ;; Isolate the subprocess so it won't find any AGENTS.md
          ;; from the user's home or project directories.
          (setenv "HOME" (make-temp-file "ekg-bench-home-" t))
          ;; Reset default-directory so subprocesses (e.g., run_elisp)
          ;; don't inherit the host's working directory which may not
          ;; exist under the new HOME.  Must update all existing buffers
          ;; since new buffers inherit from the current buffer, not the
          ;; default value.
          (setq-default default-directory "/tmp/")
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (setq default-directory "/tmp/")))
          (defun ekg-agent--read-agents-md (_dir) nil)
          (defun ekg-agent--agents-md-context () nil)
          ;; Suppress log window display in the daemon.
          (defun ekg-agent--ensure-log-window ()
            (let ((buf (current-buffer)))
              (unless (get-buffer-window buf t)
                (set-window-buffer (selected-window) buf))))
          ;; Configure the LLM provider from the environment.
          (let ((provider-elisp (getenv "LLM_TEST_PROVIDER_ELISP")))
            (when (and provider-elisp (not (string-empty-p provider-elisp)))
              (setq ekg-llm-provider (eval (read provider-elisp))))))
      (error
       (with-temp-file ,error-file
         (insert (format "Init error: %S\n" err))
         (insert (format "Load path:\n%s\n"
                         (mapconcat #'identity load-path "\n"))))))))

(defun ekg-agent-bench--eval-in-emacs (emacs-info sexp)
  "Evaluate SEXP in the Emacs subprocess EMACS-INFO.
Returns the output string, or signals an error on failure."
  (let* ((server-name (plist-get emacs-info :server-name))
         (socket-dir (plist-get emacs-info :socket-dir))
         (socket-file (expand-file-name server-name socket-dir))
         (sexp-str (if (stringp sexp) sexp (format "%S" sexp)))
         (out-buf (generate-new-buffer " *ekg-bench-eval*")))
    (unwind-protect
        (let ((exit-code
               (call-process "emacsclient" nil out-buf nil
                             "--socket-name" socket-file
                             "--eval" sexp-str)))
          (with-current-buffer out-buf
            (let ((output (buffer-string)))
              (if (= exit-code 0)
                  (string-trim output)
                (error "Emacsclient failed with exit code %d: %s"
                       exit-code output)))))
      (kill-buffer out-buf))))

(defun ekg-agent-bench--eval-in-emacs-async (emacs-info sexp)
  "Evaluate SEXP in the Emacs subprocess EMACS-INFO asynchronously.
Returns a futur that resolves to the output string."
  (let* ((server-name (plist-get emacs-info :server-name))
         (socket-dir (plist-get emacs-info :socket-dir))
         (socket-file (expand-file-name server-name socket-dir))
         (sexp-str (if (stringp sexp) sexp (format "%S" sexp)))
         (out-buf (generate-new-buffer " *ekg-bench-eval-async*"))
         (f (futur-process-call "emacsclient" nil out-buf nil
                                "--socket-name" socket-file
                                "--eval" sexp-str)))
    (futur-bind
     f
     (lambda (exit-code)
       (let ((output (with-current-buffer out-buf
                       (string-trim (buffer-string)))))
         (kill-buffer out-buf)
         (if (= exit-code 0)
             output
           (error "Emacsclient failed with exit code %d: %s"
                  exit-code output)))))))

(defun ekg-agent-bench--stop-emacs (emacs-info)
  "Stop the Emacs subprocess EMACS-INFO and clean up."
  (let* ((server-name (plist-get emacs-info :server-name))
         (socket-dir (plist-get emacs-info :socket-dir))
         (socket-file (expand-file-name server-name socket-dir))
         (init-file (plist-get emacs-info :init-file))
         (process (plist-get emacs-info :process)))
    (ignore-errors
      (call-process "emacsclient" nil nil nil
                    "--socket-name" socket-file
                    "--eval" "(kill-emacs)"))
    ;; Give it a moment to exit.
    (let ((deadline (+ (float-time) 5)))
      (while (and (process-live-p process)
                  (< (float-time) deadline))
        (sit-for 0.1)))
    (when (process-live-p process)
      (kill-process process))
    (ignore-errors (delete-directory socket-dir t))
    (ignore-errors (delete-file init-file))))

(defvar ekg-agent-bench--server-counter 0
  "Counter for generating unique server names.")

(defun ekg-agent-bench--start-emacs ()
  "Start a fresh Emacs subprocess for benchmarking (synchronous).
Like `llm-test--start-emacs' but with better error reporting when
the daemon fails to start.  Blocks until the daemon is ready.  Use
`ekg-agent-bench--start-emacs-async' from timer/callback contexts."
  ;; Use `bench-server-name' not `server-name': the latter is a
  ;; defcustom from server.el (special/dynamic variable), so a
  ;; let-binding would NOT be captured by closures that run outside
  ;; this dynamic extent (e.g. futur callbacks, timers).
  (let* ((load-paths (ekg-agent-bench--compute-load-paths))
         (error-file (make-temp-file "ekg-bench-init-error-"))
         (init-forms (ekg-agent-bench--init-forms error-file))
         (bench-server-name (format "ekg-bench-%d-%d"
                                    (emacs-pid)
                                    (cl-incf ekg-agent-bench--server-counter)))
         (socket-dir (make-temp-file "ekg-bench-socket-" t))
         (init-file (make-temp-file "ekg-bench-init-" nil ".el"))
         (buf-name (format " *ekg-bench-emacs-%s*" bench-server-name)))
    (with-temp-file init-file
      (insert (format "(setq server-socket-dir %S server-name %S)\n"
                      socket-dir bench-server-name))
      (dolist (dir load-paths)
        (insert (format "(add-to-list 'load-path %S)\n" dir)))
      (dolist (form init-forms)
        (insert (format "%S\n" form))))
    (let* ((emacs-bin (ekg-agent-bench--emacs-executable))
           (process (start-process
                     (format "ekg-bench-emacs-%s" bench-server-name)
                     buf-name
                     emacs-bin
                     "-Q"
                     "-l" init-file
                     (format "--daemon=%s" bench-server-name)))
           (socket-file (expand-file-name bench-server-name socket-dir))
           (deadline (+ (float-time) ekg-agent-bench-default-timeout)))
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
                        :server-name bench-server-name
                        :socket-dir socket-dir
                        :init-file init-file)))
        (ekg-agent-bench--eval-in-emacs
         info
         (format "(set-frame-size (selected-frame) %d %d)"
                 ekg-agent-bench-frame-width ekg-agent-bench-frame-height))
        info))))

(defun ekg-agent-bench--wait-for-socket-async (socket-file process deadline
                                                           cleanup-fn)
  "Poll for SOCKET-FILE to appear, returning a futur.
PROCESS is the daemon process.  DEADLINE is `float-time' cutoff.
CLEANUP-FN is called with no args on failure to clean up temp files.
Resolves to t on success; signals error on timeout/failure."
  (cond
   ((file-exists-p socket-file) (ekg-agent-bench--resolved t))
   ((>= (float-time) deadline)
    (when (process-live-p process) (kill-process process))
    (funcall cleanup-fn)
    (futur-failed '(error "Daemon timed out waiting for socket")))
   ((not (process-live-p process))
    (funcall cleanup-fn)
    (futur-failed '(error "Daemon process exited before socket appeared")))
   (t
    (futur-let* ((_ <- (futur-timeout 0.2)))
      (ekg-agent-bench--wait-for-socket-async
       socket-file process deadline cleanup-fn)))))

(defun ekg-agent-bench--start-emacs-async ()
  "Start a fresh Emacs subprocess for benchmarking (async).
Returns a futur that resolves to an emacs-info plist.  Unlike
`ekg-agent-bench--start-emacs', this never blocks the event loop."
  (let* ((load-paths (ekg-agent-bench--compute-load-paths))
         (error-file (make-temp-file "ekg-bench-init-error-"))
         (init-forms (ekg-agent-bench--init-forms error-file))
         ;; Use `bench-server-name' not `server-name': the latter is a
         ;; defcustom from server.el (special/dynamic variable), so a
         ;; let-binding would NOT be captured by futur-let* closures
         ;; that run outside this dynamic extent.
         (bench-server-name (format "ekg-bench-%d-%d"
                                    (emacs-pid)
                                    (cl-incf ekg-agent-bench--server-counter)))
         (socket-dir (make-temp-file "ekg-bench-socket-" t))
         (init-file (make-temp-file "ekg-bench-init-" nil ".el"))
         (buf-name (format " *ekg-bench-emacs-%s*" bench-server-name)))
    (with-temp-file init-file
      (insert (format "(setq server-socket-dir %S server-name %S)\n"
                      socket-dir bench-server-name))
      (dolist (dir load-paths)
        (insert (format "(add-to-list 'load-path %S)\n" dir)))
      (dolist (form init-forms)
        (insert (format "%S\n" form))))
    (let* ((emacs-bin (ekg-agent-bench--emacs-executable))
           (process (start-process
                     (format "ekg-bench-emacs-%s" bench-server-name)
                     buf-name emacs-bin "-Q" "-l" init-file
                     (format "--daemon=%s" bench-server-name)))
           (socket-file (expand-file-name bench-server-name socket-dir))
           (deadline (+ (float-time) ekg-agent-bench-default-timeout))
           (cleanup (lambda ()
                      (ignore-errors (delete-directory socket-dir t))
                      (ignore-errors (delete-file init-file))
                      (ignore-errors (delete-file error-file)))))
      (futur-let*
          ((_ <- (ekg-agent-bench--wait-for-socket-async
                  socket-file process deadline cleanup)))
        ;; Check init error file.
        (when (and (file-exists-p error-file)
                   (> (file-attribute-size (file-attributes error-file)) 0))
          (let ((init-error (with-temp-buffer
                              (insert-file-contents error-file)
                              (buffer-string))))
            (ignore-errors (kill-process process))
            (funcall cleanup)
            (error "Daemon started but init failed:\n%s" init-error)))
        (ignore-errors (delete-file error-file))
        (let ((info (list :process process
                          :server-name bench-server-name
                          :socket-dir socket-dir
                          :init-file init-file)))
          ;; Set frame size asynchronously.
          (futur-let*
              ((_ <- (ekg-agent-bench--eval-in-emacs-async
                      info
                      (format "(set-frame-size (selected-frame) %d %d)"
                              ekg-agent-bench-frame-width ekg-agent-bench-frame-height))))
            (ekg-agent-bench--resolved info)))))))

;;; Agent Polling and Metric Extraction

(defun ekg-agent-bench--find-log-buffer (emacs-info)
  "Find the ekg agent log buffer name in the subprocess EMACS-INFO.
Return the buffer name string, or nil if not found."
  (let ((result (ekg-agent-bench--eval-in-emacs
                 emacs-info
                 "(car (seq-filter
                        (lambda (b) (string-match-p \"\\\\*ekg agent log:\" (buffer-name b)))
                        (buffer-list)))")))
    (unless (or (equal result "nil") (string-empty-p result))
      ;; Result is printed as #<buffer NAME>, extract the name.
      (when (string-match "\\*ekg agent log:[^*]+\\*" result)
        (match-string 0 result)))))

(defun ekg-agent-bench--agent-running-p (emacs-info log-buffer-name)
  "Check if the agent is still running in EMACS-INFO log buffer LOG-BUFFER-NAME."
  (let ((result (ekg-agent-bench--eval-in-emacs
                 emacs-info
                 (format "(if (and (get-buffer %S)
                                   (buffer-local-value 'ekg-agent--running-p
                                                       (get-buffer %S)))
                              \"running\" \"stopped\")"
                         log-buffer-name log-buffer-name))))
    (equal result "\"running\"")))

(defun ekg-agent-bench--poll-until-done (emacs-info timeout)
  "Poll the subprocess EMACS-INFO until the agent finishes.
Return `done' if the agent finished, `timeout' if TIMEOUT seconds elapse."
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

(defun ekg-agent-bench--parse-log-timestamp (timestamp)
  "Parse TIMESTAMP from the agent log into a float time."
  (unless (string-match
           "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\'"
           timestamp)
    (error "Unrecognized benchmark log timestamp: %s" timestamp))
  (float-time
   (encode-time (string-to-number (match-string 6 timestamp))
                (string-to-number (match-string 5 timestamp))
                (string-to-number (match-string 4 timestamp))
                (string-to-number (match-string 3 timestamp))
                (string-to-number (match-string 2 timestamp))
                (string-to-number (match-string 1 timestamp)))))

(defun ekg-agent-bench--parse-tool-log-line (line)
  "Parse a tool log LINE.
Return a cons of (TIMESTAMP . TOOL-NAME), or nil if LINE is not a tool line."
  (when (string-match
         "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)[[:space:]]+\\(?:STARTED\\|DONE\\)[[:space:]]+\\([^[:space:]]+\\)[[:space:]]*$"
         line)
    (let ((timestamp-text (match-string 1 line))
          (tool-name (match-string 2 line)))
      (cons (ekg-agent-bench--parse-log-timestamp timestamp-text)
            tool-name))))

(defun ekg-agent-bench--status-update-metrics (content)
  "Extract explicit visible status-update metrics from benchmark log CONTENT."
  (let (all-times status-times)
    (dolist (line (split-string content "\n" t))
      (when (string-match
             "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\) \\(.*\\)$"
             line)
        (let* ((timestamp-text (match-string 1 line))
               (message-text (match-string 2 line))
               (timestamp
                (ekg-agent-bench--parse-log-timestamp timestamp-text)))
          (push timestamp all-times)
          (when (string-prefix-p "State: " message-text)
            (push timestamp status-times)))))
    (setq all-times (nreverse all-times)
          status-times (nreverse status-times))
    (let ((max-gap
           (when all-times
             (let ((points (append (list (car all-times))
                                   status-times
                                   (list (car (last all-times))))))
               (cl-loop for prev = nil then current
                        for current in points
                        when prev maximize (- current prev) into gap
                        finally return (or gap 0.0))))))
      (list :status-update-count (length status-times)
            :max-status-update-gap max-gap))))

(defun ekg-agent-bench--extract-metrics (emacs-info)
  "Extract benchmark metrics from the agent log buffer in EMACS-INFO.
Return a plist including iterations, tools, status-update data, and log text."
  (let* ((log-content
          (ekg-agent-bench--eval-in-emacs
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
         (tools nil)
         (status-metrics (ekg-agent-bench--status-update-metrics content)))
    (dolist (line lines)
      (when-let ((tool-line (ekg-agent-bench--parse-tool-log-line line)))
        (cl-incf iterations)
        (push (cdr tool-line) tools)))
    (list :iterations iterations
          :tools-used (delete-dups (nreverse tools))
          :status-update-count (plist-get status-metrics :status-update-count)
          :max-status-update-gap
          (plist-get status-metrics :max-status-update-gap)
          :log content)))

(defun ekg-agent-bench--eval-verify (emacs-info verify-expr)
  "Evaluate VERIFY-EXPR in the subprocess EMACS-INFO.
Return t if truthy, `skip' if VERIFY-EXPR is nil (not applicable),
nil if the expression evaluated to false."
  (if (null verify-expr)
      'skip
    (let ((result (ekg-agent-bench--eval-in-emacs
                   emacs-info
                   (format "(if (progn %s) \"pass\" \"fail\")" verify-expr))))
      (equal result "\"pass\""))))

(defun ekg-agent-bench--combine-check-results (&rest results)
  "Combine pass/fail/skip RESULTS, treating `skip' as neutral."
  (let ((saw-pass nil))
    (catch 'result
      (dolist (result results)
        (cond
         ((eq result 'skip))
         ((null result) (throw 'result nil))
         (t (setq saw-pass t))))
      (if saw-pass t 'skip))))

(defun ekg-agent-bench--append-error-message (existing new-message)
  "Append NEW-MESSAGE to EXISTING, separating messages cleanly."
  (if (and existing (not (string-empty-p existing)))
      (concat existing "; " new-message)
    new-message))

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
          (ekg-agent-bench--eval-in-emacs
           emacs-info
           (format "(progn %s nil)" group-setup))
        (error (setq error-msg (format "Group setup failed: %s"
                                       (error-message-string err))))))
    ;; Run task-specific setup.
    (when (and (not error-msg)
               (ekg-agent-bench-task-setup task)
               (not (string-empty-p (ekg-agent-bench-task-setup task))))
      (condition-case err
          (ekg-agent-bench--eval-in-emacs
           emacs-info
           (format "(progn %s nil)" (ekg-agent-bench-task-setup task)))
        (error (setq error-msg (format "Task setup failed: %s"
                                       (error-message-string err))))))
    ;; Trigger the agent.  We schedule it via run-at-time so that
    ;; emacsclient returns immediately — the agent's iteration-0
    ;; does a synchronous LLM call (prompt-id) that would otherwise
    ;; block the emacsclient eval.  Wrap the timer body in a
    ;; `condition-case' that stores any async error in a subprocess
    ;; global so we can retrieve it on timeout.
    (when (not error-msg)
      (condition-case err
          (ekg-agent-bench--eval-in-emacs
           emacs-info
           (format
            "(progn
               (setq ekg-agent-bench--last-trigger-error nil)
               (run-at-time 0 nil
                            (lambda ()
                              (condition-case err
                                  (progn %s)
                                (error
                                 (setq ekg-agent-bench--last-trigger-error
                                       (format \"%%S\" err))))))
               nil)"
            (ekg-agent-bench-task-trigger task)))
        (error (setq error-msg (format "Trigger failed: %s"
                                       (error-message-string err))))))
    ;; Poll for completion.
    (if error-msg
        (setq status 'error)
      (setq status (ekg-agent-bench--poll-until-done emacs-info timeout)))
    ;; If the trigger's async lambda raised, surface it now.
    (unless (eq status 'error)
      (let ((async-err
             (ignore-errors
               (ekg-agent-bench--eval-in-emacs
                emacs-info
                "(if (boundp 'ekg-agent-bench--last-trigger-error)
                     (or ekg-agent-bench--last-trigger-error \"nil\")
                   \"nil\")"))))
        (when (and async-err
                   (stringp async-err)
                   (not (equal async-err "\"nil\""))
                   (not (equal async-err "nil")))
          (setq error-msg
                (ekg-agent-bench--append-error-message
                 error-msg
                 (format "Trigger async error: %s"
                         (if (and (> (length async-err) 1)
                                  (eq (aref async-err 0) ?\"))
                             (read async-err)
                           async-err)))))))
    ;; Collect metrics.
    (let* ((wall-time (- (float-time) start-time))
           (metrics (unless (eq status 'error)
                      (ekg-agent-bench--extract-metrics emacs-info)))
           (task-check (unless (eq status 'error)
                         (condition-case err
                             (ekg-agent-bench--eval-verify
                              emacs-info
                              (ekg-agent-bench-task-verify-task task))
                           (error
                            (setq error-msg
                                  (format "verify-task error: %s"
                                          (error-message-string err)))
                            nil))))
           (status-gap-target (ekg-agent-bench-task-max-status-update-gap task))
           (status-gap-passed (if (or (eq status 'error)
                                      (null status-gap-target)
                                      (null metrics))
                                  'skip
                                (let ((gap (plist-get metrics :max-status-update-gap)))
                                  (and (numberp gap)
                                       (<= gap status-gap-target)))))
           (task-passed (if (eq status 'timeout)
                            nil
                          (ekg-agent-bench--combine-check-results
                           task-check status-gap-passed)))
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
      (when (eq status 'timeout)
        (setq error-msg
              (ekg-agent-bench--append-error-message
               error-msg
               "task timed out")))
      (when (null status-gap-passed)
        (setq error-msg
              (ekg-agent-bench--append-error-message
               error-msg
               (format "status update gap %.1fs exceeded target %.1fs"
                       (or (plist-get metrics :max-status-update-gap) 0.0)
                       status-gap-target))))
      (make-ekg-agent-bench-result
       :name task-name
       :task-passed task-passed
       :skill-passed skill-passed
       :memory-passed memory-passed
       :iterations (or (plist-get metrics :iterations) 0)
       :tools-used (or (plist-get metrics :tools-used) nil)
       :wall-time wall-time
       :status-update-count (or (plist-get metrics :status-update-count) 0)
       :max-status-update-gap (plist-get metrics :max-status-update-gap)
       :status-update-gap-target status-gap-target
       :status status
       :error-message error-msg
       :agent-log (plist-get metrics :log)))))

;;; Results Display

(defun ekg-agent-bench--format-pass (val)
  "Format a pass/fail/skip VAL as a short display string."
  (cond
   ((eq val 'skip) "n/a")
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
              (unless (eq tp 'skip) (cl-incf task-total) (when (eq tp t) (cl-incf task-pass)))
              (unless (eq sp 'skip) (cl-incf skill-total) (when (eq sp t) (cl-incf skill-pass)))
              (unless (eq mp 'skip) (cl-incf memory-total) (when (eq mp t) (cl-incf memory-pass))))
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
            (when (or (ekg-agent-bench-result-status-update-gap-target r)
                      (ekg-agent-bench-result-status-update-count r))
              (insert (format "  Status updates: %d"
                              (or (ekg-agent-bench-result-status-update-count r)
                                  0)))
              (let ((gap (ekg-agent-bench-result-max-status-update-gap r)))
                (when gap
                  (insert (format "  Max gap: %.1fs" gap))))
              (let ((target
                     (ekg-agent-bench-result-status-update-gap-target r)))
                (when target
                  (insert (format "  Target: %.1fs" target))))
              (insert "\n"))
            (when (ekg-agent-bench-result-error-message r)
              (insert (format "  ERROR: %s\n"
                              (ekg-agent-bench-result-error-message r))))
            (when (ekg-agent-bench-result-agent-log r)
              (insert "\n  Agent log:\n")
              (dolist (line (split-string (ekg-agent-bench-result-agent-log r) "\n"))
                (unless (string-empty-p line)
                  (insert (format "  | %s\n" line))))
              (insert "\n")))
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
  (message "ekg-agent-bench: computing load paths...")
  (let ((paths (ekg-agent-bench--compute-load-paths)))
    (message "  %d directories on load-path" (length paths))
    (dolist (p paths)
      (message "    %s" p)))
  (message "ekg-agent-bench: starting subprocess (timeout %ds)..."
           ekg-agent-bench-default-timeout)
  (let ((info (ekg-agent-bench--start-emacs)))
    (unwind-protect
        (progn
          (message "  daemon started OK")
          (dolist (lib '("ekg" "ekg-agent" "ekg-llm" "ekg-org" "ekg-embedding"
                         "llm" "triples" "futur" "vui"))
            (let ((result (ekg-agent-bench--eval-in-emacs
                           info
                           (format "(if (featurep '%s) \"loaded\" \"NOT loaded\")"
                                   lib))))
              (message "  %s: %s" lib result)))
          (message "  ekg-db: %s"
                   (ekg-agent-bench--eval-in-emacs info "(if ekg-db \"connected\" \"nil\")"))
          (message "  provider: %s"
                   (ekg-agent-bench--eval-in-emacs info "(type-of ekg-llm-provider)"))
          (message "  ekg-db-file: %s"
                   (ekg-agent-bench--eval-in-emacs info "ekg-db-file"))
          (message "ekg-agent-bench: diagnosis PASSED"))
      (ekg-agent-bench--stop-emacs info))))

;;; ERT Registration

(defun ekg-agent-bench-register-ert-tests (directory)
  "Register benchmark tasks from DIRECTORY as ERT test cases."
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
                      (unless (getenv "LLM_TEST_PROVIDER_ELISP")
                        (ert-skip "LLM_TEST_PROVIDER_ELISP not set"))
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
                          (ekg-agent-bench--stop-emacs emacs-info))))))))))))

;;; Entry Points

;;; Async Task Runner (futur-based)

(defun ekg-agent-bench--resolved (value)
  "Return a futur that resolves to VALUE on the next timer cycle.
Unlike `futur-done' (which returns a raw value, causing `futur-bind'
to dispatch via the background thread if one exists), this always
delivers VALUE through a timer on the main thread."
  (let ((f (futur-new #'ignore)))
    (run-at-time 0 nil #'futur-deliver-value f value)
    f))

(defun ekg-agent-bench--ensure-no-background-thread ()
  "Ensure the futur background thread is not active.
The futur library creates a background thread when `futur-use-threads'
is non-nil.  That thread holds the Emacs GIL during continuations,
and calling `message' from it crashes the macOS NS port.  We never
need it — all our async work goes through process sentinels and timers.

Uses `error' signal instead of `quit' because the thread runs with
`inhibit-quit' t."
  (when (and (boundp 'futur--background) futur--background)
    (when (and (fboundp 'thread-alive-p)
               (thread-alive-p futur--background))
      ;; Signal `error' not `quit' — the thread has (inhibit-quit t).
      (thread-signal futur--background 'error
                     '("futur background thread killed by ekg-agent-bench"))
      ;; Give the thread a moment to die.
      (sit-for 0.1))
    (setq futur--background nil)))

(defun ekg-agent-bench--safe-message (fmt &rest args)
  "Like `message' with FMT and ARGS, but safe to call from any thread.
On the main thread, call `message' directly.  On a background thread,
bounces via `run-at-time' to avoid the macOS NS port deadlock where
`message' triggers redisplay from a non-main thread."
  (if (eq (current-thread) main-thread)
      (apply #'message fmt args)
    (apply #'run-at-time 0 nil #'message fmt args)))

(defun ekg-agent-bench--safe-funcall (fn &rest args)
  "Like `funcall' but guaranteed to run FN with ARGS on the main thread.
If already on the main thread, calls directly.  Otherwise bounces
via `run-at-time'.  Use for callbacks that do UI work (e.g.
`pop-to-buffer', `ekg-agent-bench--display-results')."
  (if (eq (current-thread) main-thread)
      (apply fn args)
    (apply #'run-at-time 0 nil fn args)))

(defun ekg-agent-bench--eval-async (emacs-info expr)
  "Evaluate EXPR in the subprocess EMACS-INFO asynchronously.
Return a futur that resolves to the result string."
  (ekg-agent-bench--eval-in-emacs-async emacs-info expr))

(defun ekg-agent-bench--poll-step-async (emacs-info deadline log-buf)
  "One async poll step in EMACS-INFO until DEADLINE with LOG-BUF.
Return a futur resolving to `done' or `timeout'."
  (if (>= (float-time) deadline)
      (ekg-agent-bench--resolved 'timeout)
    (if (not log-buf)
        ;; Still waiting for log buffer to appear.
        (futur-let*
            ((_ <- (futur-timeout ekg-agent-bench-poll-interval))
             (found <- (ekg-agent-bench--eval-async
                        emacs-info
                        "(let ((buf (car (seq-filter
                                          (lambda (b) (string-match-p \"\\\\\\\\*ekg agent log:\" (buffer-name b)))
                                          (buffer-list)))))
                           (if buf (buffer-name buf) \"nil\"))")))
          ;; found is e.g. "\"*ekg agent log: foo*\"" or "nil".
          (let ((buf-name (when (and found
                                     (not (equal found "nil"))
                                     (> (length found) 2)
                                     (eq (aref found 0) ?\"))
                            (read found))))
            (ekg-agent-bench--poll-step-async
             emacs-info deadline buf-name)))
      ;; Log buffer exists, check if agent is still running.
      (futur-let*
          ((_ <- (futur-timeout ekg-agent-bench-poll-interval))
           (result <- (ekg-agent-bench--eval-async
                       emacs-info
                       (format "(with-current-buffer %S
                                  (if (and (boundp 'ekg-agent--running-p)
                                           ekg-agent--running-p)
                                      \"running\" \"stopped\"))"
                               log-buf))))
        (if (equal result "\"running\"")
            (ekg-agent-bench--poll-step-async emacs-info deadline log-buf)
          (ekg-agent-bench--resolved 'done))))))

(defun ekg-agent-bench--poll-until-done-async (emacs-info timeout)
  "Poll EMACS-INFO asynchronously until the agent finishes or TIMEOUT is reached.
Return a futur resolving to `done' or `timeout'."
  (let ((deadline (+ (float-time) timeout)))
    (ekg-agent-bench--poll-step-async emacs-info deadline nil)))

(defun ekg-agent-bench--extract-metrics-async (emacs-info)
  "Async version of `ekg-agent-bench--extract-metrics' for EMACS-INFO.
Return a futur resolving to a plist."
  (futur-let*
      ((log-content <- (ekg-agent-bench--eval-async
                        emacs-info
                        "(let ((buf (car (seq-filter
                                          (lambda (b) (string-match-p \"\\\\\\\\*ekg agent log:\" (buffer-name b)))
                                          (buffer-list)))))
                           (if buf
                               (with-current-buffer buf
                                 (buffer-substring-no-properties (point-min) (point-max)))
                             \"\"))")))
    (let* ((content (if (and (> (length log-content) 1)
                             (eq (aref log-content 0) ?\"))
                        (read log-content)
                      log-content))
           (lines (split-string content "\n"))
           (iterations 0)
           (tools nil)
           (status-metrics (ekg-agent-bench--status-update-metrics content)))
      (dolist (line lines)
        (when-let ((tool-line (ekg-agent-bench--parse-tool-log-line line)))
          (cl-incf iterations)
          (push (cdr tool-line) tools)))
      (ekg-agent-bench--resolved
       (list :iterations iterations
             :tools-used (delete-dups (nreverse tools))
             :status-update-count
             (plist-get status-metrics :status-update-count)
             :max-status-update-gap
             (plist-get status-metrics :max-status-update-gap)
             :log content)))))

(defun ekg-agent-bench--eval-verify-async (emacs-info verify-expr)
  "Evaluate VERIFY-EXPR asynchronously in EMACS-INFO.
Return a futur resolving to t, nil, or `skip'."
  (if (null verify-expr)
      (ekg-agent-bench--resolved 'skip)
    (futur-let*
        ((result <- (ekg-agent-bench--eval-async
                     emacs-info
                     (format "(if (progn %s) \"pass\" \"fail\")" verify-expr))))
      (ekg-agent-bench--resolved (equal result "\"pass\"")))))

(defun ekg-agent-bench--run-task-async (emacs-info group-setup task)
  "Run a single benchmark TASK asynchronously in EMACS-INFO with GROUP-SETUP.
Return a futur resolving to an `ekg-agent-bench-result'."
  (let ((start-time (float-time))
        (timeout (or (ekg-agent-bench-task-timeout task)
                     ekg-agent-bench-default-timeout))
        (task-name (or (ekg-agent-bench-task-name task) "unnamed")))
    (ekg-agent-bench--safe-message "bench: running %s..." task-name)
    (futur-let*
        ;; Group setup.
        ((setup-result
          <- (if (and group-setup (not (string-empty-p group-setup)))
                 (ekg-agent-bench--eval-async
                  emacs-info (format "(progn %s nil)" group-setup))
               (ekg-agent-bench--resolved nil)))
         ;; Task-specific setup.
         (task-setup-result
          <- (if (and (ekg-agent-bench-task-setup task)
                      (not (string-empty-p (ekg-agent-bench-task-setup task))))
                 (ekg-agent-bench--eval-async
                  emacs-info
                  (format "(progn %s nil)" (ekg-agent-bench-task-setup task)))
               (ekg-agent-bench--resolved nil)))
         ;; Trigger.
         (trigger-result
          <- (ekg-agent-bench--eval-async
              emacs-info
              (format "(progn (run-at-time 0 nil (lambda () %s)) nil)"
                      (ekg-agent-bench-task-trigger task))))
         ;; Poll for completion.
         (status <- (ekg-agent-bench--poll-until-done-async emacs-info timeout))
         ;; Collect metrics.
         (metrics <- (ekg-agent-bench--extract-metrics-async emacs-info))
         ;; Verify.
         (task-check
          <- (ekg-agent-bench--eval-verify-async
              emacs-info (ekg-agent-bench-task-verify-task task)))
         (skill-passed
          <- (ekg-agent-bench--eval-verify-async
              emacs-info (ekg-agent-bench-task-verify-skill task)))
         (memory-passed
          <- (ekg-agent-bench--eval-verify-async
              emacs-info (ekg-agent-bench-task-verify-memory task))))
      ;; Suppress byte-compiler warnings for unused variables.
      (ignore setup-result task-setup-result trigger-result)
      (let* ((wall-time (- (float-time) start-time))
             (status-gap-target
              (ekg-agent-bench-task-max-status-update-gap task))
             (status-gap-passed
              (if (or (eq status 'error)
                      (null status-gap-target)
                      (null metrics))
                  'skip
                (let ((gap (plist-get metrics :max-status-update-gap)))
                  (and (numberp gap)
                       (<= gap status-gap-target)))))
             (task-passed (if (eq status 'timeout)
                              nil
                            (ekg-agent-bench--combine-check-results
                             task-check status-gap-passed)))
             (error-message
              (cond
               ((eq status 'timeout)
                "task timed out")
               ((null status-gap-passed)
                (format "status update gap %.1fs exceeded target %.1fs"
                        (or (plist-get metrics :max-status-update-gap) 0.0)
                        status-gap-target)))))
        (ekg-agent-bench--safe-message "bench: %s completed (%s, %.0fs)"
                                       task-name status wall-time)
        (ekg-agent-bench--resolved
         (make-ekg-agent-bench-result
          :name task-name
          :task-passed task-passed
          :skill-passed skill-passed
          :memory-passed memory-passed
          :iterations (or (plist-get metrics :iterations) 0)
          :tools-used (or (plist-get metrics :tools-used) nil)
          :wall-time wall-time
          :status-update-count (or (plist-get metrics :status-update-count) 0)
          :max-status-update-gap (plist-get metrics :max-status-update-gap)
          :status-update-gap-target status-gap-target
          :status status
          :error-message error-message
          :agent-log (plist-get metrics :log)))))))

(defun ekg-agent-bench--make-error-result (task-name error-msg)
  "Create an error result for TASK-NAME with ERROR-MSG."
  (make-ekg-agent-bench-result
   :name task-name :status 'error :error-message error-msg
   :wall-time 0 :iterations 0 :status-update-count 0))

(defun ekg-agent-bench--run-tasks-sequentially (task-specs results-so-far callback)
  "Run TASK-SPECS sequentially, each in a fresh subprocess.
TASK-SPECS is a list of (group-setup . task) pairs.
Accumulates results into RESULTS-SO-FAR (in reverse).
Calls CALLBACK with the final list of results (in order).

Fully async: daemon start, setup, trigger, polling, and verification
all use futur-based non-blocking IO."
  (if (null task-specs)
      (ekg-agent-bench--safe-funcall callback (nreverse results-so-far))
    (let* ((spec (car task-specs))
           (group-setup (car spec))
           (task (cdr spec))
           (task-name (ekg-agent-bench-task-name task))
           (rest (cdr task-specs)))
      (futur-bind
       (ekg-agent-bench--start-emacs-async)
       (lambda (emacs-info)
         ;; Daemon running — run the task async.
         (futur-bind
          (ekg-agent-bench--run-task-async emacs-info group-setup task)
          (lambda (val)
            (ekg-agent-bench--stop-emacs emacs-info)
            (push val results-so-far)
            (ekg-agent-bench--run-tasks-sequentially
             rest results-so-far callback))
          (lambda (err)
            (ekg-agent-bench--stop-emacs emacs-info)
            (ekg-agent-bench--safe-message
             "bench: error running %s: %S" task-name err)
            (push (ekg-agent-bench--make-error-result
                   task-name (format "%S" err))
                  results-so-far)
            (ekg-agent-bench--run-tasks-sequentially
             rest results-so-far callback))))
       (lambda (start-err)
         ;; Daemon failed to start — record error and continue.
         (ekg-agent-bench--safe-message
          "bench: daemon start failed for %s: %S"
          task-name start-err)
         (push (ekg-agent-bench--make-error-result
                task-name
                (format "Daemon start failed: %S" start-err))
               results-so-far)
         (ekg-agent-bench--run-tasks-sequentially
          rest results-so-far callback))))))

;;; Entry Points (synchronous, for ERT and batch use)

;;;###autoload
(defun ekg-agent-bench-run (&optional directory)
  "Run all benchmark tasks from DIRECTORY and display results.
DIRECTORY defaults to the benchmarks/ subdirectory next to this file."
  (interactive)
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
              (ekg-agent-bench--stop-emacs emacs-info))))))
    (setq all-results (nreverse all-results))
    (ekg-agent-bench--display-results
     all-results
     (getenv "LLM_TEST_PROVIDER_ELISP"))
    all-results))

;;;###autoload
(defun ekg-agent-bench-run-one (task-name &optional directory)
  "Run a single benchmark task by TASK-NAME and display results.
DIRECTORY defaults to the benchmarks/ subdirectory."
  (interactive "sTask name: ")
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
             (getenv "LLM_TEST_PROVIDER_ELISP"))
            result)
        (ekg-agent-bench--stop-emacs emacs-info)))))

;;; Entry Points (async, non-blocking)

;;;###autoload
(defun ekg-agent-bench-run-async (&optional directory)
  "Run all benchmarks asynchronously without blocking Emacs.
DIRECTORY defaults to the benchmarks/ subdirectory next to this file.
Results are displayed in *ekg-agent-bench* when all tasks complete."
  (interactive)
  (ekg-agent-bench--ensure-no-background-thread)
  (let* ((dir (or directory
                  (expand-file-name "benchmarks"
                                    (file-name-directory
                                     (or load-file-name
                                         (locate-library "ekg-agent-bench"))))))
         (groups (ekg-agent-bench--load-directory dir))
         (task-specs nil))
    ;; Build a flat list of (group-setup . task) pairs.
    (dolist (group groups)
      (let ((group-setup (ekg-agent-bench-group-setup group)))
        (dolist (task (ekg-agent-bench-group-tasks group))
          (push (cons group-setup task) task-specs))))
    (setq task-specs (nreverse task-specs))
    (message "ekg-agent-bench: starting %d tasks asynchronously..."
             (length task-specs))
    ;; Defer start so the calling command returns immediately.
    (run-at-time 0 nil
                 #'ekg-agent-bench--run-tasks-sequentially
                 task-specs nil
                 (lambda (results)
                   (ekg-agent-bench--display-results
                    results (getenv "LLM_TEST_PROVIDER_ELISP"))
                   (ekg-agent-bench--safe-message
                    "ekg-agent-bench: complete (%d tasks)"
                    (length results))))))

;;;###autoload
(defun ekg-agent-bench-run-one-async (task-name &optional directory)
  "Run a single benchmark TASK-NAME asynchronously without blocking Emacs.
DIRECTORY defaults to the benchmarks/ subdirectory.
Results are displayed in *ekg-agent-bench* when the task completes."
  (interactive "sTask name: ")
  (ekg-agent-bench--ensure-no-background-thread)
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
           (task (cdr found)))
      (message "ekg-agent-bench: running %s asynchronously..." task-name)
      ;; Defer start so the calling command returns immediately.
      (run-at-time 0 nil
                   #'ekg-agent-bench--run-tasks-sequentially
                   (list (cons (ekg-agent-bench-group-setup group) task))
                   nil
                   (lambda (results)
                     (ekg-agent-bench--display-results
                      results (getenv "LLM_TEST_PROVIDER_ELISP"))
                     (ekg-agent-bench--safe-message
                      "ekg-agent-bench: %s complete" task-name))))))

(provide 'ekg-agent-bench)

;;; ekg-agent-bench.el ends here
