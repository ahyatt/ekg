;;; ekg-auto-save.el --- Auto-saving notes -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023  Qingshui Zheng <qingshuizheng@outlook.com>
;;
;; Author: Qingshui Zheng <qingshuizheng@outlook.com>
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
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Automatically save EKG notes. This is a safety measure to prevent
;; you from losing more than a limited amount of work in case of
;; system crashes or accidental buffer kill.
;;
;;; Code:

(require 'seq)
(require 'ekg)

(defgroup ekg-auto-save nil
  "Auto save ekg notes."
  :group 'ekg)

(defcustom ekg-auto-save-timeout auto-save-timeout
  "Number of seconds idle time before auto-save.
This is similar to, and works like `auto-save-timeout'.
Non-positive number or nil means disable auto-saving due to
idleness."
  :type '(choice number (const nil))
  :group 'ekg)

(defcustom ekg-auto-save-interval auto-save-interval
  "Number of input events between auto-saves.
This is similar to, and works like `auto-save-interval'. Zero or
nil means disable auto-saving due to number of keystrokes typed."
  :type '(choice integer (const nil))
  :group 'ekg)

(defcustom ekg-auto-save-command-triggers
  '( next-buffer previous-buffer
     windmove-up windmove-down windmove-left windmove-right
     switch-to-buffer other-window)
  "A list of commands which would trigger `ekg-auto-save-func'."
  :type '(repeat symbol)
  :group 'ekg)

(defcustom ekg-auto-save-hook-triggers nil
  "A list of hooks which would trigger `ekg-auto-save-func'."
  :type '(repeat symbol)
  :group 'ekg)

(defcustom ekg-auto-save-predicate nil
  "Predicate function for `ekg-auto-save-mode'.
If non-nil, the value should be a function of no arguments; it
will be called once in each ekg capture/edit buffer when the time
comes to auto-save. A buffer will be saved only if the predicate
function returns a non-nil value.

For example, you could add this to your Init file to avoid saving
current buffer while in `corfu' completion:

    (setq ekg-auto-save-predicate
          (lambda ()
            (not (boundp \\='corfu--total))))

Or, if you are right in the middle of a transient window, you
do not want the buffer to be saved yet:

    (setq ekg-auto-save-predicate
          (lambda ()
            (when (featurep \\='transient)
              (not (window-live-p transient--window)))))

If the value of this variable is not a function, it is ignored.
This is the same as having a predicate that always returns
non-nil."
  :type '(choice :tag "Function:"
                 (const :tag "No extra predicate" :value nil)
                 (function :tag "Predicate function" :value always))
  :group 'ekg)


;;; Idle timer

(defvar-local ekg-auto-save-idle-timer nil)

(defun ekg-auto-save-initialize-idle-timer ()
  "Initialize `ekg-auto-save' idle timer."
  (when (and (numberp ekg-auto-save-timeout)
             (> ekg-auto-save-timeout 0))
    (setq ekg-auto-save-idle-timer
          (run-with-idle-timer
           ekg-auto-save-timeout t #'ekg-auto-save-func))))

(defun ekg-auto-save-stop-idle-timer ()
  "Stop `ekg-auto-save' idle timer."
  (when ekg-auto-save-idle-timer
    (cancel-timer ekg-auto-save-idle-timer)))


;;; Keystroke counter

(defvar-local ekg-auto-save-keystroke-counter 0
  "Counter to track the number of keystrokes.")

(defun ekg-auto-save-after-some-keystrokes ()
  "Save note after every certain number of keystrokes.
The number of keystrokes are set with `ekg-auto-save-interval'."
  (when (and (integerp ekg-auto-save-interval)
             (> ekg-auto-save-interval 0))
    (setq ekg-auto-save-keystroke-counter
          (1+ ekg-auto-save-keystroke-counter))
    (when (>= ekg-auto-save-keystroke-counter ekg-auto-save-interval)
      (setq ekg-auto-save-keystroke-counter 0)
      (ekg-auto-save-func))))


;;; Command triggers

(defun ekg-auto-save-func-advice (&rest _args)
  "A simple wrapper around `ekg-auto-save-func'.
Change the args of `ekg-auto-save-func'to make commands in
`ekg-auto-save-command-triggers' advice-friendly."
  (ekg-auto-save-func))

(defun ekg-auto-save-advice-trigger-commands ()
  "Apply advices to commands in `ekg-auto-save-command-triggers'."
  (mapc (lambda (command)
          (advice-add command :before #'ekg-auto-save-func-advice))
        ekg-auto-save-command-triggers))

(defun ekg-auto-save-remove-advice-from-trigger-commands ()
  "Remove advices from commands in `ekg-auto-save-command-triggers'."
  (mapc (lambda (command)
          (advice-remove command #'ekg-auto-save-func-advice))
        ekg-auto-save-command-triggers))


;;; Ekg-auto-save functions

(defun ekg-auto-save-func ()
  "Save the current note.
In capture buffer, note will be saved as draft.
In edit buffer, note will be saved as a normal note.
After save, reset idle timer and keystroke counter."
  ;; TODO Do not auto-save when llm is streaming output?
  (when (and (or ekg-edit-mode ekg-capture-mode)
             (buffer-modified-p)
             ;; avoid creating immature tags from unfinished tags
             (not (ekg--in-metadata-p))
             (not (minibufferp))
             (not (or (use-region-p) (secondary-selection-exist-p)))
             (not (or defining-kbd-macro executing-kbd-macro))
             (or (not (functionp ekg-auto-save-predicate))
                 (funcall ekg-auto-save-predicate)))
    (if ekg-capture-mode
        (ekg-save-draft)
      (ekg-edit-save))
    ;; Reset the idle timer and keystroke counter, so that the next
    ;; auto-save occurs based on whichever comes first.
    (ekg-auto-save-stop-idle-timer)
    (ekg-auto-save-initialize-idle-timer)
    (setq ekg-auto-save-keystroke-counter 0)))

(defun ekg-auto-save-cleanup-on-buffer-kill ()
  "Do some cleanup on buffer kill.
Cancel buffer related idle timer, keystroke counter; remove
advices on commands when there's no editable ekg buffer."
  (ekg-auto-save-stop-idle-timer)
  (remove-hook 'post-self-insert-hook #'ekg-auto-save-after-some-keystrokes t)
  (unless (seq-some
           (lambda (buf)
             (with-current-buffer buf
               (or ekg-capture-mode ekg-edit-mode)))
           (buffer-list))
    (ekg-auto-save-remove-advice-from-trigger-commands)))


;;; Define Minor Mode

(defun ekg-auto-save-initialize ()
  "Initialize `ekg-auto-save'.
1. Initialize idle timer.
2. Initialize keystroke counter.
3. Hook `ekg-auto-save-func' to hook triggers.
4. Advice command-triggers to `ekg-auto-save-func'.
5. Enable saving on buffer kill, and do some cleanup."
  (ekg-auto-save-initialize-idle-timer)
  (add-hook 'post-self-insert-hook #'ekg-auto-save-after-some-keystrokes nil t)
  (dolist (hook ekg-auto-save-hook-triggers)
    (add-hook hook #'ekg-auto-save-func nil t))
  (ekg-auto-save-advice-trigger-commands)
  (add-hook 'kill-buffer-hook #'ekg-auto-save-cleanup-on-buffer-kill nil t))

(defun ekg-auto-save-stop ()
  "Cleanup advices, hooks, timers and keystroke counter."
  (ekg-auto-save-stop-idle-timer)
  (remove-hook 'post-self-insert-hook #'ekg-auto-save-after-some-keystrokes t)
  (dolist (hook ekg-auto-save-hook-triggers)
    (remove-hook hook #'ekg-auto-save-func t))
  (ekg-auto-save-remove-advice-from-trigger-commands)
  (remove-hook 'kill-buffer-hook #'ekg-auto-save-cleanup-on-buffer-kill t))

;;;###autoload
(define-minor-mode ekg-auto-save-mode
  "A minor mode that auto-saves ekg notes."
  :group 'ekg
  :global nil
  (unless (or ekg-edit-mode ekg-capture-mode)
    (user-error "Only available in ekg-capture/edit-mode."))
  (if ekg-auto-save-mode
      (ekg-auto-save-initialize)
    (ekg-auto-save-stop)))

(provide 'ekg-auto-save)
;;; ekg-auto-save.el ends here
