;;; ekg-agent-tools-gnus.el --- Optional Gnus tools for ekg-agent -*- lexical-binding: t -*-

;; Copyright (C) 2026  Andrew Hyatt

;; Author: Andrew Hyatt <ahyatt@gmail.com>

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
;; This package provides optional Gnus tools for `ekg-agent'.  These tools
;; can expose local mail data, so users must opt into them explicitly.

;;; Code:

(require 'cl-lib)
(require 'ekg-agent)
(require 'llm)
(require 'mail-utils)
(require 'seq)
(require 'subr-x)

(eval-when-compile
  (require 'gnus)
  (require 'gnus-group)
  (require 'gnus-search)
  (require 'gnus-start))

(defvar gnus-active-hashtb)
(defvar gnus-check-bogus-newsgroups)
(defvar gnus-check-new-newsgroups)
(defvar gnus-group-buffer)
(defvar gnus-level-subscribed)
(defvar gnus-newsrc-alist)
(defvar gnus-select-method)
(defvar gnus-search-engine-instance-alist)
(defvar gnus-search-use-parsed-queries)
(defvar gnus-verbose)
(defvar gnus-verbose-backends)
(defvar gnus-fetch-old-ephemeral-headers)
(defvar gnus-fetch-old-headers)
(defvar gnus-inhibit-demon)
(defvar gnus-large-ephemeral-newsgroup)
(defvar gnus-large-newsgroup)
(defvar nntp-server-buffer)

(declare-function gnus-get-unread-articles-in-group "gnus-start"
                  (info active &optional update))
(declare-function gnus-group-get-new-news "gnus-group" (&optional arg
                                                                  one-level))
(declare-function gnus-group-native-p "gnus-group" (group))
(declare-function gnus-group-setup-buffer "gnus-group" ())
(declare-function gnus-group-server "gnus-group" (group))
(declare-function gnus-find-method-for-group "gnus" (group))
(declare-function gnus-request-head "gnus-int" (article group))
(declare-function gnus-search-run-query "gnus-search" (specs))
(declare-function gnus-search-server-to-engine "gnus-search" (server))
(declare-function gnus-search-shutdown "gnus-search" ())
(declare-function gnus-setup-news "gnus-start" (&optional rawfile level
                                                          dont-connect))
(declare-function gnus-method-to-server "gnus" (method))
(declare-function gnus-request-article "gnus-int" (article group
                                                          &optional buffer))

(defgroup ekg-agent-tools-gnus nil
  "Optional Gnus tools for ekg-agent."
  :group 'ekg)

(defcustom ekg-agent-tools-gnus-default-max-folders 50
  "Default maximum number of Gnus folders returned by the folder tool."
  :type 'integer
  :group 'ekg-agent-tools-gnus)

(defcustom ekg-agent-tools-gnus-default-max-headers 200
  "Default maximum number of Gnus headers returned by the headers tool."
  :type 'integer
  :group 'ekg-agent-tools-gnus)

(defcustom ekg-agent-tools-gnus-max-headers 2000
  "Maximum number of Gnus headers that can be returned by the headers tool."
  :type 'integer
  :group 'ekg-agent-tools-gnus)

(defcustom ekg-agent-tools-gnus-default-max-search-results 10
  "Default maximum number of Gnus search results returned by the search tool."
  :type 'integer
  :group 'ekg-agent-tools-gnus)

(defcustom ekg-agent-tools-gnus-max-search-results 200
  "Maximum number of Gnus search results returned by the search tool."
  :type 'integer
  :group 'ekg-agent-tools-gnus)

(defcustom ekg-agent-tools-gnus-default-search-candidates 200
  "Default maximum candidate results requested before local sorting."
  :type 'integer
  :group 'ekg-agent-tools-gnus)

(defcustom ekg-agent-tools-gnus-ignore-missing-folders t
  "Whether Gnus search should ignore missing folders when some are valid."
  :type 'boolean
  :group 'ekg-agent-tools-gnus)

(defcustom ekg-agent-tools-gnus-default-max-fetch-changes 20
  "Default maximum number of changed folders returned by the fetch tool."
  :type 'integer
  :group 'ekg-agent-tools-gnus)

(defcustom ekg-agent-tools-gnus-header-max-attempts 1000
  "Maximum article numbers to inspect when looking for recent Gnus headers.
IMAP folders can have gaps in their UID ranges, so finding N recent
headers may require trying more than N article numbers."
  :type 'integer
  :group 'ekg-agent-tools-gnus)

(defun ekg-agent-tools-gnus--bool (value)
  "Return non-nil when VALUE represents true."
  (and value (not (memq value '(false :false :json-false)))))

(defun ekg-agent-tools-gnus--clip-number (value default maximum)
  "Return VALUE as a positive number, or DEFAULT, capped at MAXIMUM."
  (let ((value (if (numberp value) value default)))
    (min maximum (max 1 value))))

(defun ekg-agent-tools-gnus--clean-field (value)
  "Return VALUE as one safe tab-separated field."
  (require 'xml)
  (let ((value (xml-substitute-special (format "%s" (or value "")))))
    (string-trim
     (replace-regexp-in-string
      "[\t\n\r]+" " "
      (replace-regexp-in-string "[\x3fff80-\x3fffff]" "" value)))))

(defun ekg-agent-tools-gnus--clip-string (text max-chars)
  "Return TEXT clipped to MAX-CHARS with a truncation notice."
  (let ((text (or text "")))
    (if (> (length text) max-chars)
        (concat (substring text 0 max-chars)
                "\n\n[Content truncated at "
                (number-to-string max-chars) " characters]")
      text)))

(defmacro ekg-agent-tools-gnus--without-interaction (&rest body)
  "Run BODY with common Gnus and minibuffer prompts disabled."
  (declare (indent 0) (debug t))
  `(let ((gnus-fetch-old-ephemeral-headers nil)
         (gnus-fetch-old-headers nil)
         (gnus-inhibit-demon t)
         (gnus-large-ephemeral-newsgroup nil)
         (gnus-large-newsgroup nil)
         (use-dialog-box nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (&rest _args)
                  (error "Interactive completing-read disabled")))
               ((symbol-function 'read-from-minibuffer)
                (lambda (&rest _args)
                  (error "Interactive minibuffer read disabled")))
               ((symbol-function 'read-string)
                (lambda (&rest _args)
                  (error "Interactive string read disabled")))
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _args) nil))
               ((symbol-function 'y-or-n-p)
                (lambda (&rest _args) nil))
               ((symbol-function 'gnus-yes-or-no-p)
                (lambda (&rest _args) nil))
               ((symbol-function 'gnus-y-or-n-p)
                (lambda (&rest _args) nil))
               ((symbol-function 'map-y-or-n-p)
                (lambda (&rest _args) nil)))
       ,@body)))

(defmacro ekg-agent-tools-gnus--preserve-session (&rest body)
  "Run BODY while restoring user-visible Gnus session state."
  (declare (indent 0) (debug t))
  `(let* ((group-buffer-live
           (and (boundp 'gnus-group-buffer)
                (buffer-live-p gnus-group-buffer)))
          (group-buffer (and group-buffer-live gnus-group-buffer))
          (group-point (and group-buffer-live
                            (with-current-buffer group-buffer (point))))
          (saved-group-window-start
           (and group-buffer-live
                (get-buffer-window group-buffer)
                (window-start (get-buffer-window group-buffer))))
          (search-engine-cache
           (and (boundp 'gnus-search-engine-instance-alist)
                gnus-search-engine-instance-alist)))
     (unwind-protect
         (save-window-excursion
           (save-current-buffer
             ,@body))
       (when (fboundp 'gnus-search-shutdown)
         (ignore-errors (gnus-search-shutdown)))
       (when (boundp 'gnus-search-engine-instance-alist)
         (setq gnus-search-engine-instance-alist search-engine-cache))
       (cond
        ((and group-buffer-live (buffer-live-p group-buffer))
         (with-current-buffer group-buffer
           (goto-char (min group-point (point-max))))
         (when-let* ((window (get-buffer-window group-buffer))
                     (saved-group-window-start saved-group-window-start))
           (set-window-start window saved-group-window-start t)))
        ((and (boundp 'gnus-group-buffer)
              (buffer-live-p gnus-group-buffer))
         (kill-buffer gnus-group-buffer))))))

(defun ekg-agent-tools-gnus--ensure-ready ()
  "Ensure the local Gnus startup data is available.
This reads the user's newsrc data when necessary, but avoids
connecting to servers just to list folders."
  (require 'gnus)
  (require 'gnus-start)
  (require 'gnus-int)
  (require 'gnus-util)
  (unless (and (boundp 'gnus-newsrc-alist)
               gnus-newsrc-alist
               (boundp 'gnus-active-hashtb)
               gnus-active-hashtb)
    (let ((gnus-check-new-newsgroups nil)
          (gnus-check-bogus-newsgroups nil))
      (ekg-agent-tools-gnus--without-interaction
        (gnus-setup-news nil nil t))))
  (unless (and (boundp 'gnus-newsrc-alist)
               (consp gnus-newsrc-alist))
    (error "Gnus newsrc data is not available")))

(defun ekg-agent-tools-gnus--infos ()
  "Return real Gnus group info entries."
  (ekg-agent-tools-gnus--ensure-ready)
  (seq-filter
   (lambda (info)
     (let ((group (gnus-info-group info)))
       (and (stringp group)
            (not (string-equal group "dummy.group")))))
   gnus-newsrc-alist))

(defun ekg-agent-tools-gnus--active (info)
  "Return the active range for Gnus INFO."
  (or (gnus-active (gnus-info-group info))
      (cdr (assq 'active (gnus-info-params info)))))

(defun ekg-agent-tools-gnus--active-count (active)
  "Return the number of articles in ACTIVE."
  (if (and (consp active)
           (numberp (car active))
           (numberp (cdr active))
           (not (zerop (cdr active))))
      (1+ (- (cdr active) (car active)))
    0))

(defun ekg-agent-tools-gnus--range-length (range)
  "Return the number of article numbers represented by Gnus RANGE."
  (cond
   ((null range) 0)
   ((numberp range) 1)
   ((and (consp range) (numberp (car range)) (numberp (cdr range)))
    (1+ (- (cdr range) (car range))))
   ((listp range)
    (let ((count 0))
      (dolist (entry range count)
        (setq count (+ count (ekg-agent-tools-gnus--range-length entry))))))
   (t 0)))

(defun ekg-agent-tools-gnus--unread-count (info active)
  "Return the unread count for Gnus INFO and ACTIVE range."
  (let ((entry (gnus-group-entry (gnus-info-group info))))
    (or (and entry (numberp (car entry)) (car entry))
        (and active (gnus-get-unread-articles-in-group info active))
        0)))

(defun ekg-agent-tools-gnus--status (info)
  "Return a display status for Gnus INFO."
  (if (<= (gnus-info-level info) gnus-level-subscribed)
      "subscribed"
    "unsubscribed"))

(defun ekg-agent-tools-gnus--method (info)
  "Return a short method string for Gnus INFO."
  (let ((method (or (gnus-info-method info)
                    (and (boundp 'gnus-select-method)
                         gnus-select-method))))
    (cond
     ((null method) "default")
     ((and (consp method) (symbolp (car method)))
      (format "%s:%s" (car method) (or (cadr method) "")))
     (t (format "%S" method)))))

(defun ekg-agent-tools-gnus--marks-summary (info)
  "Return a compact marks summary for Gnus INFO."
  (mapconcat
   (lambda (mark)
     (format "%s:%s" (car mark)
             (ekg-agent-tools-gnus--range-length (cdr mark))))
   (gnus-info-marks info)
   ","))

(defun ekg-agent-tools-gnus--folder-line (info)
  "Return a tab-separated summary line for Gnus INFO."
  (let* ((active (ekg-agent-tools-gnus--active info))
         (active-count (ekg-agent-tools-gnus--active-count active))
         (unread-count (ekg-agent-tools-gnus--unread-count info active)))
    (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
            (gnus-info-group info)
            (gnus-info-level info)
            (ekg-agent-tools-gnus--status info)
            (ekg-agent-tools-gnus--method info)
            active-count
            unread-count
            (ekg-agent-tools-gnus--range-length (gnus-info-read info))
            (ekg-agent-tools-gnus--marks-summary info))))

(defun ekg-agent-tools-gnus--folder-snapshot ()
  "Return a hash table of current Gnus folder counts."
  (let ((snapshot (make-hash-table :test #'equal)))
    (dolist (info (ekg-agent-tools-gnus--infos) snapshot)
      (let* ((group (gnus-info-group info))
             (active (ekg-agent-tools-gnus--active info))
             (active-count (ekg-agent-tools-gnus--active-count active))
             (unread-count (ekg-agent-tools-gnus--unread-count info active)))
        (puthash group
                 (list :active active-count
                       :unread unread-count
                       :low (and (consp active) (car active))
                       :high (and (consp active) (cdr active)))
                 snapshot)))))

(defun ekg-agent-tools-gnus--snapshot-total (snapshot property)
  "Return total PROPERTY across SNAPSHOT."
  (let ((total 0))
    (maphash
     (lambda (_group stats)
       (setq total (+ total (or (plist-get stats property) 0))))
     snapshot)
    total))

(defun ekg-agent-tools-gnus--snapshot-changes (before after)
  "Return changed folder rows between BEFORE and AFTER snapshots."
  (let (rows)
    (maphash
     (lambda (group after-stats)
       (let ((before-stats (gethash group before)))
         (when (or (null before-stats)
                   (/= (or (plist-get before-stats :active) 0)
                       (or (plist-get after-stats :active) 0))
                   (/= (or (plist-get before-stats :unread) 0)
                       (or (plist-get after-stats :unread) 0))
                   (not (equal (plist-get before-stats :high)
                               (plist-get after-stats :high))))
           (push (list group before-stats after-stats) rows))))
     after)
    (sort rows
          (lambda (a b)
            (let* ((a-before (nth 1 a))
                   (a-after (nth 2 a))
                   (b-before (nth 1 b))
                   (b-after (nth 2 b))
                   (a-delta (abs (- (or (plist-get a-after :unread) 0)
                                    (or (plist-get a-before :unread) 0))))
                   (b-delta (abs (- (or (plist-get b-after :unread) 0)
                                    (or (plist-get b-before :unread) 0)))))
              (if (= a-delta b-delta)
                  (string< (car a) (car b))
                (> a-delta b-delta)))))))

(defun ekg-agent-tools-gnus--change-line (row)
  "Return a tab-separated line describing changed Gnus ROW."
  (let ((group (nth 0 row))
        (before (nth 1 row))
        (after (nth 2 row)))
    (format "%s\t%s\t%s\t%s\t%s\t%s\t%s"
            group
            (or (plist-get before :active) 0)
            (or (plist-get after :active) 0)
            (or (plist-get before :unread) 0)
            (or (plist-get after :unread) 0)
            (or (plist-get before :high) "")
            (or (plist-get after :high) ""))))

(defun ekg-agent-tools-gnus--fetch-arg (level hard)
  "Return the prefix-style argument for a Gnus fetch using LEVEL and HARD."
  (cond
   ((ekg-agent-tools-gnus--bool hard) t)
   ((numberp level) (max 1 (min 9 level)))
   (t nil)))

;;;###autoload
(cl-defun ekg-agent-tools-gnus-fetch-latest (&key level hard max-folders)
  "Fetch newly arrived Gnus articles and return changed folder counts.
LEVEL is the Gnus group level to scan.  If HARD is non-nil, force
Gnus to re-read active files from servers.  MAX-FOLDERS limits the
changed folder rows returned."
  (ekg-agent-tools-gnus--preserve-session
    (ekg-agent-tools-gnus--ensure-ready)
    (require 'gnus-group)
    (let* ((max-folders (ekg-agent-tools-gnus--clip-number
                         max-folders
                         ekg-agent-tools-gnus-default-max-fetch-changes
                         100))
           (arg (ekg-agent-tools-gnus--fetch-arg level hard))
           (mode (cond
                  ((ekg-agent-tools-gnus--bool hard) "hard")
                  ((numberp level) (format "level %s" arg))
                  (t "default")))
           (before (ekg-agent-tools-gnus--folder-snapshot))
           (started (current-time))
           (gnus-verbose 0)
           (gnus-verbose-backends 0))
      (ekg-agent-tools-gnus--without-interaction
        (save-current-buffer
          (gnus-group-setup-buffer)
          (with-current-buffer gnus-group-buffer
            (gnus-group-get-new-news arg))))
      (let* ((after (ekg-agent-tools-gnus--folder-snapshot))
             (elapsed (float-time (time-subtract (current-time) started)))
             (changes (ekg-agent-tools-gnus--snapshot-changes before after))
             (summary
              (list
               (format "Fetched Gnus latest data at %s."
                       (format-time-string "%F %T"))
               (format "mode: %s" mode)
               (format "elapsed_seconds: %.2f" elapsed)
               (format "folders_changed: %s" (length changes))
               (format "total_active_before: %s"
                       (ekg-agent-tools-gnus--snapshot-total before :active))
               (format "total_active_after: %s"
                       (ekg-agent-tools-gnus--snapshot-total after :active))
               (format "total_unread_before: %s"
                       (ekg-agent-tools-gnus--snapshot-total before :unread))
               (format "total_unread_after: %s"
                       (ekg-agent-tools-gnus--snapshot-total after :unread)))))
        (if changes
            (string-join
             (append summary
                     (list
                      (concat "group\tactive_before\tactive_after"
                              "\tunread_before\tunread_after"
                              "\tlatest_before\tlatest_after"))
                     (mapcar #'ekg-agent-tools-gnus--change-line
                             (seq-take changes max-folders)))
             "\n")
          (string-join
           (append summary (list "No Gnus folder counts changed."))
           "\n"))))))

;;;###autoload
(cl-defun ekg-agent-tools-gnus-list-folders (&key max-folders include-empty
                                                  only-subscribed)
  "Return a tab-separated summary of Gnus folders.
MAX-FOLDERS limits the result size.  If INCLUDE-EMPTY is nil,
folders with no active or unread articles are omitted.  If
ONLY-SUBSCRIBED is non-nil, folders above `gnus-level-subscribed'
are omitted."
  (ekg-agent-tools-gnus--preserve-session
    (ekg-agent-tools-gnus--ensure-ready)
    (let* ((max-folders (ekg-agent-tools-gnus--clip-number
                         max-folders
                         ekg-agent-tools-gnus-default-max-folders 200))
           (include-empty (ekg-agent-tools-gnus--bool include-empty))
           (only-subscribed (ekg-agent-tools-gnus--bool only-subscribed))
           (infos (seq-filter
                   (lambda (info)
                     (let* ((active (ekg-agent-tools-gnus--active info))
                            (active-count
                             (ekg-agent-tools-gnus--active-count active))
                            (unread-count
                             (ekg-agent-tools-gnus--unread-count info active)))
                       (and (or include-empty
                                (> active-count 0)
                                (> unread-count 0))
                            (or (not only-subscribed)
                                (<= (gnus-info-level info)
                                    gnus-level-subscribed)))))
                   (ekg-agent-tools-gnus--infos))))
      (if infos
          (string-join
           (cons "group\tlevel\tstatus\tmethod\tactive\tunread\tread\tmarks"
                 (mapcar #'ekg-agent-tools-gnus--folder-line
                         (seq-take infos max-folders)))
           "\n")
        "No Gnus folders found."))))

(defun ekg-agent-tools-gnus--clean-header-value (value)
  "Return VALUE decoded and safe for tab-separated output."
  (require 'rfc2047)
  (let ((value (or value "")))
    (setq value (or (ignore-errors (rfc2047-decode-string value)) value))
    (string-trim (replace-regexp-in-string "[\t\n\r]+" " " value))))

(defun ekg-agent-tools-gnus--result-id (group article)
  "Return a stable tool result ID for GROUP and ARTICLE."
  (base64-encode-string (format "%s\t%s" group article) t))

(defun ekg-agent-tools-gnus--decode-result-id (result-id)
  "Decode RESULT-ID into a list of group and article."
  (when (and (stringp result-id) (not (string-empty-p result-id)))
    (let ((parts (split-string (base64-decode-string result-id) "\t")))
      (when (= (length parts) 2)
        (list (car parts) (string-to-number (cadr parts)))))))

(defun ekg-agent-tools-gnus--parse-date-time (date)
  "Return DATE as an Emacs time value, or nil if parsing fails."
  (and (stringp date)
       (ignore-errors (date-to-time date))))

(defun ekg-agent-tools-gnus--article-metadata (group article)
  "Return metadata for ARTICLE in GROUP, or nil if no header is found."
  (when (ekg-agent-tools-gnus--without-interaction
          (ignore-errors (gnus-request-head article group)))
    (with-current-buffer nntp-server-buffer
      (let ((subject (ekg-agent-tools-gnus--clean-header-value
                      (mail-fetch-field "subject")))
            (from (ekg-agent-tools-gnus--clean-header-value
                   (mail-fetch-field "from")))
            (date (ekg-agent-tools-gnus--clean-header-value
                   (mail-fetch-field "date")))
            (message-id (ekg-agent-tools-gnus--clean-header-value
                         (mail-fetch-field "message-id"))))
        (unless (string-empty-p subject)
          (list :result-id (ekg-agent-tools-gnus--result-id group article)
                :group group
                :article article
                :date date
                :date-time (ekg-agent-tools-gnus--parse-date-time date)
                :from from
                :subject subject
                :message-id message-id))))))

(defun ekg-agent-tools-gnus--header-line (group article)
  "Return a tab-separated header line for ARTICLE in GROUP, or nil."
  (when-let* ((metadata (ekg-agent-tools-gnus--article-metadata group article)))
    (format "%s\t%s\t%s\t%s\t%s\t%s"
            (plist-get metadata :group)
            (plist-get metadata :article)
            (plist-get metadata :date)
            (plist-get metadata :from)
            (plist-get metadata :subject)
            (plist-get metadata :message-id))))

;;;###autoload
(cl-defun ekg-agent-tools-gnus-recent-headers (&key folder num-headers)
  "Return recent message headers for Gnus FOLDER.
NUM-HEADERS is capped by `ekg-agent-tools-gnus-default-max-headers' and
`ekg-agent-tools-gnus-header-max-attempts'.  This requests headers only,
not message bodies."
  (ekg-agent-tools-gnus--preserve-session
    (ekg-agent-tools-gnus--ensure-ready)
    (unless (and (stringp folder) (gnus-get-info folder))
      (error "No such Gnus folder: %s" folder))
    (let* ((info (gnus-get-info folder))
           (num-headers (ekg-agent-tools-gnus--clip-number
                         num-headers
                         ekg-agent-tools-gnus-default-max-headers
                         ekg-agent-tools-gnus-max-headers))
           (active (ekg-agent-tools-gnus--active info))
           (article (and active (cdr active)))
           (low (and active (car active)))
           (attempts 0)
           (gnus-verbose 0)
           (gnus-verbose-backends 0)
           lines)
      (while (and article low
                  (>= article low)
                  (< (length lines) num-headers)
                  (< attempts ekg-agent-tools-gnus-header-max-attempts))
        (setq attempts (1+ attempts))
        (when-let* ((line (ekg-agent-tools-gnus--header-line folder article)))
          (push line lines))
        (setq article (1- article)))
      (if lines
          (string-join
           (cons "group\tarticle\tdate\tfrom\tsubject\tmessage-id"
                 (nreverse lines))
           "\n")
        (format "No recent headers found for Gnus folder: %s" folder)))))

(defun ekg-agent-tools-gnus--split-folders (folders)
  "Return FOLDERS string as a list of folder names."
  (when (stringp folders)
    (seq-filter
     (lambda (folder) (not (string-empty-p folder)))
     (mapcar #'string-trim (split-string folders "[,\n]" t)))))

(defun ekg-agent-tools-gnus--quote-query-value (value)
  "Return VALUE formatted for a Gnus parsed search query."
  (let ((value (string-trim (format "%s" value))))
    (if (string-match-p "[[:space:]()\"]" value)
        (format "%S" value)
      value)))

(defun ekg-agent-tools-gnus--query-term (key value)
  "Return a Gnus search query term for KEY and VALUE."
  (when (and (stringp value)
             (not (string-empty-p (string-trim value))))
    (format "%s:%s" key (ekg-agent-tools-gnus--quote-query-value value))))

(defun ekg-agent-tools-gnus--plain-query-p (query)
  "Return non-nil when QUERY has no explicit Gnus search keys."
  (and (stringp query)
       (not (string-empty-p (string-trim query)))
       (not (string-match-p "\\_<[-[:alnum:]]+:" query))))

(defun ekg-agent-tools-gnus--plain-query-terms (query)
  "Return plain QUERY words as body search terms."
  (mapcar
   (lambda (term) (ekg-agent-tools-gnus--query-term "body" term))
   (split-string query "[[:space:]]+" t)))

(defun ekg-agent-tools-gnus--build-query (query from subject body)
  "Return a Gnus search query from QUERY, FROM, SUBJECT, and BODY."
  (let ((terms (append
                (unless (string-empty-p (string-trim (or query "")))
                  (if (ekg-agent-tools-gnus--plain-query-p query)
                      (ekg-agent-tools-gnus--plain-query-terms query)
                    (list (string-trim query))))
                (list (ekg-agent-tools-gnus--query-term "from" from)
                      (ekg-agent-tools-gnus--query-term "subject" subject)
                      (ekg-agent-tools-gnus--query-term "body" body)))))
    (string-join (delq nil terms) " ")))

(defun ekg-agent-tools-gnus--folder-server (folder)
  "Return the Gnus server name for FOLDER."
  (if (gnus-group-native-p folder)
      (gnus-group-server folder)
    (gnus-method-to-server (gnus-find-method-for-group folder))))

(defun ekg-agent-tools-gnus--search-group-spec (folders)
  "Return a `gnus-search-run-query' group spec for FOLDERS.
When FOLDERS is nil, search all known Gnus folders grouped by
server."
  (let ((folders (or folders
                     (mapcar (lambda (info) (gnus-info-group info))
                             (ekg-agent-tools-gnus--infos)))))
    (mapcar
     (lambda (group)
       (cons (car group) (cdr group)))
     (seq-group-by #'ekg-agent-tools-gnus--folder-server folders))))

(defun ekg-agent-tools-gnus--searchable-group-spec (group-spec explicit-p)
  "Return searchable groups from GROUP-SPEC.
If EXPLICIT-P is non-nil, error when any requested server has no
configured search engine."
  (let (searchable skipped)
    (dolist (group group-spec)
      (condition-case err
          (progn
            (gnus-search-server-to-engine (car group))
            (push group searchable))
        (gnus-search-config-error
         (if explicit-p
             (signal (car err) (cdr err))
           (push (car group) skipped)))))
    (unless searchable
      (if skipped
          (error "No searchable Gnus servers found; skipped: %s"
                 (string-join (nreverse skipped) ", "))
        (error "No searchable Gnus servers found")))
    (nreverse searchable)))

(defun ekg-agent-tools-gnus--search-result-metadata (result)
  "Return metadata for Gnus search RESULT."
  (let* ((group (aref result 0))
         (article (aref result 1))
         (score (aref result 2))
         (metadata (ekg-agent-tools-gnus--article-metadata group article)))
    (if metadata
        (plist-put metadata :score score)
      (list :result-id (ekg-agent-tools-gnus--result-id group article)
            :group group
            :article article
            :date ""
            :date-time nil
            :from ""
            :subject ""
            :message-id ""
            :score score))))

(defun ekg-agent-tools-gnus--latest-first-p (a b)
  "Return non-nil when metadata A is newer than metadata B."
  (let ((a-time (plist-get a :date-time))
        (b-time (plist-get b :date-time)))
    (cond
     ((and a-time b-time) (time-less-p b-time a-time))
     (a-time t)
     (b-time nil)
     (t (> (or (plist-get a :article) 0)
           (or (plist-get b :article) 0))))))

(defun ekg-agent-tools-gnus--dedupe-search-results (metadata)
  "Return METADATA with duplicate Message-IDs removed."
  (let ((seen (make-hash-table :test #'equal))
        deduped)
    (dolist (item metadata (nreverse deduped))
      (let ((key (or (and (not (string-empty-p
                                (or (plist-get item :message-id) "")))
                          (plist-get item :message-id))
                     (format "%s\t%s"
                             (plist-get item :group)
                             (plist-get item :article)))))
        (unless (gethash key seen)
          (puthash key t seen)
          (push item deduped))))))

(defun ekg-agent-tools-gnus--search-result-line (metadata)
  "Return a tab-separated search result line for METADATA."
  (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
          (ekg-agent-tools-gnus--clean-field (plist-get metadata :result-id))
          (ekg-agent-tools-gnus--clean-field (plist-get metadata :group))
          (plist-get metadata :article)
          (ekg-agent-tools-gnus--clean-field (plist-get metadata :date))
          (ekg-agent-tools-gnus--clean-field (plist-get metadata :from))
          (ekg-agent-tools-gnus--clean-field (plist-get metadata :subject))
          (ekg-agent-tools-gnus--clean-field (plist-get metadata :message-id))
          (or (plist-get metadata :score) "")))

;;;###autoload
(cl-defun ekg-agent-tools-gnus-search-messages (&key query folders from subject body
                                               num-results raw)
  "Search Gnus messages using QUERY and return matching headers.
QUERY is interpreted by Gnus' search facilities.  By default, the
Gnus parsed search syntax is enabled for this request, so examples
include from:\"Stefan Monnier\", body:uuid, subject:\"meeting\",
or since:1w.  FROM, SUBJECT, and BODY add structured parsed
query terms, which is preferred for agent use.  If QUERY contains
no explicit search keys, its words are searched as body terms.
If RAW is non-nil, pass QUERY directly to the configured search
engine and ignore FROM, SUBJECT, and BODY.  FOLDERS is an
optional comma-separated list of exact Gnus folder names.
NUM-RESULTS limits returned rows."
  (ekg-agent-tools-gnus--preserve-session
    (ekg-agent-tools-gnus--ensure-ready)
    (require 'gnus-search)
    (let ((query (if (ekg-agent-tools-gnus--bool raw)
                     (or query "")
                   (ekg-agent-tools-gnus--build-query query from subject body))))
      (unless (and (stringp query) (not (string-empty-p (string-trim query))))
        (error "Gnus search query must be a non-empty string"))
      (let* ((folders (ekg-agent-tools-gnus--split-folders folders))
             (missing (seq-filter
                       (lambda (folder) (not (gnus-get-info folder)))
                       folders))
             (folders (seq-remove
                       (lambda (folder) (member folder missing))
                       folders))
             (num-results (ekg-agent-tools-gnus--clip-number
                           num-results
                           ekg-agent-tools-gnus-default-max-search-results
                           ekg-agent-tools-gnus-max-search-results))
             (candidate-results
              (max num-results ekg-agent-tools-gnus-default-search-candidates))
             (group-spec (ekg-agent-tools-gnus--search-group-spec folders))
             (group-spec (ekg-agent-tools-gnus--searchable-group-spec
                          group-spec (not (null folders))))
             (query-spec (append `((query . ,query)
                                   (limit . ,candidate-results))
                                 (when (ekg-agent-tools-gnus--bool raw)
                                   '((raw . t)))))
             (gnus-search-use-parsed-queries
              (and (not (ekg-agent-tools-gnus--bool raw)) t))
             (gnus-verbose 0)
             (gnus-verbose-backends 0)
             results)
        (when (and missing
                   (or (not ekg-agent-tools-gnus-ignore-missing-folders)
                       (null folders)))
          (error "No such Gnus folder: %s" (string-join missing ", ")))
        (setq results
              (ekg-agent-tools-gnus--without-interaction
                (gnus-search-run-query
                 `((search-query-spec . ,query-spec)
                   (search-group-spec . ,group-spec)))))
        (if (> (length results) 0)
            (let ((metadata
                   (ekg-agent-tools-gnus--dedupe-search-results
                    (sort (mapcar #'ekg-agent-tools-gnus--search-result-metadata
                                  (append results nil))
                          #'ekg-agent-tools-gnus--latest-first-p))))
              (string-join
               (cons (concat "result_id\tgroup\tarticle\tdate\tfrom\tsubject"
                             "\tmessage-id\tscore")
                     (mapcar #'ekg-agent-tools-gnus--search-result-line
                             (seq-take metadata num-results)))
               "\n"))
          (format "No Gnus messages found for query: %s" query))))))

;;;###autoload
(cl-defun ekg-agent-tools-gnus-read-message (&key result-id group article
                                            max-content-chars)
  "Read a Gnus message by RESULT-ID or GROUP and ARTICLE.
RESULT-ID is emitted by `ekg-agent-tools-gnus-search-messages'.
MAX-CONTENT-CHARS limits the returned raw message content."
  (ekg-agent-tools-gnus--preserve-session
    (ekg-agent-tools-gnus--ensure-ready)
    (let* ((decoded (ekg-agent-tools-gnus--decode-result-id result-id))
           (group (or group (car decoded)))
           (article (or article (cadr decoded)))
           (max-content-chars (ekg-agent-tools-gnus--clip-number
                               max-content-chars 20000 100000)))
      (unless (and (stringp group) (not (string-empty-p group)))
        (error "Gnus message group is required"))
      (unless (and (numberp article) (> article 0))
        (error "Gnus message article number is required"))
      (unless (gnus-get-info group)
        (error "No such Gnus folder: %s" group))
      (with-temp-buffer
        (unless (ekg-agent-tools-gnus--without-interaction
                  (gnus-request-article article group (current-buffer)))
          (error "Unable to fetch Gnus article %s from %s" article group))
        (ekg-agent-tools-gnus--clip-string
         (buffer-substring-no-properties (point-min) (point-max))
         max-content-chars)))))

(defconst ekg-agent-tools-gnus-tool-fetch-latest
  (make-llm-tool
   :function (lambda (level hard max-folders)
               (condition-case err
                   (ekg-agent-tools-gnus-fetch-latest
                    :level level
                    :hard hard
                    :max-folders max-folders)
                 (error (format "Error: %s" (error-message-string err)))))
   :name "gnus_fetch_latest"
   :description
   "Fetch latest Gnus data from configured servers and report changed folders."
   :args
   '((:name "level"
            :type integer
            :description
            "Optional Gnus group level to scan, from 1 to 9. Omit for default.")
     (:name "hard"
            :type boolean
            :description "Whether to force hard re-reading of server active files.")
     (:name "max_folders"
            :type integer
            :description "Maximum changed folders to return, capped at 100."))))

(defconst ekg-agent-tools-gnus-tool-list-folders
  (make-llm-tool
   :function (lambda (max-folders include-empty only-subscribed)
               (condition-case err
                   (ekg-agent-tools-gnus-list-folders
                    :max-folders max-folders
                    :include-empty include-empty
                    :only-subscribed only-subscribed)
                 (error (format "Error: %s" (error-message-string err)))))
   :name "gnus_list_folders"
   :description
   "List Gnus folders with local counts. Does not fetch message bodies."
   :args
   '((:name "max_folders"
            :type integer
            :description "Maximum number of folders to return, capped at 200.")
     (:name "include_empty"
            :type boolean
            :description "Whether to include folders with no active articles.")
     (:name "only_subscribed"
            :type boolean
            :description "Whether to include only subscribed Gnus folders."))))

(defconst ekg-agent-tools-gnus-tool-recent-headers
  (make-llm-tool
   :function (lambda (folder num-headers)
               (condition-case err
                   (ekg-agent-tools-gnus-recent-headers
                    :folder folder
                    :num-headers num-headers)
                 (error (format "Error: %s" (error-message-string err)))))
   :name "gnus_recent_headers"
   :description
   "Fetch recent Gnus message headers. Does not fetch message bodies."
   :args
   '((:name "folder"
            :type string
            :description "Exact Gnus folder name, such as INBOX or emacs-devel.")
     (:name "num_headers"
            :type integer
            :description "Maximum number of recent headers, capped at 2000."))))

(defconst ekg-agent-tools-gnus-tool-search-messages
  (make-llm-tool
   :function (lambda (query folders from subject body num-results raw)
               (condition-case err
                   (ekg-agent-tools-gnus-search-messages
                    :query query
                    :folders folders
                    :from from
                    :subject subject
                    :body body
                    :num-results num-results
                    :raw raw)
                 (error (format "Error: %s" (error-message-string err)))))
   :name "gnus_search_messages"
   :description
   (concat
    "Search Gnus messages and return latest matching headers with result_id. "
    "For sender/content searches, prefer from/body args; e.g. "
    "from=Stefan Monnier and body=uuid.")
   :args
   '((:name "query"
            :type string
            :description
            "Optional Gnus parsed query. Plain unkeyed words are treated as body terms.")
     (:name "folders"
            :type string
            :description
            "Optional comma-separated exact Gnus folder names. Missing folders are skipped if others are valid.")
     (:name "from"
            :type string
            :description "Optional sender match, such as Stefan Monnier.")
     (:name "subject"
            :type string
            :description "Optional subject match, such as uuid.el.")
     (:name "body"
            :type string
            :description "Optional message body/content match, such as uuid.")
     (:name "num_results"
            :type integer
            :description "Maximum latest results to return, default 10 and capped at 200.")
     (:name "raw"
            :type boolean
            :description
            "Whether to pass query directly to the search engine, ignoring from/subject/body."))))

(defconst ekg-agent-tools-gnus-tool-read-message
  (make-llm-tool
   :function (lambda (result-id group article max-content-chars)
               (condition-case err
                   (ekg-agent-tools-gnus-read-message
                    :result-id result-id
                    :group group
                    :article article
                    :max-content-chars max-content-chars)
                 (error (format "Error: %s" (error-message-string err)))))
   :name "gnus_read_message"
   :description
   "Read one Gnus message from a result_id returned by gnus_search_messages."
   :args
   '((:name "result_id"
            :type string
            :description "Opaque result_id from gnus_search_messages.")
     (:name "group"
            :type string
            :description "Optional exact Gnus folder name when result_id is absent.")
     (:name "article"
            :type integer
            :description "Optional article number when result_id is absent.")
     (:name "max_content_chars"
            :type integer
            :description "Maximum raw message characters to return, capped at 100000."))))

(defconst ekg-agent-tools-gnus-tools
  (list ekg-agent-tools-gnus-tool-fetch-latest
        ekg-agent-tools-gnus-tool-list-folders
        ekg-agent-tools-gnus-tool-recent-headers
        ekg-agent-tools-gnus-tool-search-messages
        ekg-agent-tools-gnus-tool-read-message)
  "Optional Gnus tools suitable for `ekg-agent-extra-tools'.")

(provide 'ekg-agent-tools-gnus)
;;; ekg-agent-tools-gnus.el ends here
