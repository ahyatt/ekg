;;; ekg-tools.el --- Agent tools for connected local services -*- lexical-binding: t -*-

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
;; This package provides optional tools for `ekg-agent'.  The tools are
;; intentionally separate from `ekg-agent.el' so users can opt into local
;; integrations that may expose private connected data.

;;; Code:

(require 'cl-lib)
(require 'ekg-agent)
(require 'llm)
(require 'seq)
(require 'subr-x)

(eval-when-compile
  (require 'gnus)
  (require 'gnus-group)
  (require 'gnus-start))

(defvar gnus-active-hashtb)
(defvar gnus-check-bogus-newsgroups)
(defvar gnus-check-new-newsgroups)
(defvar gnus-group-buffer)
(defvar gnus-level-subscribed)
(defvar gnus-newsrc-alist)
(defvar gnus-select-method)
(defvar gnus-verbose)
(defvar gnus-verbose-backends)
(defvar nntp-server-buffer)

(defvar elfeed-db-entries)
(defvar elfeed-db-feeds)
(defvar elfeed-feeds)
(defvar elfeed-search-filter)

(declare-function gnus-get-unread-articles-in-group "gnus-start"
                  (info active &optional update))
(declare-function gnus-group-get-new-news "gnus-group" (&optional arg
                                                                  one-level))
(declare-function gnus-group-setup-buffer "gnus-group" ())
(declare-function gnus-request-head "gnus-int" (article group))
(declare-function gnus-setup-news "gnus-start" (&optional rawfile level
                                                          dont-connect))
(declare-function elfeed-db-ensure "elfeed-db" ())
(declare-function elfeed-db-get-feed "elfeed-db" (id))
(declare-function elfeed-db-get-entry "elfeed-db" (id))
(declare-function elfeed-db-last-update "elfeed-db" ())
(declare-function elfeed-deref "elfeed-db" (ref))
(declare-function elfeed-entry-content "elfeed-db" (entry))
(declare-function elfeed-entry-content-type "elfeed-db" (entry))
(declare-function elfeed-entry-date "elfeed-db" (entry))
(declare-function elfeed-entry-feed "elfeed-db" (entry))
(declare-function elfeed-entry-feed-id "elfeed-db" (entry))
(declare-function elfeed-entry-id "elfeed-db" (entry))
(declare-function elfeed-entry-link "elfeed-db" (entry))
(declare-function elfeed-entry-tags "elfeed-db" (entry))
(declare-function elfeed-entry-title "elfeed-db" (entry))
(declare-function elfeed-feed-id "elfeed-db" (feed))
(declare-function elfeed-feed-title "elfeed-db" (feed))
(declare-function elfeed-feed-url "elfeed-db" (feed))
(declare-function elfeed-search-parse-filter "elfeed-search" (filter))

(defgroup ekg-tools nil
  "Optional connected-data tools for ekg."
  :group 'ekg)

(defcustom ekg-tools-gnus-default-max-folders 50
  "Default maximum number of Gnus folders returned by the folder tool."
  :type 'integer
  :group 'ekg-tools)

(defcustom ekg-tools-gnus-default-max-headers 10
  "Default maximum number of Gnus headers returned by the headers tool."
  :type 'integer
  :group 'ekg-tools)

(defcustom ekg-tools-gnus-default-max-fetch-changes 20
  "Default maximum number of changed folders returned by the fetch tool."
  :type 'integer
  :group 'ekg-tools)

(defcustom ekg-tools-gnus-header-max-attempts 1000
  "Maximum article numbers to inspect when looking for recent Gnus headers.
IMAP folders can have gaps in their UID ranges, so finding N recent
headers may require trying more than N article numbers."
  :type 'integer
  :group 'ekg-tools)

(defcustom ekg-tools-elfeed-default-max-feeds 50
  "Default maximum number of Elfeed feeds returned by the feed tool."
  :type 'integer
  :group 'ekg-tools)

(defcustom ekg-tools-elfeed-default-max-entries 20
  "Default maximum number of Elfeed entries returned by the entry tool."
  :type 'integer
  :group 'ekg-tools)

(defcustom ekg-tools-elfeed-default-content-chars 4000
  "Default maximum number of characters returned for Elfeed entry content."
  :type 'integer
  :group 'ekg-tools)

(defun ekg-tools--bool (value)
  "Return non-nil when VALUE represents true."
  (and value (not (memq value '(false :false :json-false)))))

(defun ekg-tools--clip-number (value default maximum)
  "Return VALUE as a positive number, or DEFAULT, capped at MAXIMUM."
  (let ((value (if (numberp value) value default)))
    (min maximum (max 1 value))))

(defun ekg-tools--clean-field (value)
  "Return VALUE as one safe tab-separated field."
  (require 'xml)
  (let ((value (xml-substitute-special (format "%s" (or value "")))))
    (string-trim
     (replace-regexp-in-string
      "[\t\n\r]+" " "
      (replace-regexp-in-string "[\x3fff80-\x3fffff]" "" value)))))

(defun ekg-tools--clip-string (text max-chars)
  "Return TEXT clipped to MAX-CHARS with a truncation notice."
  (let ((text (or text "")))
    (if (> (length text) max-chars)
        (concat (substring text 0 max-chars)
                "\n\n[Content truncated at "
                (number-to-string max-chars) " characters]")
      text)))

(defun ekg-tools--html-to-text (html)
  "Render HTML as readable text."
  (require 'shr)
  (with-temp-buffer
    (insert html)
    (condition-case nil
        (let ((dom (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (let ((shr-width 80)
                (shr-use-fonts nil)
                (shr-bullet "- ")
                (shr-current-font 'default))
            (shr-insert-document dom)))
      (error
       (goto-char (point-min))
       (while (re-search-forward "<[^>]+>" nil t)
         (replace-match " "))))
    (string-trim
     (replace-regexp-in-string
      "[\x3fff80-\x3fffff]" ""
      (buffer-substring-no-properties (point-min) (point-max))))))

(defun ekg-tools-gnus--ensure-ready ()
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
      (gnus-setup-news nil nil t)))
  (unless (and (boundp 'gnus-newsrc-alist)
               (consp gnus-newsrc-alist))
    (error "Gnus newsrc data is not available")))

(defun ekg-tools-gnus--infos ()
  "Return real Gnus group info entries."
  (ekg-tools-gnus--ensure-ready)
  (seq-filter
   (lambda (info)
     (let ((group (gnus-info-group info)))
       (and (stringp group)
            (not (string-equal group "dummy.group")))))
   gnus-newsrc-alist))

(defun ekg-tools-gnus--active (info)
  "Return the active range for Gnus INFO."
  (or (gnus-active (gnus-info-group info))
      (cdr (assq 'active (gnus-info-params info)))))

(defun ekg-tools-gnus--active-count (active)
  "Return the number of articles in ACTIVE."
  (if (and (consp active)
           (numberp (car active))
           (numberp (cdr active))
           (not (zerop (cdr active))))
      (1+ (- (cdr active) (car active)))
    0))

(defun ekg-tools-gnus--range-length (range)
  "Return the number of article numbers represented by Gnus RANGE."
  (cond
   ((null range) 0)
   ((numberp range) 1)
   ((and (consp range) (numberp (car range)) (numberp (cdr range)))
    (1+ (- (cdr range) (car range))))
   ((listp range)
    (let ((count 0))
      (dolist (entry range count)
        (setq count (+ count (ekg-tools-gnus--range-length entry))))))
   (t 0)))

(defun ekg-tools-gnus--unread-count (info active)
  "Return the unread count for Gnus INFO and ACTIVE range."
  (let ((entry (gnus-group-entry (gnus-info-group info))))
    (or (and entry (numberp (car entry)) (car entry))
        (and active (gnus-get-unread-articles-in-group info active))
        0)))

(defun ekg-tools-gnus--status (info)
  "Return a display status for Gnus INFO."
  (if (<= (gnus-info-level info) gnus-level-subscribed)
      "subscribed"
    "unsubscribed"))

(defun ekg-tools-gnus--method (info)
  "Return a short method string for Gnus INFO."
  (let ((method (or (gnus-info-method info)
                    (and (boundp 'gnus-select-method)
                         gnus-select-method))))
    (cond
     ((null method) "default")
     ((and (consp method) (symbolp (car method)))
      (format "%s:%s" (car method) (or (cadr method) "")))
     (t (format "%S" method)))))

(defun ekg-tools-gnus--marks-summary (info)
  "Return a compact marks summary for Gnus INFO."
  (mapconcat
   (lambda (mark)
     (format "%s:%s" (car mark)
             (ekg-tools-gnus--range-length (cdr mark))))
   (gnus-info-marks info)
   ","))

(defun ekg-tools-gnus--folder-line (info)
  "Return a tab-separated summary line for Gnus INFO."
  (let* ((active (ekg-tools-gnus--active info))
         (active-count (ekg-tools-gnus--active-count active))
         (unread-count (ekg-tools-gnus--unread-count info active)))
    (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
            (gnus-info-group info)
            (gnus-info-level info)
            (ekg-tools-gnus--status info)
            (ekg-tools-gnus--method info)
            active-count
            unread-count
            (ekg-tools-gnus--range-length (gnus-info-read info))
            (ekg-tools-gnus--marks-summary info))))

(defun ekg-tools-gnus--folder-snapshot ()
  "Return a hash table of current Gnus folder counts."
  (let ((snapshot (make-hash-table :test #'equal)))
    (dolist (info (ekg-tools-gnus--infos) snapshot)
      (let* ((group (gnus-info-group info))
             (active (ekg-tools-gnus--active info))
             (active-count (ekg-tools-gnus--active-count active))
             (unread-count (ekg-tools-gnus--unread-count info active)))
        (puthash group
                 (list :active active-count
                       :unread unread-count
                       :low (and (consp active) (car active))
                       :high (and (consp active) (cdr active)))
                 snapshot)))))

(defun ekg-tools-gnus--snapshot-total (snapshot property)
  "Return total PROPERTY across SNAPSHOT."
  (let ((total 0))
    (maphash
     (lambda (_group stats)
       (setq total (+ total (or (plist-get stats property) 0))))
     snapshot)
    total))

(defun ekg-tools-gnus--snapshot-changes (before after)
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

(defun ekg-tools-gnus--change-line (row)
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

(defun ekg-tools-gnus--fetch-arg (level hard)
  "Return the prefix-style argument for a Gnus fetch using LEVEL and HARD."
  (cond
   ((ekg-tools--bool hard) t)
   ((numberp level) (max 1 (min 9 level)))
   (t nil)))

;;;###autoload
(cl-defun ekg-tools-gnus-fetch-latest (&key level hard max-folders)
  "Fetch newly arrived Gnus articles and return changed folder counts.
LEVEL is the Gnus group level to scan.  If HARD is non-nil, force
Gnus to re-read active files from servers.  MAX-FOLDERS limits the
changed folder rows returned."
  (ekg-tools-gnus--ensure-ready)
  (require 'gnus-group)
  (let* ((max-folders (ekg-tools--clip-number
                       max-folders
                       ekg-tools-gnus-default-max-fetch-changes
                       100))
         (arg (ekg-tools-gnus--fetch-arg level hard))
         (mode (cond
                ((ekg-tools--bool hard) "hard")
                ((numberp level) (format "level %s" arg))
                (t "default")))
         (before (ekg-tools-gnus--folder-snapshot))
         (started (current-time))
         (gnus-verbose 0)
         (gnus-verbose-backends 0))
    (save-current-buffer
      (gnus-group-setup-buffer)
      (with-current-buffer gnus-group-buffer
        (gnus-group-get-new-news arg)))
    (let* ((after (ekg-tools-gnus--folder-snapshot))
           (elapsed (float-time (time-subtract (current-time) started)))
           (changes (ekg-tools-gnus--snapshot-changes before after))
           (summary
            (list
             (format "Fetched Gnus latest data at %s."
                     (format-time-string "%F %T"))
             (format "mode: %s" mode)
             (format "elapsed_seconds: %.2f" elapsed)
             (format "folders_changed: %s" (length changes))
             (format "total_active_before: %s"
                     (ekg-tools-gnus--snapshot-total before :active))
             (format "total_active_after: %s"
                     (ekg-tools-gnus--snapshot-total after :active))
             (format "total_unread_before: %s"
                     (ekg-tools-gnus--snapshot-total before :unread))
             (format "total_unread_after: %s"
                     (ekg-tools-gnus--snapshot-total after :unread)))))
      (if changes
          (string-join
           (append summary
                   (list
                    (concat "group\tactive_before\tactive_after"
                            "\tunread_before\tunread_after"
                            "\tlatest_before\tlatest_after"))
                   (mapcar #'ekg-tools-gnus--change-line
                           (seq-take changes max-folders)))
           "\n")
        (string-join
         (append summary (list "No Gnus folder counts changed."))
         "\n")))))

;;;###autoload
(cl-defun ekg-tools-gnus-list-folders (&key max-folders include-empty
                                            only-subscribed)
  "Return a tab-separated summary of Gnus folders.
MAX-FOLDERS limits the result size.  If INCLUDE-EMPTY is nil,
folders with no active or unread articles are omitted.  If
ONLY-SUBSCRIBED is non-nil, folders above `gnus-level-subscribed'
are omitted."
  (ekg-tools-gnus--ensure-ready)
  (let* ((max-folders (ekg-tools--clip-number
                       max-folders ekg-tools-gnus-default-max-folders 200))
         (include-empty (ekg-tools--bool include-empty))
         (only-subscribed (ekg-tools--bool only-subscribed))
         (infos (seq-filter
                 (lambda (info)
                   (let* ((active (ekg-tools-gnus--active info))
                          (active-count (ekg-tools-gnus--active-count active))
                          (unread-count
                           (ekg-tools-gnus--unread-count info active)))
                     (and (or include-empty
                              (> active-count 0)
                              (> unread-count 0))
                          (or (not only-subscribed)
                              (<= (gnus-info-level info)
                                  gnus-level-subscribed)))))
                 (ekg-tools-gnus--infos))))
    (if infos
        (string-join
         (cons "group\tlevel\tstatus\tmethod\tactive\tunread\tread\tmarks"
               (mapcar #'ekg-tools-gnus--folder-line
                       (seq-take infos max-folders)))
         "\n")
      "No Gnus folders found.")))

(defun ekg-tools-gnus--clean-header-value (value)
  "Return VALUE decoded and safe for tab-separated output."
  (require 'rfc2047)
  (let ((value (or value "")))
    (setq value (or (ignore-errors (rfc2047-decode-string value)) value))
    (string-trim (replace-regexp-in-string "[\t\n\r]+" " " value))))

(defun ekg-tools-gnus--header-line (group article)
  "Return a tab-separated header line for ARTICLE in GROUP, or nil."
  (require 'mail-utils)
  (when (ignore-errors (gnus-request-head article group))
    (with-current-buffer nntp-server-buffer
      (let ((subject (ekg-tools-gnus--clean-header-value
                      (mail-fetch-field "subject")))
            (from (ekg-tools-gnus--clean-header-value
                   (mail-fetch-field "from")))
            (date (ekg-tools-gnus--clean-header-value
                   (mail-fetch-field "date")))
            (message-id (ekg-tools-gnus--clean-header-value
                         (mail-fetch-field "message-id"))))
        (unless (string-empty-p subject)
          (format "%s\t%s\t%s\t%s\t%s\t%s"
                  group article date from subject message-id))))))

;;;###autoload
(cl-defun ekg-tools-gnus-recent-headers (&key folder num-headers)
  "Return recent message headers for Gnus FOLDER.
NUM-HEADERS is capped by `ekg-tools-gnus-default-max-headers' and
`ekg-tools-gnus-header-max-attempts'.  This requests headers only,
not message bodies."
  (ekg-tools-gnus--ensure-ready)
  (unless (and (stringp folder) (gnus-get-info folder))
    (error "No such Gnus folder: %s" folder))
  (let* ((info (gnus-get-info folder))
         (num-headers (ekg-tools--clip-number
                       num-headers ekg-tools-gnus-default-max-headers 25))
         (active (ekg-tools-gnus--active info))
         (article (and active (cdr active)))
         (low (and active (car active)))
         (attempts 0)
         (gnus-verbose 0)
         (gnus-verbose-backends 0)
         lines)
    (while (and article low
                (>= article low)
                (< (length lines) num-headers)
                (< attempts ekg-tools-gnus-header-max-attempts))
      (setq attempts (1+ attempts))
      (when-let* ((line (ekg-tools-gnus--header-line folder article)))
        (push line lines))
      (setq article (1- article)))
    (if lines
        (string-join
         (cons "group\tarticle\tdate\tfrom\tsubject\tmessage-id"
               (nreverse lines))
         "\n")
      (format "No recent headers found for Gnus folder: %s" folder))))

(defconst ekg-tools-agent-tool-gnus-fetch-latest
  (make-llm-tool
   :function (lambda (level hard max-folders)
               (condition-case err
                   (ekg-tools-gnus-fetch-latest
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

(defconst ekg-tools-agent-tool-gnus-list-folders
  (make-llm-tool
   :function (lambda (max-folders include-empty only-subscribed)
               (condition-case err
                   (ekg-tools-gnus-list-folders
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

(defconst ekg-tools-agent-tool-gnus-recent-headers
  (make-llm-tool
   :function (lambda (folder num-headers)
               (condition-case err
                   (ekg-tools-gnus-recent-headers
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
      :description "Maximum number of recent headers, capped at 25."))))

(defconst ekg-tools-agent-gnus-tools
  (list ekg-tools-agent-tool-gnus-fetch-latest
        ekg-tools-agent-tool-gnus-list-folders
        ekg-tools-agent-tool-gnus-recent-headers)
  "Optional Gnus tools suitable for `ekg-agent-extra-tools'.")

(defun ekg-tools-elfeed--ensure-ready ()
  "Ensure Elfeed and its database are available."
  (unless (require 'elfeed nil t)
    (error "Elfeed is not installed"))
  (unless (require 'elfeed-db nil t)
    (error "Elfeed database support is not available"))
  (unless (require 'elfeed-search nil t)
    (error "Elfeed search support is not available"))
  (elfeed-db-ensure)
  (unless (and (hash-table-p elfeed-db-feeds)
               (hash-table-p elfeed-db-entries))
    (error "Elfeed database is not available")))

(defun ekg-tools-elfeed--configured-feed-map ()
  "Return a hash table mapping configured Elfeed feed URLs to tags."
  (let ((table (make-hash-table :test #'equal)))
    (when (boundp 'elfeed-feeds)
      (dolist (spec elfeed-feeds)
        (pcase spec
          ((pred stringp)
           (puthash spec nil table))
          (`(,url . ,tags)
           (when (stringp url)
             (puthash url tags table))))))
    table))

(defun ekg-tools-elfeed--feed-stats ()
  "Return a hash table of Elfeed feed statistics."
  (let ((table (make-hash-table :test #'equal)))
    (maphash
     (lambda (_id entry)
       (let* ((feed-id (elfeed-entry-feed-id entry))
              (stats (or (gethash feed-id table)
                         (list :entries 0 :unread 0 :latest nil))))
         (plist-put stats :entries (1+ (plist-get stats :entries)))
         (when (memq 'unread (elfeed-entry-tags entry))
           (plist-put stats :unread (1+ (plist-get stats :unread))))
         (when (or (null (plist-get stats :latest))
                   (> (elfeed-entry-date entry)
                      (elfeed-entry-date (plist-get stats :latest))))
           (plist-put stats :latest entry))
         (puthash feed-id stats table)))
     elfeed-db-entries)
    table))

(defun ekg-tools-elfeed--format-time (seconds)
  "Format epoch SECONDS for tool output."
  (if (and seconds (> seconds 0))
      (format-time-string "%F %R" (seconds-to-time seconds))
    ""))

(defun ekg-tools-elfeed--feed-line (feed configured-tags stats configured-p)
  "Return a tab-separated line for FEED.
CONFIGURED-TAGS are tags from `elfeed-feeds'.  STATS are database
statistics.  CONFIGURED-P is non-nil when the feed is configured."
  (let* ((latest (plist-get stats :latest))
         (entries (or (plist-get stats :entries) 0))
         (unread (or (plist-get stats :unread) 0))
         (source (cond
                  ((and configured-p (> entries 0)) "configured+db")
                  (configured-p "configured")
                  (t "db-only"))))
    (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
            (ekg-tools--clean-field (elfeed-feed-title feed))
            (ekg-tools--clean-field (or (elfeed-feed-url feed)
                                        (elfeed-feed-id feed)))
            (ekg-tools--clean-field
             (mapconcat #'symbol-name configured-tags ","))
            entries
            unread
            (if latest
                (ekg-tools-elfeed--format-time (elfeed-entry-date latest))
              "")
            (ekg-tools--clean-field
             (and latest (elfeed-entry-title latest)))
            source)))

;;;###autoload
(cl-defun ekg-tools-elfeed-list-feeds (&key max-feeds include-empty)
  "Return a tab-separated summary of Elfeed feeds.
MAX-FEEDS limits the result size.  If INCLUDE-EMPTY is nil, feeds
with no database entries are omitted."
  (ekg-tools-elfeed--ensure-ready)
  (let* ((max-feeds (ekg-tools--clip-number
                     max-feeds ekg-tools-elfeed-default-max-feeds 200))
         (include-empty (ekg-tools--bool include-empty))
         (configured (ekg-tools-elfeed--configured-feed-map))
         (stats (ekg-tools-elfeed--feed-stats))
         rows)
    (maphash
     (lambda (id feed)
       (let ((feed-stats (gethash id stats))
             (configured-tags (gethash id configured :missing)))
         (when (or include-empty feed-stats)
           (push (ekg-tools-elfeed--feed-line
                  feed
                  (if (eq configured-tags :missing) nil configured-tags)
                  feed-stats
                  (not (eq configured-tags :missing)))
                 rows))))
     elfeed-db-feeds)
    (maphash
     (lambda (url tags)
       (unless (gethash url elfeed-db-feeds)
         (let ((feed (elfeed-db-get-feed url)))
           (when include-empty
             (push (ekg-tools-elfeed--feed-line feed tags nil t)
                   rows)))))
     configured)
    (setq rows (sort rows #'string<))
    (if rows
        (string-join
         (cons (concat "title\turl\tconfigured_tags\tentries\tunread"
                       "\tlatest_date\tlatest_title\tsource")
               (seq-take rows max-feeds))
         "\n")
      "No Elfeed feeds found.")))

(defun ekg-tools-elfeed--entry-list ()
  "Return all Elfeed entries sorted newest first."
  (let (entries)
    (maphash (lambda (_id entry) (push entry entries)) elfeed-db-entries)
    (sort entries (lambda (a b)
                    (> (elfeed-entry-date a)
                       (elfeed-entry-date b))))))

(defun ekg-tools-elfeed--entry-matches-filter-p (entry feed filter)
  "Return non-nil when ENTRY from FEED matches parsed FILTER."
  (cl-destructuring-bind (&key after before must-have must-not-have
                               matches not-matches feeds not-feeds
                               &allow-other-keys)
      filter
    (let* ((tags (elfeed-entry-tags entry))
           (date (elfeed-entry-date entry))
           (age (- (float-time) date))
           (title (or (elfeed-entry-title entry) ""))
           (link (or (elfeed-entry-link entry) ""))
           (feed-title (or (elfeed-feed-title feed) ""))
           (feed-id (or (elfeed-feed-id feed) "")))
      (and (or (null after) (<= age after))
           (or (null before) (> age before))
           (cl-every (lambda (tag) (memq tag tags)) must-have)
           (cl-notany (lambda (tag) (memq tag tags)) must-not-have)
           (or (null matches)
               (cl-every (lambda (regex)
                           (or (string-match-p regex title)
                               (string-match-p regex link)))
                         matches))
           (cl-notany (lambda (regex)
                        (or (string-match-p regex title)
                            (string-match-p regex link)))
                      not-matches)
           (or (null feeds)
               (cl-some (lambda (regex)
                          (or (string-match-p regex feed-id)
                              (string-match-p regex feed-title)))
                        feeds))
           (cl-notany (lambda (regex)
                        (or (string-match-p regex feed-id)
                            (string-match-p regex feed-title)))
                      not-feeds)))))

(defun ekg-tools-elfeed--entry-content-text (entry max-chars)
  "Return ENTRY content as readable text, clipped to MAX-CHARS."
  (let* ((raw (elfeed-deref (elfeed-entry-content entry)))
         (text (cond
                ((not (stringp raw)) "")
                ((eq (elfeed-entry-content-type entry) 'html)
                 (ekg-tools--html-to-text raw))
                (t raw))))
    (ekg-tools--clip-string (string-trim text) max-chars)))

(defun ekg-tools-elfeed--entry-line (entry include-content max-content-chars)
  "Return a tab-separated line for ENTRY.
If INCLUDE-CONTENT is non-nil, include a content field clipped to
MAX-CONTENT-CHARS."
  (let ((feed (elfeed-entry-feed entry)))
    (string-join
     (append
      (list
       (ekg-tools--clean-field (elfeed-entry-id entry))
       (ekg-tools-elfeed--format-time (elfeed-entry-date entry))
       (ekg-tools--clean-field (and feed (elfeed-feed-title feed)))
       (ekg-tools--clean-field (elfeed-entry-title entry))
       (ekg-tools--clean-field (elfeed-entry-link entry))
       (ekg-tools--clean-field
        (mapconcat #'symbol-name (elfeed-entry-tags entry) ",")))
      (when include-content
        (list (ekg-tools--clean-field
               (ekg-tools-elfeed--entry-content-text
                entry max-content-chars)))))
     "\t")))

;;;###autoload
(cl-defun ekg-tools-elfeed-search-entries (&key filter num-entries
                                                include-content
                                                max-content-chars)
  "Return Elfeed entries matching FILTER.
FILTER uses Elfeed search syntax.  Empty FILTER means newest
entries without tag/date filtering.  NUM-ENTRIES limits the
result size.  INCLUDE-CONTENT adds clipped entry text to each
row, limited by MAX-CONTENT-CHARS."
  (ekg-tools-elfeed--ensure-ready)
  (let* ((filter (or filter ""))
         (parsed-filter (and (not (string-empty-p filter))
                             (elfeed-search-parse-filter filter)))
         (parsed-limit (plist-get parsed-filter :limit))
         (num-entries (ekg-tools--clip-number
                       num-entries
                       ekg-tools-elfeed-default-max-entries
                       100))
         (num-entries (if parsed-limit
                          (min num-entries parsed-limit)
                        num-entries))
         (include-content (ekg-tools--bool include-content))
         (max-content-chars (ekg-tools--clip-number
                             max-content-chars
                             ekg-tools-elfeed-default-content-chars
                             20000))
         rows)
    (catch 'done
      (dolist (entry (ekg-tools-elfeed--entry-list))
        (let ((feed (elfeed-entry-feed entry)))
          (when (or (null parsed-filter)
                    (ekg-tools-elfeed--entry-matches-filter-p
                     entry feed parsed-filter))
            (push (ekg-tools-elfeed--entry-line
                   entry include-content max-content-chars)
                  rows)
            (when (>= (length rows) num-entries)
              (throw 'done nil))))))
    (if rows
        (string-join
         (cons (concat "id\tdate\tfeed\ttitle\tlink\ttags"
                       (if include-content "\tcontent" ""))
               (nreverse rows))
         "\n")
      "No Elfeed entries found.")))

(defun ekg-tools-elfeed--find-entry (id-or-link)
  "Find an Elfeed entry by ID-OR-LINK."
  (or (elfeed-db-get-entry id-or-link)
      (let (found)
        (maphash
         (lambda (_id entry)
           (when (or (equal id-or-link (elfeed-entry-link entry))
                     (equal id-or-link (format "%s" (elfeed-entry-id entry))))
             (setq found entry)))
         elfeed-db-entries)
        found)))

;;;###autoload
(cl-defun ekg-tools-elfeed-get-entry (&key id-or-link max-content-chars)
  "Return one Elfeed entry by ID-OR-LINK, including content.
MAX-CONTENT-CHARS limits the returned content."
  (ekg-tools-elfeed--ensure-ready)
  (let* ((max-content-chars (ekg-tools--clip-number
                             max-content-chars
                             ekg-tools-elfeed-default-content-chars
                             50000))
         (entry (and (stringp id-or-link)
                     (ekg-tools-elfeed--find-entry id-or-link))))
    (if entry
        (let ((feed (elfeed-entry-feed entry)))
          (format (concat "Title: %s\nFeed: %s\nDate: %s\nTags: %s\n"
                          "Link: %s\nID: %s\n\n%s")
                  (ekg-tools--clean-field (elfeed-entry-title entry))
                  (ekg-tools--clean-field (and feed (elfeed-feed-title feed)))
                  (ekg-tools-elfeed--format-time (elfeed-entry-date entry))
                  (ekg-tools--clean-field
                   (mapconcat #'symbol-name (elfeed-entry-tags entry) ","))
                  (ekg-tools--clean-field (elfeed-entry-link entry))
                  (ekg-tools--clean-field (elfeed-entry-id entry))
                  (ekg-tools-elfeed--entry-content-text
                   entry max-content-chars)))
      (format "No Elfeed entry found for: %s" id-or-link))))

(defconst ekg-tools-agent-tool-elfeed-list-feeds
  (make-llm-tool
   :function (lambda (max-feeds include-empty)
               (condition-case err
                   (ekg-tools-elfeed-list-feeds
                    :max-feeds max-feeds
                    :include-empty include-empty)
                 (error (format "Error: %s" (error-message-string err)))))
   :name "elfeed_list_feeds"
   :description
   "List Elfeed feeds with entry counts, unread counts, and latest entries."
   :args
   '((:name "max_feeds"
      :type integer
      :description "Maximum number of feeds to return, capped at 200.")
     (:name "include_empty"
      :type boolean
      :description "Whether to include configured feeds with no entries."))))

(defconst ekg-tools-agent-tool-elfeed-search-entries
  (make-llm-tool
   :function (lambda (filter num-entries include-content max-content-chars)
               (condition-case err
                   (ekg-tools-elfeed-search-entries
                    :filter filter
                    :num-entries num-entries
                    :include-content include-content
                    :max-content-chars max-content-chars)
                 (error (format "Error: %s" (error-message-string err)))))
   :name "elfeed_search_entries"
   :description
   "Search Elfeed entries with Elfeed filter syntax, newest first."
   :args
   '((:name "filter"
      :type string
      :description "Elfeed filter, such as +unread, +tech, =Hacker, or @1-week-ago.")
     (:name "num_entries"
      :type integer
      :description "Maximum number of entries to return, capped at 100.")
     (:name "include_content"
      :type boolean
      :description "Whether to include clipped entry content in each row.")
     (:name "max_content_chars"
      :type integer
      :description "Maximum content characters per entry, capped at 20000."))))

(defconst ekg-tools-agent-tool-elfeed-get-entry
  (make-llm-tool
   :function (lambda (id-or-link max-content-chars)
               (condition-case err
                   (ekg-tools-elfeed-get-entry
                    :id-or-link id-or-link
                    :max-content-chars max-content-chars)
                 (error (format "Error: %s" (error-message-string err)))))
   :name "elfeed_get_entry"
   :description
   "Read one Elfeed entry by exact ID or link, including clipped content."
   :args
   '((:name "id_or_link"
      :type string
      :description "Exact entry ID or link from elfeed_search_entries.")
     (:name "max_content_chars"
      :type integer
      :description "Maximum content characters to return, capped at 50000."))))

(defconst ekg-tools-agent-elfeed-tools
  (list ekg-tools-agent-tool-elfeed-list-feeds
        ekg-tools-agent-tool-elfeed-search-entries
        ekg-tools-agent-tool-elfeed-get-entry)
  "Optional Elfeed tools suitable for `ekg-agent-extra-tools'.")

(provide 'ekg-tools)
;;; ekg-tools.el ends here
