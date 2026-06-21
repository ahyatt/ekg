;;; ekg-agent-tools-elfeed.el --- Optional Elfeed tools for ekg-agent -*- lexical-binding: t -*-

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
;; This package provides optional Elfeed tools for `ekg-agent'.  These tools
;; can expose local feed data, so users must opt into them explicitly.

;;; Code:

(require 'cl-lib)
(require 'ekg-agent)
(require 'llm)
(require 'seq)
(require 'subr-x)

(defvar elfeed-db-entries)
(defvar elfeed-db-feeds)
(defvar elfeed-feeds)
(defvar elfeed-search-filter)

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

(defgroup ekg-agent-tools-elfeed nil
  "Optional Elfeed tools for ekg-agent."
  :group 'ekg)

(defcustom ekg-agent-tools-elfeed-default-max-feeds 50
  "Default maximum number of Elfeed feeds returned by the feed tool."
  :type 'integer
  :group 'ekg-agent-tools-elfeed)

(defcustom ekg-agent-tools-elfeed-default-max-entries 20
  "Default maximum number of Elfeed entries returned by the entry tool."
  :type 'integer
  :group 'ekg-agent-tools-elfeed)

(defcustom ekg-agent-tools-elfeed-default-content-chars 4000
  "Default maximum number of characters returned for Elfeed entry content."
  :type 'integer
  :group 'ekg-agent-tools-elfeed)

(defun ekg-agent-tools-elfeed--bool (value)
  "Return non-nil when VALUE represents true."
  (and value (not (memq value '(false :false :json-false)))))

(defun ekg-agent-tools-elfeed--clip-number (value default maximum)
  "Return VALUE as a positive number, or DEFAULT, capped at MAXIMUM."
  (let ((value (if (numberp value) value default)))
    (min maximum (max 1 value))))

(defun ekg-agent-tools-elfeed--clean-field (value)
  "Return VALUE as one safe tab-separated field."
  (require 'xml)
  (let ((value (xml-substitute-special (format "%s" (or value "")))))
    (string-trim
     (replace-regexp-in-string
      "[\t\n\r]+" " "
      (replace-regexp-in-string "[\x3fff80-\x3fffff]" "" value)))))

(defun ekg-agent-tools-elfeed--clip-string (text max-chars)
  "Return TEXT clipped to MAX-CHARS with a truncation notice."
  (let ((text (or text "")))
    (if (> (length text) max-chars)
        (concat (substring text 0 max-chars)
                "\n\n[Content truncated at "
                (number-to-string max-chars) " characters]")
      text)))

(defun ekg-agent-tools-elfeed--html-to-text (html)
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

(defun ekg-agent-tools-elfeed--ensure-ready ()
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

(defun ekg-agent-tools-elfeed--configured-feed-map ()
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

(defun ekg-agent-tools-elfeed--feed-stats ()
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

(defun ekg-agent-tools-elfeed--format-time (seconds)
  "Format epoch SECONDS for tool output."
  (if (and seconds (> seconds 0))
      (format-time-string "%F %R" (seconds-to-time seconds))
    ""))

(defun ekg-agent-tools-elfeed--feed-line (feed configured-tags stats configured-p)
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
            (ekg-agent-tools-elfeed--clean-field (elfeed-feed-title feed))
            (ekg-agent-tools-elfeed--clean-field (or (elfeed-feed-url feed)
                                        (elfeed-feed-id feed)))
            (ekg-agent-tools-elfeed--clean-field
             (mapconcat #'symbol-name configured-tags ","))
            entries
            unread
            (if latest
                (ekg-agent-tools-elfeed--format-time (elfeed-entry-date latest))
              "")
            (ekg-agent-tools-elfeed--clean-field
             (and latest (elfeed-entry-title latest)))
            source)))

;;;###autoload
(cl-defun ekg-agent-tools-elfeed-list-feeds (&key max-feeds include-empty)
  "Return a tab-separated summary of Elfeed feeds.
MAX-FEEDS limits the result size.  If INCLUDE-EMPTY is nil, feeds
with no database entries are omitted."
  (ekg-agent-tools-elfeed--ensure-ready)
  (let* ((max-feeds (ekg-agent-tools-elfeed--clip-number
                     max-feeds ekg-agent-tools-elfeed-default-max-feeds 200))
         (include-empty (ekg-agent-tools-elfeed--bool include-empty))
         (configured (ekg-agent-tools-elfeed--configured-feed-map))
         (stats (ekg-agent-tools-elfeed--feed-stats))
         rows)
    (maphash
     (lambda (id feed)
       (let ((feed-stats (gethash id stats))
             (configured-tags (gethash id configured :missing)))
         (when (or include-empty feed-stats)
           (push (ekg-agent-tools-elfeed--feed-line
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
             (push (ekg-agent-tools-elfeed--feed-line feed tags nil t)
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

(defun ekg-agent-tools-elfeed--insert-newest-entry (entry entries limit)
  "Insert ENTRY into newest-first ENTRIES, keeping at most LIMIT entries."
  (let ((entry-date (elfeed-entry-date entry))
        inserted
        result)
    (dolist (current entries)
      (unless (or inserted
                  (<= entry-date (elfeed-entry-date current)))
        (push entry result)
        (setq inserted t))
      (push current result))
    (unless inserted
      (push entry result))
    (seq-take (nreverse result) limit)))

(defun ekg-agent-tools-elfeed--entry-matches-filter-p (entry feed filter)
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

(defun ekg-agent-tools-elfeed--entry-content-text (entry max-chars)
  "Return ENTRY content as readable text, clipped to MAX-CHARS."
  (let* ((raw (elfeed-deref (elfeed-entry-content entry)))
         (text (cond
                ((not (stringp raw)) "")
                ((eq (elfeed-entry-content-type entry) 'html)
                 (ekg-agent-tools-elfeed--html-to-text raw))
                (t raw))))
    (ekg-agent-tools-elfeed--clip-string (string-trim text) max-chars)))

(defun ekg-agent-tools-elfeed--entry-line (entry include-content max-content-chars)
  "Return a tab-separated line for ENTRY.
If INCLUDE-CONTENT is non-nil, include a content field clipped to
MAX-CONTENT-CHARS."
  (let ((feed (elfeed-entry-feed entry)))
    (string-join
     (append
      (list
       (ekg-agent-tools-elfeed--clean-field (elfeed-entry-id entry))
       (ekg-agent-tools-elfeed--format-time (elfeed-entry-date entry))
       (ekg-agent-tools-elfeed--clean-field (and feed (elfeed-feed-title feed)))
       (ekg-agent-tools-elfeed--clean-field (elfeed-entry-title entry))
       (ekg-agent-tools-elfeed--clean-field (elfeed-entry-link entry))
       (ekg-agent-tools-elfeed--clean-field
        (mapconcat #'symbol-name (elfeed-entry-tags entry) ",")))
      (when include-content
        (list (ekg-agent-tools-elfeed--clean-field
               (ekg-agent-tools-elfeed--entry-content-text
                entry max-content-chars)))))
     "\t")))

;;;###autoload
(cl-defun ekg-agent-tools-elfeed-search-entries (&key filter num-entries
                                                include-content
                                                max-content-chars)
  "Return Elfeed entries matching FILTER.
FILTER uses Elfeed search syntax.  Empty FILTER means newest
entries without tag/date filtering.  NUM-ENTRIES limits the
result size.  INCLUDE-CONTENT adds clipped entry text to each
row, limited by MAX-CONTENT-CHARS."
  (ekg-agent-tools-elfeed--ensure-ready)
  (let* ((filter (or filter ""))
         (parsed-filter (and (not (string-empty-p filter))
                             (elfeed-search-parse-filter filter)))
         (parsed-limit (plist-get parsed-filter :limit))
         (num-entries (ekg-agent-tools-elfeed--clip-number
                       num-entries
                       ekg-agent-tools-elfeed-default-max-entries
                       100))
         (num-entries (if parsed-limit
                          (min num-entries parsed-limit)
                        num-entries))
         (include-content (ekg-agent-tools-elfeed--bool include-content))
         (max-content-chars (ekg-agent-tools-elfeed--clip-number
                             max-content-chars
                             ekg-agent-tools-elfeed-default-content-chars
                             20000))
        entries
        rows)
    (maphash
     (lambda (_id entry)
       (let ((feed (elfeed-entry-feed entry)))
         (when (or (null parsed-filter)
                   (ekg-agent-tools-elfeed--entry-matches-filter-p
                    entry feed parsed-filter))
           (setq entries
                 (ekg-agent-tools-elfeed--insert-newest-entry
                  entry entries num-entries)))))
     elfeed-db-entries)
    (setq rows
          (mapcar (lambda (entry)
                    (ekg-agent-tools-elfeed--entry-line
                     entry include-content max-content-chars))
                  entries))
    (if rows
        (string-join
         (cons (concat "id\tdate\tfeed\ttitle\tlink\ttags"
                       (if include-content "\tcontent" ""))
               rows)
         "\n")
      "No Elfeed entries found.")))

(defun ekg-agent-tools-elfeed--find-entry (id-or-link)
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
(cl-defun ekg-agent-tools-elfeed-get-entry (&key id-or-link max-content-chars)
  "Return one Elfeed entry by ID-OR-LINK, including content.
MAX-CONTENT-CHARS limits the returned content."
  (ekg-agent-tools-elfeed--ensure-ready)
  (let* ((max-content-chars (ekg-agent-tools-elfeed--clip-number
                             max-content-chars
                             ekg-agent-tools-elfeed-default-content-chars
                             50000))
         (entry (and (stringp id-or-link)
                     (ekg-agent-tools-elfeed--find-entry id-or-link))))
    (if entry
        (let ((feed (elfeed-entry-feed entry)))
          (format (concat "Title: %s\nFeed: %s\nDate: %s\nTags: %s\n"
                          "Link: %s\nID: %s\n\n%s")
                  (ekg-agent-tools-elfeed--clean-field (elfeed-entry-title entry))
                  (ekg-agent-tools-elfeed--clean-field (and feed (elfeed-feed-title feed)))
                  (ekg-agent-tools-elfeed--format-time (elfeed-entry-date entry))
                  (ekg-agent-tools-elfeed--clean-field
                   (mapconcat #'symbol-name (elfeed-entry-tags entry) ","))
                  (ekg-agent-tools-elfeed--clean-field (elfeed-entry-link entry))
                  (ekg-agent-tools-elfeed--clean-field (elfeed-entry-id entry))
                  (ekg-agent-tools-elfeed--entry-content-text
                   entry max-content-chars)))
      (format "No Elfeed entry found for: %s" id-or-link))))

(defconst ekg-agent-tools-elfeed-tool-list-feeds
  (make-llm-tool
   :function (lambda (max-feeds include-empty)
               (condition-case err
                   (ekg-agent-tools-elfeed-list-feeds
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

(defconst ekg-agent-tools-elfeed-tool-search-entries
  (make-llm-tool
   :function (lambda (filter num-entries include-content max-content-chars)
               (condition-case err
                   (ekg-agent-tools-elfeed-search-entries
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

(defconst ekg-agent-tools-elfeed-tool-get-entry
  (make-llm-tool
   :function (lambda (id-or-link max-content-chars)
               (condition-case err
                   (ekg-agent-tools-elfeed-get-entry
                    :id-or-link id-or-link
                    :max-content-chars max-content-chars)
                 (error (format "Error: %s" (error-message-string err)))))
   :name "elfeed_get_entry"
   :description
   "Read one Elfeed entry by exact ID or link, including clipped content."
   :args
   '((:name "id_or_link"
            :type string
            :description "Exact Elfeed entry ID or link.")
     (:name "max_content_chars"
            :type integer
            :description "Maximum content characters to return, capped at 50000."))))

(defconst ekg-agent-tools-elfeed-tools
  (list ekg-agent-tools-elfeed-tool-list-feeds
        ekg-agent-tools-elfeed-tool-search-entries
        ekg-agent-tools-elfeed-tool-get-entry)
  "Optional Elfeed tools suitable for `ekg-agent-extra-tools'.")

(provide 'ekg-agent-tools-elfeed)
;;; ekg-agent-tools-elfeed.el ends here
