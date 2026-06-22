;;; ekg-agent-tools-test.el --- Tests for optional ekg-agent tools -*- lexical-binding: t; -*-

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

;; Tests for optional tool schemas that do not need live service data.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'ekg-test-utils)
(require 'ekg-agent-tools-elfeed)
(require 'ekg-agent-tools-gnus)

(defun ekg-agent-tools-test--arg-names (tool)
  "Return argument names from TOOL."
  (mapcar (lambda (arg) (plist-get arg :name))
          (llm-tool-args tool)))

(ekg-deftest ekg-agent-tools-test-gnus-message-id-round-trip ()
  "Opaque Gnus message IDs decode back to their group and article."
  (let* ((message-id (ekg-agent-tools-gnus--message-id
                      "nnimap+work:INBOX" 12345))
         (decoded (ekg-agent-tools-gnus--decode-message-id message-id)))
    (should (equal decoded '("nnimap+work:INBOX" 12345)))))

(ekg-deftest ekg-agent-tools-test-gnus-read-message-schema ()
  "The Gnus read-message tool should accept source-agnostic message IDs."
  (let ((arg-names
         (ekg-agent-tools-test--arg-names
          ekg-agent-tools-gnus-tool-read-message)))
    (should (equal (car arg-names) "message_id"))
    (should-not (member "result_id" arg-names))
    (should (string-match-p
             "message_id"
             (llm-tool-description ekg-agent-tools-gnus-tool-read-message)))
    (should-not (string-match-p
                 "result_id"
                 (llm-tool-description ekg-agent-tools-gnus-tool-read-message)))))

(ekg-deftest ekg-agent-tools-test-gnus-search-result-line ()
  "Gnus search result rows separate opaque and RFC message IDs."
  (should
   (equal
    (ekg-agent-tools-gnus--search-result-line
     '(:message-id "opaque-id"
       :group "nnimap+work:INBOX"
       :article 42
       :date "Mon, 1 Jun 2026 12:00:00 -0400"
       :from "Person <person@example.com>"
       :subject "Subject"
       :rfc-message-id "<rfc@example.com>"
       :score 9))
    (string-join
     '("opaque-id"
       "nnimap+work:INBOX"
       "42"
       "Mon, 1 Jun 2026 12:00:00 -0400"
       "Person <person@example.com>"
       "Subject"
       "<rfc@example.com>"
       "9")
     "\t"))))

(ekg-deftest ekg-agent-tools-test-elfeed-get-entry-schema ()
  "The Elfeed get-entry tool should not tie IDs to one producer tool."
  (let ((description
         (cadr (memq :description
                     (car (llm-tool-args
                           ekg-agent-tools-elfeed-tool-get-entry))))))
    (should (string-match-p "Exact Elfeed entry ID or link" description))
    (should-not (string-match-p "elfeed_search_entries" description))))

(provide 'ekg-agent-tools-test)

;;; ekg-agent-tools-test.el ends here
