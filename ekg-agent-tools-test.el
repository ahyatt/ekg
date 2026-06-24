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

(ekg-deftest ekg-agent-tools-test-gnus-read-message-does-not-mark-read ()
  "Reading a Gnus message should not use summary mark machinery."
  (let ((gnus-mark-article-hook '(gnus-summary-mark-read-and-unread-as-read))
        (summary-selected nil)
        (marked nil))
    (cl-letf (((symbol-function 'ekg-agent-tools-gnus--ensure-ready)
               (lambda () t))
              ((symbol-function 'gnus-get-info)
               (lambda (group) (list group 1 nil)))
              ((symbol-function 'gnus-request-article)
               (lambda (_article _group buffer)
                 (should-not gnus-mark-article-hook)
                 (with-current-buffer buffer
                   (insert "Subject: Test\n\nBody"))
                 t))
              ((symbol-function 'gnus-summary-select-article)
               (lambda (&rest _args)
                 (setq summary-selected t)))
              ((symbol-function 'gnus-summary-mark-article)
               (lambda (&rest _args)
                 (setq marked t)))
              ((symbol-function 'gnus-mark-article-as-read)
               (lambda (&rest _args)
                 (setq marked t))))
      (should (string-match-p
               "Body"
               (ekg-agent-tools-gnus-read-message
                :group "nnimap+test:INBOX"
                :article 1)))
      (should-not summary-selected)
      (should-not marked))))

(ekg-deftest ekg-agent-tools-test-gnus-header-fetch-does-not-mark-read ()
  "Fetching Gnus metadata should not enable article mark hooks."
  (let ((gnus-mark-article-hook '(gnus-summary-mark-read-and-unread-as-read))
        (summary-selected nil)
        (marked nil)
        (nntp-server-buffer (generate-new-buffer " *ekg-gnus-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'gnus-request-head)
                   (lambda (_article _group)
                     (should-not gnus-mark-article-hook)
                     (with-current-buffer nntp-server-buffer
                       (erase-buffer)
                       (insert (string-join
                                '("Subject: Test"
                                  "From: Person <person@example.com>"
                                  "Date: Mon, 1 Jun 2026 12:00:00 -0400"
                                  "Message-ID: <test@example.com>"
                                  "")
                                "\n")))
                     t))
                  ((symbol-function 'gnus-summary-select-article)
                   (lambda (&rest _args)
                     (setq summary-selected t)))
                  ((symbol-function 'gnus-summary-mark-article)
                   (lambda (&rest _args)
                     (setq marked t)))
                  ((symbol-function 'gnus-mark-article-as-read)
                   (lambda (&rest _args)
                     (setq marked t))))
          (let ((metadata (ekg-agent-tools-gnus--article-metadata
                           "nnimap+test:INBOX" 1)))
            (should (equal (plist-get metadata :subject) "Test"))
            (should-not summary-selected)
            (should-not marked)))
      (kill-buffer nntp-server-buffer))))

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
