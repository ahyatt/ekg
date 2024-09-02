;;; ekg-email.el --- Storing email including structured data -*- lexical-binding: t -*-

;; Copyright (C) 2024  Andrew Hyatt

;; Author: Andrew Hyatt <ahyatt@gmail.com>

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
;; This package provides a way to store email in a structured way, in a note.
;; It has integration with gnus, and other integrations can be easily added.

;;; Code:

(require 'ekg)
(require 'triples)
(require 'seq)

(defun ekg-email-add-schema ()
  "Add the email schema to the ekg database."
  (triples-add-schema ekg-db 'email
                      '(to :base/type string)
                      '(from :base/type string :base/unique t)
                      '(cc :base/type string))
  (triples-add-schema ekg-db 'email-address
                      '(emails-to :base/virtual-reversed email/to)
                      '(emails-from :base/virtual-reversed email/from)
                      '(emails-cc :base/virtual-reversed email/cc)))

(add-hook 'ekg-add-schema-hook 'ekg-email-add-schema)

(add-to-list 'ekg-metadata-parsers
             (cons "Email-To" 'ekg-email--metadata-update-to))
(add-to-list 'ekg-metadata-parsers
             (cons "Email-From" 'ekg-email--metadata-update-from))
(add-to-list 'ekg-metadata-parsers
             (cons "Email-CC" 'ekg-email--metadata-update-cc))

(add-to-list 'ekg-property-multivalue-type (cons "Email-To" 'comma))
(add-to-list 'ekg-property-multivalue-type (cons "Email-CC" 'comma))

(add-to-list 'ekg-metadata-labels (cons :email/to "Email-To"))
(add-to-list 'ekg-metadata-labels (cons :email/from "Email-From"))
(add-to-list 'ekg-metadata-labels (cons :email/cc "Email-CC"))

(defun ekg-email--metadata-update-to (val)
  "Update the email-to metadata with VAL."
  (setf (ekg-note-properties ekg-note)
        (plist-put (ekg-note-properties ekg-note) :email/to val)))

(defun ekg-email--metadata-update-from (val)
  "Update the email-from metadata with VAL."
  (setf (ekg-note-properties ekg-note)
        (plist-put (ekg-note-properties ekg-note) :email/from val)))

(defun ekg-email--metadata-update-cc (val)
  "Update the email-cc metadata with VAL."
  (setf (ekg-note-properties ekg-note)
        (plist-put (ekg-note-properties ekg-note) :email/cc val)))

(defun ekg-display-note-email (note)
  "Display the email NOTE."
  (let ((to (plist-get (ekg-note-properties note) :email/to))
        (from (plist-get (ekg-note-properties note) :email/from))
        (cc (plist-get (ekg-note-properties note) :email/cc)))
    (if (or to from cc)
        (concat "To: " (string-join to ", ") "\n"
                "From: " from "\n"
                "CC: " (string-join cc ", ") "\n"))))

(defun ekg-email-from-gnus ()
  "Create a note with the current email as content and metadata."
  (interactive nil gnus-article-mode)
  (let* ((to (mapcar #'string-trim (split-string (gnus-fetch-field "To") ",")))
         (from (gnus-fetch-field "From"))
         (cc (when-let ((cc (gnus-fetch-field "Cc")))
               (mapcar #'string-trim (split-string cc ","))))
         (subject (gnus-fetch-field "Subject"))
         (prop-plist `(:email/to ,to :email/from ,from :titled/title ,subject)))
    (when cc
      (plist-put prop-plist :email/cc cc))
    (ekg-capture
     :text (gnus-with-article-buffer
             (buffer-substring-no-properties
              (save-excursion
                (article-goto-body)
                (point))
              (point-max)))
     :properties prop-plist)))

(defun ekg-email-save (note)
  "On a save, for any email addresses, type them with email-address.

NOTE is the note being saved."
  (dolist (addr (seq-uniq
                 (append
                  (plist-get (ekg-note-properties note)
                             :email/to)
                  (list (plist-get (ekg-note-properties note) :email/from))
                  (plist-get (ekg-note-properties note)
                             :email/cc))))
    (triples-set-type ekg-db addr 'email-address)))

(add-hook 'ekg-note-save-hook #'ekg-email-save)

(defun ekg-email-delete (note-id)
  "Delete email information associated with a note being deleted.

NOTE-ID is the id of the note that will be deleted."
  (triples-remove-type ekg-db note-id 'email))

(add-hook 'ekg-note-delete-hook #'ekg-email-delete)

(defun ekg-email-addresses ()
  "Return all addresses found in email."
  (ekg-connect)
  (seq-difference
   (triples-subjects-of-type ekg-db 'email-address)
   (ekg-inactive-note-ids)))

(defun ekg-email-get-notes-ids-with-address (addr)
  "Get all notes ids with emails to/from/cc containing ADDR.

ADDR must be a value matching a stored address, and not, say just
an email address that is part of what we store in the triples."
  (ekg-connect)
  (seq-uniq
   (let ((addr-vals (triples-get-type ekg-db addr 'email-address)))
     (append
      (plist-get addr-vals :emails-to)
      (plist-get addr-vals :emails-from)
      (plist-get addr-vals :emails-cc)))))

(defun ekg-email-show-notes-with-address (addr)
  "Show all notes to, from, or cc'd to ADDR."
  (interactive (list (completing-read "Addrs: "
                                      (ekg-email-addresses))))
  (ekg-connect)
  (ekg-setup-notes-buffer
   (format "with address: %s" addr)
   (lambda () (sort
               (mapcar #'ekg-get-note-with-id
                       (seq-filter
                        #'ekg-active-id-p
                        (ekg-email-get-notes-ids-with-address addr)))
               #'ekg-sort-by-creation-time))
   nil))

(provide 'ekg-email)
;;; ekg-email.el ends here
