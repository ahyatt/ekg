;;; ekg-apple-notes.el --- Bidirectional sync between ekg and Apple Notes -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

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
;; Bidirectional sync between ekg and Apple Notes on macOS.  Notes are
;; stored in a configurable Apple Notes folder.  Tags are stored as
;; individual "Tag: <tag>" lines.  Content is converted between
;; org-mode/markdown and HTML via pandoc.
;;
;; Usage:
;;   (require 'ekg-apple-notes)
;;   (ekg-apple-notes-sync)

;;; Code:

(require 'ekg)
(require 'triples)
(require 'seq)
(require 'cl-lib)

(defgroup ekg-apple-notes nil
  "Customization for ekg's Apple Notes integration."
  :group 'ekg)

(defcustom ekg-apple-notes-folder "ekg"
  "Name of the Apple Notes folder used for sync."
  :type 'string
  :group 'ekg-apple-notes)

(defcustom ekg-apple-notes-account nil
  "Apple Notes account to use, or nil for the default account."
  :type '(choice (const nil) string)
  :group 'ekg-apple-notes)

(defcustom ekg-apple-notes-pandoc-executable "pandoc"
  "Path to the pandoc executable for content conversion."
  :type 'string
  :group 'ekg-apple-notes)

(defcustom ekg-apple-notes-export-tags nil
  "If non-nil, only export notes that have at least one of these tags.
If nil, all non-hidden notes are exported."
  :type '(repeat string)
  :group 'ekg-apple-notes)

(defcustom ekg-apple-notes-exclude-tags nil
  "Tags whose notes should be excluded from sync.
Notes with any of these tags will not be exported or imported.
If `ekg-agent' is loaded, its tag variables
`ekg-agent-author-tag' and `ekg-agent-self-info-tag' are
automatically excluded as well."
  :type '(repeat string)
  :group 'ekg-apple-notes)

(defun ekg-apple-notes--exclude-tags ()
  "Return the effective list of tags to exclude from sync.
Merges `ekg-apple-notes-exclude-tags' with the `ekg-agent' tag
variables if `ekg-agent' is loaded."
  (seq-uniq
   (append ekg-apple-notes-exclude-tags
           (when (boundp 'ekg-agent-author-tag)
             (list (symbol-value 'ekg-agent-author-tag)))
           (when (boundp 'ekg-agent-self-info-tag)
             (list (symbol-value 'ekg-agent-self-info-tag))))))

;;; ---- Schema ----

(defun ekg-apple-notes-add-schema ()
  "Add the Apple Notes schema to the ekg database."
  (triples-add-schema ekg-db 'apple-notes
                      '(last-export :base/unique t :base/type integer)
                      '(last-import :base/unique t :base/type integer)
                      '(note-id :base/unique t :base/type string)
                      '(folder :base/unique t :base/type string)))

(defun ekg-apple-notes-connect ()
  "Connect to ekg and ensure the Apple Notes schema is set up."
  (ekg-connect)
  (ekg-apple-notes-add-schema))

(defun ekg-apple-notes--get-folder ()
  "Return the Apple Notes folder name for this database.
Use the value stored in the database if present, otherwise fall
back to `ekg-apple-notes-folder'."
  (or (plist-get (triples-get-type ekg-db 'apple-notes 'apple-notes)
                 :folder)
      ekg-apple-notes-folder))

(defun ekg-apple-notes-set-folder (folder)
  "Store FOLDER as the Apple Notes folder for this ekg database.
Future syncs will use this folder instead of `ekg-apple-notes-folder'."
  (interactive "sName for the ekg folder in Apple Notes for this database: ")
  (ekg-apple-notes-connect)
  (let ((plist (triples-get-type ekg-db 'apple-notes 'apple-notes)))
    (apply #'triples-set-type ekg-db 'apple-notes 'apple-notes
           (plist-put plist :folder folder))))

(defun ekg-apple-notes--get-last-export ()
  "Return the last export time as an integer epoch, or 0."
  (or (plist-get (triples-get-type ekg-db 'apple-notes 'apple-notes)
                 :last-export)
      0))

(defun ekg-apple-notes--set-last-export (time)
  "Set the last export time to TIME."
  (let ((plist (triples-get-type ekg-db 'apple-notes 'apple-notes)))
    (apply #'triples-set-type ekg-db 'apple-notes 'apple-notes
           (plist-put plist :last-export (floor (float-time time))))))

(defun ekg-apple-notes--get-last-import ()
  "Return the last import time as an integer epoch, or 0."
  (or (plist-get (triples-get-type ekg-db 'apple-notes 'apple-notes)
                 :last-import)
      0))

(defun ekg-apple-notes--set-last-import (time)
  "Set the last import time to TIME."
  (let ((plist (triples-get-type ekg-db 'apple-notes 'apple-notes)))
    (apply #'triples-set-type ekg-db 'apple-notes 'apple-notes
           (plist-put plist :last-import (floor (float-time time))))))

;;; ---- ID Mapping ----

(defun ekg-apple-notes--set-apple-id (ekg-id apple-id)
  "Store the mapping from EKG-ID to APPLE-ID."
  (triples-set-type ekg-db ekg-id 'apple-notes
                    :note-id apple-id))

(defun ekg-apple-notes--get-apple-id (ekg-id)
  "Return the Apple Notes ID for EKG-ID, or nil."
  (plist-get (triples-get-type ekg-db ekg-id 'apple-notes) :note-id))

(defun ekg-apple-notes--get-ekg-id (apple-id)
  "Return the ekg note ID mapped to APPLE-ID, or nil."
  (car (triples-subjects-with-predicate-object
        ekg-db 'apple-notes/note-id apple-id)))

(defun ekg-apple-notes-reset-mappings ()
  "Clear all Apple ID mappings from the database."
  (interactive)
  (dolist (subj (triples-subjects-of-type ekg-db 'apple-notes))
    (triples-remove-type ekg-db subj 'apple-notes)))

;;; ---- AppleScript Layer ----

(defun ekg-apple-notes--run-applescript (script)
  "Run SCRIPT via osascript and return the output string.
Signals an error if osascript fails."
  (with-temp-buffer
    (let ((exit-code (call-process "osascript" nil t nil "-e" script)))
      (if (= exit-code 0)
          (string-trim (buffer-string))
        (error "AppleScript error (exit %d): %s" exit-code (buffer-string))))))

(defun ekg-apple-notes--folder-ref ()
  "Return the AppleScript reference to the sync folder."
  (let ((folder (ekg-apple-notes--get-folder)))
    (if ekg-apple-notes-account
        (format "folder %S of account %S"
                folder ekg-apple-notes-account)
      (format "folder %S" folder))))

(defun ekg-apple-notes--ensure-folder ()
  "Create the sync folder in Apple Notes if it doesn't exist."
  (let ((folder (ekg-apple-notes--get-folder))
        (acct-clause (if ekg-apple-notes-account
                         (format " of account %S" ekg-apple-notes-account)
                       "")))
    (ekg-apple-notes--run-applescript
     (format "tell application \"Notes\"
  launch
  try
    get folder %S%s
  on error
    make new folder%s with properties {name:%S}
  end try
end tell"
             folder acct-clause
             acct-clause folder))))

(defun ekg-apple-notes--applescript-escape (str)
  "Escape STR for safe embedding in an AppleScript string literal.
Only backslashes and double quotes need escaping in AppleScript."
  (replace-regexp-in-string
   "\"" "\\\\\""
   (replace-regexp-in-string "\\\\" "\\\\\\\\" str)))

(defun ekg-apple-notes--with-body-file (body func)
  "Write BODY to a temp file, call FUNC with the file path, then clean up.
FUNC receives the temp file path and should return the result.
This avoids AppleScript string escaping issues for large HTML content."
  (let ((temp-file (make-temp-file "ekg-apple-notes-" nil ".html")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert body))
          (funcall func temp-file))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun ekg-apple-notes--create-note (title body)
  "Create a note with TITLE and BODY in the sync folder.
Returns the Apple Notes ID of the new note."
  (ekg-apple-notes--with-body-file body
                                   (lambda (body-file)
                                     (ekg-apple-notes--run-applescript
                                      (format "tell application \"Notes\"
  launch
  set bodyText to read POSIX file %S as «class utf8»
  set theNote to make new note at %s with properties {name:\"%s\", body:bodyText}
  return id of theNote
end tell"
                                              body-file
                                              (ekg-apple-notes--folder-ref)
                                              (ekg-apple-notes--applescript-escape title))))))

(defun ekg-apple-notes--update-note (apple-id body)
  "Update the note with APPLE-ID to have the given BODY."
  (ekg-apple-notes--with-body-file body
                                   (lambda (body-file)
                                     (ekg-apple-notes--run-applescript
                                      (format "tell application \"Notes\"
  launch
  set bodyText to read POSIX file %S as «class utf8»
  set body of note id %S to bodyText
end tell"
                                              body-file apple-id)))))

(defun ekg-apple-notes--delete-note (apple-id)
  "Delete the note with APPLE-ID.  Move it to Recently Deleted."
  (condition-case nil
      (ekg-apple-notes--run-applescript
       (format "tell application \"Notes\"
  launch
  delete note id %S
end tell"
               apple-id))
    (error nil)))

(cl-defstruct ekg-apple-notes--note
  "Representation of an Apple Notes note for sync."
  id name body modification-date)

(defun ekg-apple-notes--list-notes ()
  "Return a list of `ekg-apple-notes--note' for all notes in the sync folder."
  (let* ((raw (condition-case nil
                  (ekg-apple-notes--run-applescript
                   (format "tell application \"Notes\"
  launch
  set noteData to {}
  repeat with n in notes of %s
    set noteId to id of n
    set noteName to name of n
    set noteBody to body of n
    set noteMod to modification date of n as «class isot» as string
    set end of noteData to noteId & \"|||\" & noteName & \"|||\" & noteMod & \"|||\" & noteBody
  end repeat
  set AppleScript's text item delimiters to \"###NOTE###\"
  return noteData as string
end tell"
                           (ekg-apple-notes--folder-ref)))
                (error "")))
         (entries (if (string-empty-p raw) nil
                    (split-string raw "###NOTE###"))))
    (delq nil
          (mapcar
           (lambda (entry)
             (when (string-match "\\`\\(x-coredata://[^|]+\\)|||\\([^|]*\\)|||\\([^|]+\\)|||\\(\\(?:.\\|\n\\)*\\)\\'" entry)
               (make-ekg-apple-notes--note
                :id (match-string 1 entry)
                :name (match-string 2 entry)
                :modification-date (match-string 3 entry)
                :body (match-string 4 entry))))
           entries))))

;;; ---- Tag Conversion ----

(defun ekg-apple-notes--tags-to-metadata (tags)
  "Convert a list of ekg TAGS to metadata lines for the note body.
Hidden tags are excluded.  Each tag gets its own \"Tag: <tag>\" line."
  (let ((visible-tags (seq-difference tags ekg-hidden-tags)))
    (when visible-tags
      (mapconcat (lambda (tag) (concat "Tag: " tag))
                 visible-tags "\n"))))

(defun ekg-apple-notes--parse-tags-from-body (body)
  "Parse \"Tag: <tag>\" lines from BODY.
Returns a list of tag strings.  Each tag occupies its own line in
the format \"Tag: <tag>\", possibly wrapped in HTML div elements."
  (let ((tags nil))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
              "\\(?:<div>\\)?Tag: \\([^<\n]+?\\)\\(?:</div>\\|$\\)" nil t)
        (push (string-trim (match-string 1)) tags)))
    (nreverse tags)))

(defun ekg-apple-notes--remove-tags-line (text)
  "Remove \"Tag: <tag>\" lines from TEXT."
  (replace-regexp-in-string "\\(?:\n\\)?Tag: [^\n]+" "" text))

(defun ekg-apple-notes--parse-resource-from-body (body)
  "Parse a \"Resource: <url>\" line from BODY.
Returns the resource string, or nil if not found."
  (when (string-match
         "\\(?:<div>\\)?Resource: \\([^<\n]+?\\)\\(?:</div>\\|$\\)" body)
    (string-trim (match-string 1 body))))

(defun ekg-apple-notes--remove-resource-html (html)
  "Remove the \"Resource:\" div and its trailing spacer from HTML."
  (let ((html (replace-regexp-in-string
               "<div>Resource: [^<]*</div>\n?\\(?:<div><br></div>\n?\\)?"
               "" html)))
    html))

(defun ekg-apple-notes--remove-tags-html (html)
  "Remove \"Tag: <tag>\" divs and their preceding spacer from HTML.
This should be called before pandoc conversion to avoid artifacts."
  (let ((html (replace-regexp-in-string
               "<div><br></div>\n?\\(?:<div>Tag: [^<]*</div>\n?\\)+"
               "" html)))
    ;; Handle case where there's no spacer div.
    (replace-regexp-in-string
     "<div>Tag: [^<]*</div>\n?" "" html)))

;;; ---- Content Conversion ----

(defun ekg-apple-notes--pandoc (input from to)
  "Convert INPUT string from format FROM to format TO using pandoc."
  (unless (executable-find ekg-apple-notes-pandoc-executable)
    (error "Pandoc not found; install it or set `ekg-apple-notes-pandoc-executable'"))
  (with-temp-buffer
    (let ((exit-code
           (call-process-region input nil
                                ekg-apple-notes-pandoc-executable
                                nil t nil
                                "-f" from "-t" to
                                "--wrap=none")))
      (if (= exit-code 0)
          (string-trim (buffer-string))
        (error "Pandoc conversion failed (%s→%s, exit %d): %s"
               from to exit-code (buffer-string))))))

(defun ekg-apple-notes--html-delink (html)
  "Replace <a href=URL>text</a> in HTML with [text](URL).
Apple Notes strips link hrefs, so we inline them before sending."
  (replace-regexp-in-string
   "<a[^>]*href=\"\\([^\"]*\\)\"[^>]*>\\([^<]*\\)</a>"
   "[\\2](\\1)"
   html))

(defun ekg-apple-notes--note-title (note)
  "Return a title for the ekg NOTE.
Uses the :titled/title property if available, otherwise a
truncated version of the text."
  (or (let ((title (plist-get (ekg-note-properties note) :titled/title)))
        (if (listp title) (car title) title))
      (truncate-string-to-width
       (replace-regexp-in-string "\n" " " (or (ekg-note-text note) ""))
       80 nil nil "…")))

(defun ekg-apple-notes--to-html (note)
  "Convert an ekg NOTE's text to HTML for Apple Notes.
Handles `org-mode' and markdown via pandoc, with links inlined as
markdown format [text](url)."
  (let* ((text (or (ekg-note-text note) ""))
         (mode (ekg-note-mode note))
         (from (pcase mode
                 ('org-mode "org")
                 ('markdown-mode "markdown")
                 (_ "markdown")))
         (html (ekg-apple-notes--pandoc text from "html"))
         (html (ekg-apple-notes--html-delink html))
         (tags-meta (ekg-apple-notes--tags-to-metadata (ekg-note-tags note))))
    (concat (when (ekg-should-show-id-p (ekg-note-id note))
              (format "<div>Resource: %s</div>\n<div><br></div>\n"
                      (ekg-note-id note)))
            html
            (when tags-meta
              (concat "\n<div><br></div>\n"
                      (mapconcat (lambda (line) (concat "<div>" line "</div>"))
                                 (split-string tags-meta "\n")
                                 "\n"))))))

(defun ekg-apple-notes--relink-org (text)
  "Convert [text](url) patterns in TEXT to `org-mode' links [[url][text]].
Does not handle nested brackets or parentheses in link text or URLs."
  (replace-regexp-in-string
   "\\[\\([^]]+\\)\\](\\([^)]+\\))"
   "[[\\2][\\1]]"
   text))

(defun ekg-apple-notes--normalize-divs (html)
  "Convert <div> wrappers to <p> in HTML.
Apple Notes wraps every line in <div> tags, which pandoc passes
through as raw HTML when converting to markdown.  Converting to
<p> gives pandoc proper paragraph structure to work with.
Also strips trailing <br> inside divs, which Apple Notes adds
when users edit notes and which pandoc would convert to
backslash line breaks."
  (let ((html (replace-regexp-in-string "<div><br></div>" "" html)))
    (setq html (replace-regexp-in-string "<br ?/?>\\(</div>\\)" "\\1" html))
    (setq html (replace-regexp-in-string "<div>" "<p>" html))
    (replace-regexp-in-string "</div>" "</p>" html)))

(defun ekg-apple-notes--from-html (body mode)
  "Convert Apple Notes BODY (HTML) to text in MODE.
MODE should be the symbol `org-mode' or `markdown-mode'."
  (let* ((body (ekg-apple-notes--remove-resource-html body))
         (body (ekg-apple-notes--remove-tags-html body))
         (body (ekg-apple-notes--normalize-divs body))
         (to (pcase mode
               ('org-mode "org")
               ('markdown-mode "markdown")
               (_ "markdown")))
         (text (ekg-apple-notes--pandoc body "html" to)))
    (when (eq mode 'org-mode)
      (setq text (ekg-apple-notes--relink-org text)))
    (string-trim text)))

;;; ---- Export (ekg → Apple Notes) ----

(defun ekg-apple-notes--notes-to-export (since)
  "Return ekg notes modified since SINCE (integer epoch).
Respects `ekg-apple-notes-export-tags' if set."
  (let* ((pred (if (= 0 since) :time-tracked/creation-time :time-tracked/modified-time))
         (rows (triples-db-select-pred-op ekg-db pred '>= since))
         (ids (seq-uniq (mapcar #'car rows)))
         (notes (delq nil (mapcar #'ekg-get-note-with-id ids))))
    (setq notes (seq-filter #'ekg-note-active-p notes))
    (let ((excluded (ekg-apple-notes--exclude-tags)))
      (setq notes (seq-filter
                   (lambda (note)
                     (not (seq-intersection (ekg-note-tags note) excluded)))
                   notes)))
    (when ekg-apple-notes-export-tags
      (setq notes (seq-filter
                   (lambda (note)
                     (seq-intersection (ekg-note-tags note)
                                       ekg-apple-notes-export-tags))
                   notes)))
    notes))

(defun ekg-apple-notes--export-note (note)
  "Export a single ekg NOTE to Apple Notes.
Creates or updates the corresponding Apple Note."
  (let* ((title (ekg-apple-notes--note-title note))
         (body (ekg-apple-notes--to-html note))
         (apple-id (ekg-apple-notes--get-apple-id (ekg-note-id note))))
    (if apple-id
        ;; Update existing note.
        (progn
          (ekg-apple-notes--update-note apple-id body)
          (message "ekg-apple-notes: updated note %s" title))
      ;; Create new note.
      (setq apple-id (ekg-apple-notes--create-note title body))
      (ekg-apple-notes--set-apple-id (ekg-note-id note) apple-id)
      (message "ekg-apple-notes: created note %s" title))))

(defun ekg-apple-notes-export (&optional force)
  "Export modified ekg notes to Apple Notes.
With FORCE (prefix arg), re-export all notes regardless of
modification time."
  (interactive "P")
  (ekg-apple-notes-connect)
  (ekg-apple-notes--ensure-folder)
  (let* ((last-export (if force 0 (ekg-apple-notes--get-last-export)))
         (notes (ekg-apple-notes--notes-to-export last-export))
         (count 0))
    (message "ekg-apple-notes: exporting %d notes modified since %s"
             (length notes)
             (if (= 0 last-export) "the beginning"
               (format-time-string "%F %X" last-export)))
    (dolist (note notes)
      (condition-case err
          (progn
            (ekg-apple-notes--export-note note)
            (cl-incf count))
        (error (message "ekg-apple-notes: failed to export note %S: %s"
                        (ekg-note-id note) (error-message-string err)))))
    (message "ekg-apple-notes: exported %d notes" count)
    (ekg-apple-notes--set-last-export (current-time))))

;;; ---- Import (Apple Notes → ekg) ----

(defun ekg-apple-notes--parse-iso-time (iso-str)
  "Parse ISO-STR (e.g. 2026-02-24T14:30:00) to an integer epoch."
  (floor (float-time (encode-time (iso8601-parse iso-str)))))

(defun ekg-apple-notes--import-note (apple-note)
  "Import a single APPLE-NOTE into ekg.
APPLE-NOTE is an `ekg-apple-notes--note' struct.
Returns non-nil if a note was created or updated."
  (let* ((apple-id (ekg-apple-notes--note-id apple-note))
         (body (ekg-apple-notes--note-body apple-note))
         (ekg-id (ekg-apple-notes--get-ekg-id apple-id))
         (resource (ekg-apple-notes--parse-resource-from-body body))
         (tags (ekg-apple-notes--parse-tags-from-body body))
         (mode ekg-capture-default-mode)
         (text (ekg-apple-notes--from-html body mode)))
    (if ekg-id
        ;; Existing note — only import if modified after our last export.
        (let ((mod-time (ekg-apple-notes--parse-iso-time
                         (ekg-apple-notes--note-modification-date apple-note)))
              (last-export (ekg-apple-notes--get-last-export)))
          (if (<= mod-time last-export)
              nil ;; Modified by our export, not externally.
            (let ((note (ekg-get-note-with-id ekg-id)))
              (when note
                (setf (ekg-note-text note) text)
                (when tags
                  (setf (ekg-note-tags note) tags))
                (ekg-save-note note)
                (message "ekg-apple-notes: updated ekg note from Apple Notes %s"
                         apple-id)
                t))))
      ;; New note from Apple Notes — skip if tags match exclusions.
      (unless (seq-intersection (or tags '("imported"))
                                (ekg-apple-notes--exclude-tags))
        (let ((note (ekg-note-create
                     :text text
                     :mode mode
                     :tags (or tags '("imported"))
                     :id resource)))
          (ekg-save-note note)
          (ekg-apple-notes--set-apple-id (ekg-note-id note) apple-id)
          (message "ekg-apple-notes: imported new note from Apple Notes %s"
                   apple-id)
          t)))))

(defun ekg-apple-notes-import ()
  "Import new and modified notes from Apple Notes into ekg."
  (interactive)
  (ekg-apple-notes-connect)
  (let* ((last-import (ekg-apple-notes--get-last-import))
         (start-time (current-time))
         (apple-notes (ekg-apple-notes--list-notes))
         (count 0))
    (message "ekg-apple-notes: checking %d notes in Apple Notes folder"
             (length apple-notes))
    (dolist (an apple-notes)
      (let ((mod-time (ekg-apple-notes--parse-iso-time
                       (ekg-apple-notes--note-modification-date an))))
        (when (>= mod-time last-import)
          (condition-case err
              (when (ekg-apple-notes--import-note an)
                (cl-incf count))
            (error (message "ekg-apple-notes: failed to import note %s: %s"
                            (ekg-apple-notes--note-id an)
                            (error-message-string err)))))))
    (message "ekg-apple-notes: imported %d notes" count)
    (ekg-apple-notes--set-last-import start-time)))

;;; ---- Sync ----

;;;###autoload

(defun ekg-apple-notes-sync (&optional force)
  "Bidirectional sync between ekg and Apple Notes.
Imports from Apple Notes first, then exports to Apple Notes.
With FORCE (prefix arg), re-export all notes."
  (interactive "P")
  (ekg-apple-notes-connect)
  (ekg-apple-notes-import)
  (ekg-apple-notes-export force))

(provide 'ekg-apple-notes)

;;; ekg-apple-notes.el ends here
