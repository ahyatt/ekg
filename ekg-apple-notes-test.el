;;; ekg-apple-notes-test.el --- Tests for ekg-apple-notes -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the pure functions in ekg-apple-notes: tag conversion,
;; HTML processing, and content round-trips.

;;; Code:

(require 'ert)
(require 'ekg-test-utils)
(require 'ekg-apple-notes)

;;; ---- Tag Conversion Tests ----

(ert-deftest ekg-apple-notes-test-tags-to-metadata ()
  "Tags are converted to one Tag: line per tag."
  (let ((ekg-hidden-tags '("trash" "draft")))
    (should (equal "Tag: food\nTag: recipes"
                   (ekg-apple-notes--tags-to-metadata '("food" "recipes"))))
    ;; Hidden tags are excluded.
    (should (equal "Tag: food"
                   (ekg-apple-notes--tags-to-metadata '("food" "trash"))))))

(ert-deftest ekg-apple-notes-test-tags-to-metadata-empty ()
  "No metadata lines when there are no visible tags."
  (let ((ekg-hidden-tags '("trash")))
    (should-not (ekg-apple-notes--tags-to-metadata '("trash")))))

(ert-deftest ekg-apple-notes-test-tags-to-metadata-spaces ()
  "Tags with spaces are preserved as-is."
  (let ((ekg-hidden-tags nil))
    (should (equal "Tag: my tag"
                   (ekg-apple-notes--tags-to-metadata '("my tag"))))))

(ert-deftest ekg-apple-notes-test-parse-tags-from-body ()
  "Parse Tag: lines from HTML body."
  (should (equal '("food" "date/2024-01-01" "my tag")
                 (ekg-apple-notes--parse-tags-from-body
                  "<div>some content</div>\n<div>Tag: food</div>\n<div>Tag: date/2024-01-01</div>\n<div>Tag: my tag</div>"))))

(ert-deftest ekg-apple-notes-test-parse-tags-plain-text ()
  "Parse Tag: lines from plain text body."
  (should (equal '("food" "recipes")
                 (ekg-apple-notes--parse-tags-from-body
                  "some content\nTag: food\nTag: recipes"))))

(ert-deftest ekg-apple-notes-test-parse-tags-no-line ()
  "Return nil when there are no Tag: lines."
  (should-not (ekg-apple-notes--parse-tags-from-body
               "<div>just content without tags</div>")))

(ert-deftest ekg-apple-notes-test-remove-tags-line ()
  "Remove Tag: lines from text."
  (should (equal "some content"
                 (ekg-apple-notes--remove-tags-line
                  "some content\nTag: food\nTag: recipes"))))

;;; ---- HTML Tag Removal Tests ----

(ert-deftest ekg-apple-notes-test-remove-tags-html ()
  "Tag divs and spacer are removed from HTML."
  (should (equal "<div>content</div>"
                 (string-trim
                  (ekg-apple-notes--remove-tags-html
                   "<div>content</div>\n<div><br></div>\n<div>Tag: food</div>\n<div>Tag: recipes</div>")))))

(ert-deftest ekg-apple-notes-test-remove-tags-html-no-spacer ()
  "Tag divs without spacer are removed."
  (should (equal "<div>content</div>"
                 (string-trim
                  (ekg-apple-notes--remove-tags-html
                   "<div>content</div>\n<div>Tag: food</div>")))))

(ert-deftest ekg-apple-notes-test-remove-tags-html-spaces-in-tag ()
  "Tag divs with spaces in tag names are removed."
  (should (equal "<div>content</div>"
                 (string-trim
                  (ekg-apple-notes--remove-tags-html
                   "<div>content</div>\n<div><br></div>\n<div>Tag: my tag</div>")))))

;;; ---- Resource Parsing Tests ----

(ert-deftest ekg-apple-notes-test-parse-resource-from-body ()
  "Parse Resource: line from HTML body."
  (should (equal "https://example.com/page"
                 (ekg-apple-notes--parse-resource-from-body
                  "<div>Resource: https://example.com/page</div>\n<div><br></div>\n<div>content</div>"))))

(ert-deftest ekg-apple-notes-test-parse-resource-from-body-none ()
  "Return nil when there is no Resource: line."
  (should-not (ekg-apple-notes--parse-resource-from-body
               "<div>just content</div>")))

(ert-deftest ekg-apple-notes-test-remove-resource-html ()
  "Resource div and spacer are removed from HTML."
  (should (equal "<div>content</div>"
                 (string-trim
                  (ekg-apple-notes--remove-resource-html
                   "<div>Resource: https://example.com</div>\n<div><br></div>\n<div>content</div>")))))

(ert-deftest ekg-apple-notes-test-remove-resource-html-no-spacer ()
  "Resource div without spacer is removed."
  (should (equal "<div>content</div>"
                 (string-trim
                  (ekg-apple-notes--remove-resource-html
                   "<div>Resource: https://example.com</div>\n<div>content</div>")))))

(ert-deftest ekg-apple-notes-test-to-html-includes-resource ()
  "Notes with URI IDs include a Resource: line at the top of exported HTML."
  (skip-unless (executable-find "pandoc"))
  (let* ((ekg-hidden-tags nil)
         (note (ekg-note-create :text "hello" :mode 'org-mode
                                :tags '("test")
                                :id "https://example.com/page"))
         (html (ekg-apple-notes--to-html note)))
    (should (string-prefix-p "<div>Resource: https://example.com/page</div>" html))))

(ert-deftest ekg-apple-notes-test-to-html-no-resource-for-numeric-id ()
  "Notes with numeric IDs do not include a Resource: line."
  (skip-unless (executable-find "pandoc"))
  (let* ((ekg-hidden-tags nil)
         (note (ekg-note-create :text "hello" :mode 'org-mode
                                :tags '("test")
                                :id 12345))
         (html (ekg-apple-notes--to-html note)))
    (should-not (string-match-p "Resource:" html))))

;;; ---- HTML Processing Tests ----

(ert-deftest ekg-apple-notes-test-html-delink ()
  "Links are converted to markdown [text](url) format."
  (should (equal "[Example](https://example.com)"
                 (ekg-apple-notes--html-delink
                  "<a href=\"https://example.com\">Example</a>"))))

(ert-deftest ekg-apple-notes-test-html-delink-multiple ()
  "Multiple links are all converted."
  (should (equal "Visit [A](http://a.com) and [B](http://b.com)"
                 (ekg-apple-notes--html-delink
                  "Visit <a href=\"http://a.com\">A</a> and <a href=\"http://b.com\">B</a>"))))

(ert-deftest ekg-apple-notes-test-normalize-divs ()
  "Div wrappers are converted to paragraphs."
  (should (equal "<p>First</p>\n<p>Second</p>"
                 (ekg-apple-notes--normalize-divs
                  "<div>First</div>\n<div>Second</div>"))))

(ert-deftest ekg-apple-notes-test-normalize-divs-br-spacer ()
  "Div-wrapped br spacers are removed."
  (should (equal "<p>First</p>\n\n<p>Second</p>"
                 (ekg-apple-notes--normalize-divs
                  "<div>First</div>\n<div><br></div>\n<div>Second</div>"))))

(ert-deftest ekg-apple-notes-test-normalize-divs-trailing-br ()
  "Trailing br inside divs is stripped to avoid pandoc backslashes."
  (should (equal "<p>First</p>\n<p>Second</p>\n<p>Third</p>"
                 (ekg-apple-notes--normalize-divs
                  "<div>First<br></div>\n<div>Second<br/></div>\n<div>Third<br /></div>"))))

(ert-deftest ekg-apple-notes-test-relink-org ()
  "Markdown links are converted to org links."
  (should (equal "Visit [[https://example.com][Example]]"
                 (ekg-apple-notes--relink-org
                  "Visit [Example](https://example.com)"))))

(ert-deftest ekg-apple-notes-test-relink-org-multiple ()
  "Multiple markdown links are all converted to org links."
  (should (equal "[[http://a.com][A]] and [[http://b.com][B]]"
                 (ekg-apple-notes--relink-org
                  "[A](http://a.com) and [B](http://b.com)"))))

;;; ---- Pandoc Integration Tests ----
;; These tests require pandoc to be installed.

(ert-deftest ekg-apple-notes-test-pandoc-org-to-html ()
  "Org text is converted to HTML."
  (skip-unless (executable-find "pandoc"))
  (let ((html (ekg-apple-notes--pandoc "Some *bold* text" "org" "html")))
    (should (string-match-p "<strong>bold</strong>" html))))

(ert-deftest ekg-apple-notes-test-pandoc-html-to-org ()
  "HTML is converted to org text."
  (skip-unless (executable-find "pandoc"))
  (let ((org (ekg-apple-notes--pandoc "<p>Some <b>bold</b> text</p>" "html" "org")))
    (should (string-match-p "\\*bold\\*" org))))

(ert-deftest ekg-apple-notes-test-pandoc-md-to-html ()
  "Markdown is converted to HTML."
  (skip-unless (executable-find "pandoc"))
  (let ((html (ekg-apple-notes--pandoc "Some **bold** text" "markdown" "html")))
    (should (string-match-p "<strong>bold</strong>" html))))

(ert-deftest ekg-apple-notes-test-pandoc-html-to-md ()
  "HTML is converted to markdown."
  (skip-unless (executable-find "pandoc"))
  (let ((md (ekg-apple-notes--pandoc "<p>Some <b>bold</b> text</p>" "html" "markdown")))
    (should (string-match-p "\\*\\*bold\\*\\*" md))))

;;; ---- ISO Time Parsing ----

(ert-deftest ekg-apple-notes-test-parse-iso-time ()
  "ISO 8601 timestamps are parsed to epoch integers."
  (let ((epoch (ekg-apple-notes--parse-iso-time "2026-02-24T14:30:00")))
    (should (integerp epoch))
    (should (> epoch 0))))

(provide 'ekg-apple-notes-test)

;;; ekg-apple-notes-test.el ends here
