;;; ekg-apple-notes-test.el --- Tests for ekg-apple-notes -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the pure functions in ekg-apple-notes: tag conversion,
;; HTML processing, and content round-trips.

;;; Code:

(require 'ert)
(require 'ekg-test-utils)
(require 'ekg-apple-notes)

;;; ---- Tag Conversion Tests ----

(ert-deftest ekg-apple-notes-test-tag-simple ()
  "Simple tags pass through unchanged."
  (should (equal "food" (ekg-apple-notes--tag-to-hashtag "food"))))

(ert-deftest ekg-apple-notes-test-tag-with-slash ()
  "Tags with slashes (prefixes) are preserved."
  (should (equal "date/2024-01-01"
                 (ekg-apple-notes--tag-to-hashtag "date/2024-01-01"))))

(ert-deftest ekg-apple-notes-test-tag-with-spaces ()
  "Spaces in tags are replaced with underscores."
  (should (equal "my_tag" (ekg-apple-notes--tag-to-hashtag "my tag"))))

(ert-deftest ekg-apple-notes-test-tag-with-dots ()
  "Dots in tags are replaced with underscores."
  (should (equal "v1_0" (ekg-apple-notes--tag-to-hashtag "v1.0"))))

(ert-deftest ekg-apple-notes-test-tag-with-colons ()
  "Colons in tags are replaced with underscores."
  (should (equal "key_value" (ekg-apple-notes--tag-to-hashtag "key:value"))))

(ert-deftest ekg-apple-notes-test-tag-consecutive-invalid ()
  "Consecutive invalid characters collapse to a single underscore."
  (should (equal "a_b" (ekg-apple-notes--tag-to-hashtag "a . b"))))

(ert-deftest ekg-apple-notes-test-tag-unicode ()
  "Unicode letters are preserved in tags."
  (should (equal "café" (ekg-apple-notes--tag-to-hashtag "café"))))

(ert-deftest ekg-apple-notes-test-tags-to-metadata ()
  "Tags are converted to a metadata line with hashtags."
  (let ((ekg-hidden-tags '("trash" "draft")))
    (should (equal "Tags: #food #recipes"
                   (ekg-apple-notes--tags-to-metadata '("food" "recipes"))))
    ;; Hidden tags are excluded.
    (should (equal "Tags: #food"
                   (ekg-apple-notes--tags-to-metadata '("food" "trash"))))))

(ert-deftest ekg-apple-notes-test-tags-to-metadata-empty ()
  "No metadata line when there are no visible tags."
  (let ((ekg-hidden-tags '("trash")))
    (should-not (ekg-apple-notes--tags-to-metadata '("trash")))))

(ert-deftest ekg-apple-notes-test-parse-tags-from-body ()
  "Parse hashtags from a Tags: line in HTML body."
  (should (equal '("food" "date/2024-01-01" "my_tag")
                 (ekg-apple-notes--parse-tags-from-body
                  "<div>some content</div>\n<div>Tags: #food #date/2024-01-01 #my_tag</div>"))))

(ert-deftest ekg-apple-notes-test-parse-tags-no-line ()
  "Return nil when there is no Tags: line."
  (should-not (ekg-apple-notes--parse-tags-from-body
               "<div>just content without tags</div>")))

(ert-deftest ekg-apple-notes-test-remove-tags-line ()
  "Remove the Tags: metadata line from text."
  (should (equal "some content"
                 (ekg-apple-notes--remove-tags-line
                  "some content\nTags: #food #recipes"))))

;;; ---- HTML Tag Removal Tests ----

(ert-deftest ekg-apple-notes-test-remove-tags-html ()
  "Tags div and spacer are removed from HTML."
  (should (equal "<div>content</div>"
                 (string-trim
                  (ekg-apple-notes--remove-tags-html
                   "<div>content</div>\n<div><br></div>\n<div>Tags: #food #recipes</div>")))))

(ert-deftest ekg-apple-notes-test-remove-tags-html-no-spacer ()
  "Tags div without spacer is removed."
  (should (equal "<div>content</div>"
                 (string-trim
                  (ekg-apple-notes--remove-tags-html
                   "<div>content</div>\n<div>Tags: #food</div>")))))

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
