;;; ekg-llm-test.el --- Tests for ekg-llm  -*- lexical-binding: t; -*-

;; Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
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

;; These tests should be run and pass before every commit to ekg-llm.

;;; Code:
(require 'ekg)
(require 'ekg-llm)
(require 'ekg-test-utils)

(ekg-deftest ekg-llm-test-interaction-func-append ()
  (ekg-capture :text "Initial text\n" :mode 'text-mode)
  (funcall (ekg-llm-interaction-func 'append) "Appended text")
  (should (equal "Initial text\n\nBEGIN_LLM_OUTPUT\nAppended text\nEND_LLM_OUTPUT\n"
                 (substring-no-properties (ekg-edit-note-display-text)))))

(ekg-deftest ekg-llm-test-interaction-func-replace ()
  (ekg-capture :text "Initial text\n" :mode 'text-mode)
  (funcall (ekg-llm-interaction-func 'replace) "Replaced text")
  (should (equal "Replaced text"
                 (substring-no-properties (ekg-edit-note-display-text)))))

(provide 'ekg-llm-test)

;;; ekg-llm-test.el ends here
