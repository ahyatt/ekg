;;; ekg-denote-test.el --- Tests for ekg-denote  -*- lexical-binding: t; -*-

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

;; These tests should be run and pass before every commit.

;;; Code:

(require 'ert)
(require 'ekg-test-utils)
(require 'ekg-denote)
(require 'cl-lib)

(ert-deftest ekg-denote-test-export--sublist-kws ()
  (should (equal '("kw1") (ekg-denote-export--sublist-kws '("kw1" "kw2" "kw 3") 4)))
  (should (equal '("kw1" "kw2") (ekg-denote-export--sublist-kws '("kw1" "kw2" "kw 3") 7)))
  (should (equal '("kw1" "kw2" "kw 3") (ekg-denote-export--sublist-kws '("kw1" "kw2" "kw 3") 12))))


