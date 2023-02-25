;;; ekg-logseq.el --- ekg and logseq integration -*- lexical-binding: t -*-

;; Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>

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
;; This exports data to logseq. Because ekg and logseq have somewhat different
;; properties, the mapping process needs to be described. First, because notes
;; in ekg have no titles, they should not be separate logseq notes. Instead, a
;; tag is a logseq note, and contains all notes within it. This means that notes
;; are expected to be duplicated.
;;
;; One potential way around this is to have shared org-mode bullet-points that
;; have ids, that can be included in other logseq documents. However, that won't
;; work nicely because there is no natural text you can put on that line.
;; Additionally, notes may not be org-mode notes.

(defgroup ekg-logseq nil
  "Customization for ekg's logseq integration."
  :group 'ekg)

(defcustom ekg-logseq-dir nil
  :type 'directory
  :group 'ekg-logseq
  :paren 'ekg)

(defun ekg-logseq-export ()
  (interactive)
  )
