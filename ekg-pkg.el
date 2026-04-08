;;; ekg-pkg.el --- Package definition for ekg -*- no-byte-compile: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary: This defines the package for ekg.
;;; Code:

(define-package "ekg" "0.8.0"
  "A sqlite-based note-taking package based on tags."
  '((triples "0.6.1")
    (emacs   "28.1")
    (llm "0.18.0")
    (vui "1.0.0")))

(provide 'ekg-pkg)
;;; ekg-pkg.el ends here
