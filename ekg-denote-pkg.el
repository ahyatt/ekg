;;; ekg-denote-pkg.el --- Denote integration for ekg.  -*- no-byte-compile: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;; This defines the package for ekg-denote.

;;; Code:

(define-package "ekg-denote" "1.0.0"
  "Denote integration for ekg."
  '((ekg "20260305")(denote "4.0.0")))

(provide 'ekg-denote-pkg)
;;; ekg-denote-pkg.el ends here
