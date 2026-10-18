;;; run.el --- Isolated experiment runner -*- lexical-binding: t; -*-

(require 'cl-lib)
(let ((root (expand-file-name "../.." (file-name-directory load-file-name))))
  (dolist (dir (directory-files
                (expand-file-name ".eldev/30.1/packages" root) t "^[^.]"))
    (when (file-directory-p dir) (add-to-list 'load-path dir)))
  (dolist (dir (directory-files "/workspace/.cloud-setup/deps" t "^[^.]"))
    (when (file-directory-p dir) (add-to-list 'load-path dir)))
  (add-to-list 'load-path root)
  (setq load-prefer-newer t)
  (require 'markdown-mode)
  (load (expand-file-name "ekg-org-test.el" root) nil t)
  (unless (getenv "EKG_EXPERIMENT_BASELINE")
    (load (expand-file-name "experiments/new-item/prototypes.el" root) nil t)
    (load (expand-file-name "experiments/new-item/trials.el" root) nil t)))
(ert-run-tests-batch-and-exit
 (if (getenv "EKG_EXPERIMENT_BASELINE") "^ekg-org-test-" "^ekg-experiment-"))
