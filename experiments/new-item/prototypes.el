;;; prototypes.el --- New-item interaction experiments -*- lexical-binding: t; -*-

;; These opt-in prototypes do not change EKG's installed keybindings.
(require 'ekg-org)

(defvar-local ekg-experiment--title nil
  "Title waiting for placement in the title-first prototype.")

(defun ekg-experiment--slot (relationship)
  "Return a creation slot for RELATIONSHIP to the task at point."
  (let* ((id (ekg-org-view--note-at-point))
         (note (and id (ekg-get-note-with-id id))))
    (pcase relationship
      ('top-level
       (list :parent-id nil
             :after-id (when-let* ((roots (ekg-org-view--sorted-top-level)))
                         (ekg-note-id (car (last roots))))))
      ('child
       (unless note (user-error "Select a parent task"))
       (list :parent-id id
             :after-id (when-let* ((children
                                   (ekg-org-view--sorted-children id)))
                         (ekg-note-id (car (last children))))))
      ('sibling
       (list :parent-id (and note (ekg-note-parent-id note)) :after-id id))
      (_ (user-error "Unknown relationship")))))

(defun ekg-experiment--create (slot title)
  "Create TITLE in SLOT, ignoring blank input."
  (unless (string-empty-p (string-trim title))
    (when-let* ((parent (plist-get slot :parent-id)))
      (ekg-notes--unfold-ids (list parent)))
    (ekg-org-view--insert-create-task slot title)))

(defun ekg-experiment--create-top-level (title)
  "Create TITLE at the end of the root list, showing the global view."
  (unless (string-empty-p (string-trim title))
    (let ((slot (ekg-experiment--slot 'top-level)))
      ;; A new root would be invisible in a view scoped to another task.
      ;; Navigate only after title entry succeeds, so C-g preserves context.
      (when ekg-org-view--root-id (ekg-org-view))
      (ekg-experiment--create slot title))))

(defun ekg-experiment-top-level ()
  "Create a top-level task from any depth, without selecting a parent."
  (interactive)
  (when ekg-org-view--archive (user-error "Use the active task view"))
  (ekg-experiment--create-top-level (read-string "New top-level title: ")))

(defun ekg-experiment-sibling ()
  "Prompt immediately for a sibling after the current subtree."
  (interactive)
  (when ekg-org-view--archive (user-error "Use the active task view"))
  (let ((slot (ekg-experiment--slot 'sibling)))
    (ekg-experiment--create slot (read-string "New sibling title: "))))

(defun ekg-experiment-child ()
  "Prompt immediately for a child, using the existing + command."
  (interactive)
  (when ekg-org-view--archive (user-error "Use the active task view"))
  (ekg-org-view-add-child (read-string "New child title: ")))

(defun ekg-experiment-chooser ()
  "Choose a relationship explicitly, then enter a title."
  (interactive)
  (when ekg-org-view--archive (user-error "Use the active task view"))
  (let* ((relationship
          (intern (completing-read "Create as: "
                                   '("sibling" "child" "top-level") nil t)))
         (slot (ekg-experiment--slot relationship)))
    (if (eq relationship 'top-level)
        (ekg-experiment--create-top-level (read-string "New top-level title: "))
      (ekg-experiment--create slot (read-string "Task title: ")))))

(defun ekg-experiment-title-first ()
  "Enter a title first, then use the existing visual placement mode.
RET saves the entered title; C-g cancels without writing a task."
  (interactive)
  (when ekg-org-view--archive (user-error "Use the active task view"))
  (let ((title (read-string "Task title: ")))
    (unless (string-empty-p (string-trim title))
      (vui-flush-sync)
      (let ((slots (ekg-org-view--build-insert-slots)))
        (if (null slots)
            (ekg-experiment--create '(:parent-id nil :after-id nil) title)
          (ekg-org-view-create)
          (setq-local ekg-experiment--title title)
          (let ((map (copy-keymap ekg-org-view-insert-mode-map)))
            (define-key map (kbd "RET") #'ekg-experiment-title-confirm)
            (setq overriding-local-map map))
          (ekg-experiment--show-title)
          (message "Place task: n/p move, R/L depth, RET save, C-g cancel"))))))

(defun ekg-experiment--show-title (&rest _)
  "Display the entered title in the visual placement preview."
  (when (and ekg-experiment--title ekg-org-view--insert-overlay)
    (let ((slot (nth ekg-org-view--insert-index ekg-org-view--insert-slots)))
      (ekg-org-view--insert-update-overlay
       ekg-org-view--insert-overlay (plist-get slot :buffer-pos)
       (plist-get slot :level) (ekg-org-view--insert-slot-spacing slot)
       ekg-experiment--title))))

(advice-add 'ekg-org-view--insert-show :after #'ekg-experiment--show-title)

(defun ekg-experiment--clear-title (&rest _)
  "Discard the experimental title when placement ends."
  (setq ekg-experiment--title nil))

(advice-add 'ekg-org-view--insert-cleanup :after #'ekg-experiment--clear-title)

(defun ekg-experiment-title-confirm ()
  "Save the title-first prototype at the current placement."
  (interactive)
  (let ((slot (nth ekg-org-view--insert-index ekg-org-view--insert-slots))
        (title ekg-experiment--title))
    (setq ekg-experiment--title nil)
    (ekg-org-view--insert-cleanup)
    (ekg-experiment--create slot title)))

(provide 'ekg-new-item-prototypes)
;;; prototypes.el ends here
