;;; trials.el --- Scripted new-item trials -*- lexical-binding: t; -*-

(defun ekg-experiment--setup ()
  "Build and select a nested task fixture."
  (ekg-org-add-schema)
  (let ((parent (ekg-org-test--add-task "Project")))
    (ekg-org-test--add-task "Existing child" parent))
  (ekg-org-test--add-task "Next project")
  (ekg-org-view)
  (set-buffer "*ekg-org-tasks*")
  (goto-char (point-min)))

(defun ekg-experiment--run (approach relationship title)
  "Run APPROACH with scripted RELATIONSHIP and TITLE input."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) title))
            ((symbol-function 'completing-read)
             (lambda (&rest _) (symbol-name relationship))))
    (pcase approach
      ('direct (if (eq relationship 'child)
                   (ekg-experiment-child)
                 (ekg-experiment-sibling)))
      ('chooser (ekg-experiment-chooser))
      ('title-first
       (ekg-experiment-title-first)
       (when ekg-org-view--insert-overlay
         (let* ((id (ekg-org-test--task-id-by-title "Project"))
                (index
                 (cl-position-if
                  (lambda (slot)
                    (if (eq relationship 'child)
                        (and (equal (plist-get slot :parent-id) id)
                             (equal (plist-get slot :after-id)
                                    (ekg-org-test--task-id-by-title
                                     "Existing child")))
                      (and (null (plist-get slot :parent-id))
                           (equal (plist-get slot :after-id) id))))
                  ekg-org-view--insert-slots)))
           (should index)
           (setq ekg-org-view--insert-index index)
           (ekg-org-view--insert-show)
           (ekg-experiment-title-confirm)))))))

(dolist (approach '(direct chooser title-first))
  (dolist (relationship '(sibling child))
    (eval
     `(ekg-deftest-with-db
          ,(intern (format "ekg-experiment-%s-%s" approach relationship)) ()
        (ekg-experiment--setup)
        (ekg-experiment--run ',approach ',relationship "New task")
        (should
         (equal (ekg-org-test--view-headings)
                ',(if (eq relationship 'sibling)
                      '((1 "Project") (2 "Existing child")
                        (1 "New task") (1 "Next project"))
                    '((1 "Project") (2 "Existing child")
                      (2 "New task") (1 "Next project")))))) t))
  (eval
   `(ekg-deftest-with-db
        ,(intern (format "ekg-experiment-%s-blank" approach)) ()
      (ekg-experiment--setup)
      (ekg-experiment--run ',approach 'sibling "   ")
      (should (= 3 (length (ekg-get-notes-with-tag ekg-org-task-tag))))
      (should-not ekg-org-view--insert-overlay)) t)
  (eval
   `(ekg-deftest-with-db
        ,(intern (format "ekg-experiment-%s-cancel" approach)) ()
      (ekg-experiment--setup)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (signal 'quit nil))))
        (condition-case nil
            (pcase ',approach
              ('direct (ekg-experiment-sibling))
              ('chooser
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "sibling")))
                 (ekg-experiment-chooser)))
              ('title-first (ekg-experiment-title-first)))
          (quit nil)))
      (should (= 3 (length (ekg-get-notes-with-tag ekg-org-task-tag))))
      (should-not ekg-org-view--insert-overlay)
      (should-not overriding-local-map)) t))

(ekg-deftest-with-db ekg-experiment-baseline-typing-trap ()
  (ekg-experiment--setup)
  (ekg-org-view-create)
  (unwind-protect
      (progn
        (should (string-match-p
                 "type task title"
                 (ekg-org-view--insert-placeholder-string 1)))
        (should (eq (lookup-key overriding-local-map "n")
                    'ekg-org-view-insert-next))
        (should (= 3 (length (ekg-get-notes-with-tag ekg-org-task-tag)))))
    (ekg-org-view--insert-cleanup)))

(ekg-deftest-with-db ekg-experiment-title-first-cancel-placement ()
  (ekg-experiment--setup)
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Unsaved")))
    (ekg-experiment-title-first))
  (should ekg-org-view--insert-overlay)
  (ekg-org-view-insert-cancel)
  (should (= 3 (length (ekg-get-notes-with-tag ekg-org-task-tag))))
  (should-not overriding-local-map))

(dolist (approach '(direct chooser title-first))
  (eval
   `(ekg-deftest-with-db
        ,(intern (format "ekg-experiment-%s-empty" approach)) ()
      (ekg-org-add-schema)
      (ekg-org-view)
      (set-buffer "*ekg-org-tasks*")
      (ekg-experiment--run ',approach 'sibling "First")
      (should (equal (ekg-org-test--view-headings) '((1 "First"))))) t)
  (eval
   `(ekg-deftest-with-db
        ,(intern (format "ekg-experiment-%s-archive" approach)) ()
      (ekg-experiment--setup)
      (setq ekg-org-view--archive t)
      (should-error (ekg-experiment--run ',approach 'sibling "Forbidden")
                    :type 'user-error)
      (should (= 3 (length (ekg-get-notes-with-tag ekg-org-task-tag))))) t)
  (eval
   `(ekg-deftest-with-db
        ,(intern (format "ekg-experiment-%s-nested-sibling" approach)) ()
      (ekg-experiment--setup)
      (ekg-org-view--goto-note-id
       (ekg-org-test--task-id-by-title "Existing child"))
      (if (eq ',approach 'title-first)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) "Nested sibling")))
            (ekg-experiment-title-first)
            (setq ekg-org-view--insert-index
                  (cl-position-if
                   (lambda (slot)
                     (equal (plist-get slot :after-id)
                            (ekg-org-test--task-id-by-title "Existing child")))
                   ekg-org-view--insert-slots))
            (ekg-experiment-title-confirm))
        (ekg-experiment--run ',approach 'sibling "Nested sibling"))
      (should (equal (ekg-org-test--view-headings)
                     '((1 "Project") (2 "Existing child")
                       (2 "Nested sibling") (1 "Next project"))))) t))

(ekg-deftest-with-db ekg-experiment-baseline-nonvisual-slot-order ()
  (ekg-experiment--setup)
  (let ((positions (mapcar (lambda (slot) (plist-get slot :buffer-pos))
                           (ekg-org-view--build-insert-slots))))
    ;; Confirms n/p enumerate logical slots rather than screen positions.
    (should (cl-loop for (a b) on positions while b thereis (> a b)))))

(dolist (approach '(direct chooser))
  (dolist (scenario '(nested focused empty blank cancel archive))
    (eval
     `(ekg-deftest-with-db
          ,(intern (format "ekg-experiment-%s-top-level-%s"
                           approach scenario)) ()
        (ekg-experiment--setup)
        (let* ((parent (ekg-org-test--task-id-by-title "Project"))
               (child (ekg-org-test--task-id-by-title "Existing child"))
               (grandchild (ekg-org-test--add-task "Grandchild" child)))
          (ekg-org-view--refresh grandchild)
          (when (eq ',scenario 'focused)
            (ekg-org-view-task parent)
            (ekg-org-view--goto-note-id grandchild))
          (when (eq ',scenario 'empty)
            ;; Archive fixture tasks to get an empty active root list.
            (ekg-org-view--archive-note (ekg-get-note-with-id parent))
            (ekg-org-view--archive-note
             (ekg-get-note-with-id
              (ekg-org-test--task-id-by-title "Next project")))
            (ekg-org-view--refresh))
          (when (eq ',scenario 'archive)
            (setq ekg-org-view--archive t))
          (let ((before (length (ekg-get-notes-with-tag ekg-org-task-tag)))
                (original-point (point)))
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _)
                         (pcase ',scenario
                           ('cancel (signal 'quit nil))
                           ('blank "   ")
                           (_ "New root"))))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) "top-level")))
              (cl-flet ((run ()
                          (if (eq ',approach 'direct)
                              (ekg-experiment-top-level)
                            (ekg-experiment-chooser))))
                (pcase ',scenario
                  ('archive (should-error (run) :type 'user-error))
                  ('cancel (condition-case nil (run) (quit nil)))
                  (_ (run)))))
            (if (memq ',scenario '(cancel blank archive))
                (progn
                  (should (= before (length
                                     (ekg-get-notes-with-tag ekg-org-task-tag))))
                  (should (= original-point (point))))
              (let* ((id (ekg-org-test--task-id-by-title "New root"))
                     (note (ekg-get-note-with-id id)))
                (should-not (ekg-note-parent-id note))
                (should-not ekg-org-view--root-id)
                (should (equal id (ekg-org-view--note-at-point)))
                (should (equal id (ekg-note-id
                                  (car (last
                                        (ekg-org-view--sorted-top-level))))))
                (should (equal (car (last (ekg-org-test--view-headings)))
                               '(1 "New root")))))))) t)))

;;; trials.el ends here
