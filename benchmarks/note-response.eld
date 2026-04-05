(
 :group "note response"
 :tasks
 (
  (
   :name "note-response-append-elaboration"
   :description "Create a note with a factual statement, then trigger the note-response agent.  The agent should append an elaboration using append_to_current_note.  Verifies the agent terminates and the buffer contains the appended response.
"
   :setup
   (progn
(ekg-agent-add-note
  "The Turing machine was proposed in 1936 by Alan Turing."
  '("history" "computer-science") "org-mode")
)
   :trigger
   (progn
(let* ((notes (ekg-get-notes-with-tags '("history" "computer-science")))
       (note (car notes)))
  (ekg-edit note)
  ;; Wait for the buffer to be ready.
  (sit-for 0.5)
  (ekg-agent-note-response))
)
   :verify-task
   (progn
;; The edit buffer should contain appended content beyond the
;; original note text (~55 chars).
(let ((buf (car (seq-filter
                  (lambda (b)
                    (string-match-p "\\*EKG Edit:" (buffer-name b)))
                  (buffer-list)))))
  (and buf
       (with-current-buffer buf
         (let ((content (buffer-string)))
           (and
             ;; Original content still present.
             (string-match-p "Turing" content)
             ;; Buffer has meaningful appended content.
             (> (length content) 150))))))
)
   :max-iterations 10
   :timeout 120
  )
  (
   :name "note-response-append-with-context"
   :description "Create notes with related context, then trigger note-response on a note that asks a question.  The agent should search for context and append a response that incorporates information from related notes.
"
   :setup
   (progn
(ekg-agent-add-note
  "Lambda calculus was invented by Alonzo Church in the 1930s.
   It is the theoretical foundation of functional programming."
  '("history" "computer-science" "lambda-calculus") "org-mode")
(ekg-agent-add-note
  "Haskell is a purely functional programming language named after
   Haskell Curry.  It was first released in 1990."
  '("history" "computer-science" "haskell") "org-mode")
)
   :trigger
   (progn
(let* ((note (ekg-note-create
               :text "What is the connection between lambda calculus and Haskell?"
               :mode 'org-mode
               :tags '("computer-science" "question"))))
  (ekg-save-note note)
  (ekg-edit (ekg-get-note-with-id (ekg-note-id note)))
  (sit-for 0.5)
  (ekg-agent-note-response))
)
   :verify-task
   (progn
;; The edit buffer should have the question plus an appended answer.
(let ((buf (car (seq-filter
                  (lambda (b)
                    (string-match-p "\\*EKG Edit:" (buffer-name b)))
                  (buffer-list)))))
  (and buf
       (with-current-buffer buf
         (let ((content (downcase (buffer-string))))
           (and
             ;; Original question present.
             (string-match-p "lambda calculus" content)
             ;; Response mentions something relevant.
             (or (string-match-p "church" content)
                 (string-match-p "functional" content)
                 (string-match-p "haskell" content))
             ;; Buffer grew beyond just the question.
             (> (length content) 100))))))
)
   :max-iterations 10
   :timeout 120
  )
  (
   :name "note-response-replace"
   :description "Create a note with a draft and instructions asking for a rewrite. The agent should use replace_current_note to rewrite the content.
"
   :trigger
   (progn
(let* ((note (ekg-note-create
               :text "Please rewrite this: the cat sitted on the mat and eated the food"
               :mode 'org-mode
               :tags '("writing" "draft"))))
  (ekg-save-note note)
  (ekg-edit (ekg-get-note-with-id (ekg-note-id note)))
  (sit-for 0.5)
  (ekg-agent-note-response))
)
   :verify-task
   (progn
;; The buffer should have rewritten content without the errors.
(let ((buf (car (seq-filter
                  (lambda (b)
                    (string-match-p "\\*EKG Edit:" (buffer-name b)))
                  (buffer-list)))))
  (and buf
       (with-current-buffer buf
         (let ((content (downcase (buffer-string))))
           (and
             ;; Should NOT contain the original errors.
             (not (string-match-p "sitted" content))
             (not (string-match-p "eated" content))
             ;; Should contain corrected version with cat.
             (string-match-p "cat" content))))))
)
   :max-iterations 10
   :timeout 120
  )
 )
)
