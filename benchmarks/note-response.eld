(
 :group "note response"
 :tasks
 (
  (
   :name "note-response-elaboration"
   :description "Create a note with a factual statement, then trigger the note-response agent from a notes buffer.  The agent should save an elaboration as a child response note.
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
  (ekg-show-notes-with-all-tags (ekg-note-tags note))
  (ekg-agent-notes-respond "Elaborate on this note" nil))
)
   :verify-task
   (progn
(let* ((note (seq-find
              (lambda (candidate)
                (string-match-p "Turing machine"
                                (ekg-note-text candidate)))
              (ekg-get-notes-with-tags
               '("history" "computer-science"))))
       (response (car (ekg-note-child-notes note))))
  (and response
       (> (length (ekg-note-text response)) 50)))
)
   :max-iterations 10
   :timeout 120
  )
  (
   :name "note-response-with-context"
   :description "Create notes with related context, then trigger note-response on a selected question in a notes buffer.  The agent should save a response that incorporates information from related notes.
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
  (ekg-show-notes-with-all-tags '("computer-science" "question"))
  (ekg-agent-notes-respond "Answer the question" nil))
)
   :verify-task
   (progn
(let* ((note (seq-find
              (lambda (candidate)
                (string-match-p "connection between lambda calculus"
                                (ekg-note-text candidate)))
              (ekg-get-notes-with-tags
               '("computer-science" "question"))))
       (response (car (ekg-note-child-notes note)))
       (content (and response (downcase (ekg-note-text response)))))
  (and content
       (or (string-match-p "church" content)
           (string-match-p "functional" content)
           (string-match-p "haskell" content))))
)
   :max-iterations 10
   :timeout 120
  )
  (
   :name "note-response-rewrite"
   :description "Create a note with a draft and ask for a rewrite from a notes buffer. The agent should save the rewrite as a child response note.
"
   :trigger
   (progn
(let* ((note (ekg-note-create
               :text "Please rewrite this: the cat sitted on the mat and eated the food"
               :mode 'org-mode
               :tags '("writing" "draft"))))
  (ekg-save-note note)
  (ekg-show-notes-with-all-tags '("writing" "draft"))
  (ekg-agent-notes-respond "Rewrite this sentence correctly" nil))
)
   :verify-task
   (progn
(let* ((note (seq-find
              (lambda (candidate)
                (string-match-p "the cat sitted"
                                (ekg-note-text candidate)))
              (ekg-get-notes-with-tags '("writing" "draft"))))
       (response (car (ekg-note-child-notes note)))
       (content (and response (downcase (ekg-note-text response)))))
  (and content
       (not (string-match-p "sitted" content))
       (not (string-match-p "eated" content))
       (string-match-p "cat" content)))
)
   :max-iterations 10
   :timeout 120
  )
 )
)
