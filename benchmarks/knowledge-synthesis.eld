(
 :group "knowledge synthesis"
 :setup
 (progn
;; Store several notes with different facts that must be combined.
(ekg-agent-add-note
  "Project Mercury uses Python 3.11 and is deployed on AWS Lambda.
   The main entry point is handler.py in the mercury-service repo."
  '("mercury" "infrastructure") "org-mode")
(ekg-agent-add-note
  "Project Mercury's API rate limit is 1000 requests per minute.
   When the limit is exceeded, it returns HTTP 429."
  '("mercury" "api") "org-mode")
(ekg-agent-add-note
  "The Mercury team consists of: Alice (tech lead), Bob (backend),
   and Carol (devops). Alice is on vacation until March 20."
  '("mercury" "team") "org-mode")
)
 :tasks
 (
  (
   :name "answer-from-multiple-notes"
   :description "Ask a question that requires combining information from several notes. The agent must search ekg, find the relevant notes, and synthesize an answer.
"
   :trigger
   (progn
(ekg-agent-ask
  "I need a quick briefing on Project Mercury: what tech stack does it
   use, what are the API limits, and who should I contact right now
   if I have a backend question? Check my notes for this info.")
)
   :verify-task
   (progn
;; The popup result should mention key facts from all three notes.
(and (get-buffer "*ekg agent result*")
     (with-current-buffer "*ekg agent result*"
       (let ((content (downcase (buffer-string))))
         (and
           ;; Tech stack.
           (string-match-p "python" content)
           ;; API limit (may be formatted as "1000" or "1,000").
           (string-match-p "1[,.]?000" content)
           ;; Contact: should suggest Bob since Alice is on vacation.
           (string-match-p "bob" content)))))
)
   :verify-memory
   (progn
;; Agent should have documented the briefing task.
(let* ((ekg-hidden-tags nil)
       (all-notes (ekg-get-notes-with-tag "agent")))
  (and (> (length all-notes) 0)
       (cl-some (lambda (n)
                  (let ((text (downcase (ekg-note-text n))))
                    (or (string-match-p "mercury\\|briefing" text)
                        (string-match-p "tech.*stack\\|api.*limit\\|team" text))))
                all-notes)))
)
   :max-iterations 15
   :timeout 120
  )
 )
)
