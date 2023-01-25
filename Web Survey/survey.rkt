#lang plait
;(require racket/match)
(require (typed-in "backend.rkt"
                   [read/number : (String (Listof String) Boolean -> (Listof String))]
                   [display/web : ((Listof (Listof String)) -> Void)]))

(print-only-errors #t)


(define-type Question
  (question [que : String] [answers : (Listof String)] [provide-own-answer : Boolean]))

(define question-1 (question "Which Star Wars trilogy is the best?"
                             (list "The Original" "The Prequel" "The Sequel") #f))
(define question-2 (question
                    "I’m amazed (and delighted!) at your choice. What about it bothers you the least?"
                             (list "the words the actors say" "how they say them" "something else") #f))
(define question-3 (question "Whose arc do you find more satisfying?"
                             (list "Anakin Skywalker" "Luke Skywalker") #f))
(define question-4 (question "What was the root cause of Anakin’s downfall?"
                             (list "his abilities" "his pride in his abilities"
                                   "his relationship with Padme" "his attachment to temporary things") #t))
(define question-5 (question "Is Luke defined more by his hopefulness or his loyalty?"
                             (list "his hopefulness" "his loyalty") #f))
(define question-6 (question
               "What in-universe explanation for Luke’s loss of hope in the sequel trilogy is most plausible?"
                             (list "the pressure of building a new Jedi Order"
                                   "unresolved fears about the Dark Side"
                                   "disillusionment with the history of the Jedi Order"
                                   "new knowledge about midichlorians") #t))
(define question-7 (question "To whom did Luke owe the most loyalty?"
                             (list "his father" "Obi Wan" "Han and Leia" "Yoda") #t))


; displays a question, reads the answer, and returns the item with the question and answer
(ask-question : (Question -> (Listof String)))
(define (ask-question q)
  (read/number (question-que q) (question-answers q) (question-provide-own-answer q)))


; these functions ask the function specified
(ask-question-1 : ((Listof (Listof String)) -> (Listof (Listof String))))
(define (ask-question-1 responses)
  (let ([r (ask-question question-1)])
    (let ([responses (cons r responses)])
           (if (equal? (list-ref r 1) "The Prequel")
               (ask-question-2 responses) (ask-question-3 responses)))))

(ask-question-2 : ((Listof (Listof String)) -> (Listof (Listof String))))
(define (ask-question-2 responses)
  (let ([r (ask-question question-2)])
    (let ([responses (cons r responses)]) (ask-question-3 responses))))

(ask-question-3 : ((Listof (Listof String)) -> (Listof (Listof String))))
(define (ask-question-3 responses)
  (let ([r (ask-question question-3)])
    (let ([responses (cons r responses)])
           (if (equal? (list-ref r 1) "Anakin Skywalker")
               (ask-question-4 responses) (ask-question-5 responses)))))

(ask-question-4 : ((Listof (Listof String)) -> (Listof (Listof String))))
(define (ask-question-4 responses)
  (let ([r (ask-question question-4)])
    (cons r responses)))

(ask-question-5 : ((Listof (Listof String)) -> (Listof (Listof String))))
(define (ask-question-5 responses)
  (let ([r (ask-question question-5)])
    (let ([responses (cons r responses)])
           (if (equal? (list-ref r 1) "his hopefulness")
               (ask-question-6 responses) (ask-question-7 responses)))))

(ask-question-6 : ((Listof (Listof String)) -> (Listof (Listof String))))
(define (ask-question-6 responses)
  (let ([r (ask-question question-6)])
    (cons r responses)))

(ask-question-7 : ((Listof (Listof String)) -> (Listof (Listof String))))
(define (ask-question-7 responses)
  (let ([r (ask-question question-7)])
    (cons r responses)))


(run-survey : ( -> (Listof (Listof String))))
(define (run-survey)
  (ask-question-1 empty))

