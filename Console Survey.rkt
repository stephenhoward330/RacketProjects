#lang plait

(print-only-errors #t)

(define-type Item
  (item [question : String] [response : String]))

(for-each : (('a -> Void) (Listof 'a) -> Void))
(define (for-each f xs)
  (type-case (Listof 'a) xs
    [empty (void)]
    [(cons x xs)
     (begin (f x) (for-each f xs))]))



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


; displays an item
(display-item : (Item -> Void))
(define (display-item i)
  (begin (display (item-question i))
         (display "\n                     ")
         (display (item-response i))
         (display "\n")))

; displays the options for a question
(display-options : ((Listof String) Number Boolean -> Void))
(define (display-options los n p)
  (type-case (Listof String) los
    [empty (if p
               (display (string-append "  " (string-append (string-append (to-string n)
                                                    (string-append ". " "other:______")) "\n")))
               (void))]
    [(cons s los)
     (begin (display (string-append "  " (string-append (string-append (to-string n)
                                                                       (string-append ". " s)) "\n")))
            (display-options los (add1 n) p))]))

; displays a question, reads the answer, and returns the item with the question and answer
(ask-question : (Question -> Item))
(define (ask-question q)
  (begin
    (display (string-append (question-que q) "\n"))
    (display-options (question-answers q) 1 (question-provide-own-answer q))
    (let ([r (read)])
      (cond
        [(s-exp-match? `NUMBER r)
         (if (equal? (s-exp->number r) (add1 (length (question-answers q))))
             (if (question-provide-own-answer q)
                 (let ([r2 (read)])
                   (item (question-que q) (s-exp->string r2)))
                 (error 'ask-question "provided own answer when not allowed"))
             (item (question-que q) (list-ref (question-answers q) (- (s-exp->number r) 1))))]
        [else (error 'ask-question "give a numerical response")]))))


; these functions ask the function specified
(ask-question-1 : ((Listof Item) -> (Listof Item)))
(define (ask-question-1 responses)
  (let ([r (ask-question question-1)])
    (let ([responses (cons r responses)])
           (if (equal? (item-response r) "The Prequel")
               (ask-question-2 responses) (ask-question-3 responses)))))

(ask-question-2 : ((Listof Item) -> (Listof Item)))
(define (ask-question-2 responses)
  (let ([r (ask-question question-2)])
    (let ([responses (cons r responses)]) (ask-question-3 responses))))

(ask-question-3 : ((Listof Item) -> (Listof Item)))
(define (ask-question-3 responses)
  (let ([r (ask-question question-3)])
    (let ([responses (cons r responses)])
           (if (equal? (item-response r) "Anakin Skywalker")
               (ask-question-4 responses) (ask-question-5 responses)))))

(ask-question-4 : ((Listof Item) -> (Listof Item)))
(define (ask-question-4 responses)
  (let ([r (ask-question question-4)])
    (cons r responses)))

(ask-question-5 : ((Listof Item) -> (Listof Item)))
(define (ask-question-5 responses)
  (let ([r (ask-question question-5)])
    (let ([responses (cons r responses)])
           (if (equal? (item-response r) "his hopefulness")
               (ask-question-6 responses) (ask-question-7 responses)))))

(ask-question-6 : ((Listof Item) -> (Listof Item)))
(define (ask-question-6 responses)
  (let ([r (ask-question question-6)])
    (cons r responses)))

(ask-question-7 : ((Listof Item) -> (Listof Item)))
(define (ask-question-7 responses)
  (let ([r (ask-question question-7)])
    (cons r responses)))



(main : ( -> Void))
(define (main)
  (for-each display-item (reverse (ask-question-1 empty))))

(main)








