#lang racket
(require web-server/http/request-structs
         web-server/http/xexpr
         web-server/servlet/web
;         (only-in plait pair fst snd)
         )
(provide read/number display/web)



; String (Listof String) Boolean -> Listof String
(define (read/number question options p)
  (let ([num (string->number
              (bytes->string/utf-8
               (binding:form-value
                (bindings-assq
                 #"number"
                 (request-bindings/raw
                  (send/suspend
                   (λ (k-url)
                     (response/xexpr
                      `(html
                        (body
                         (p ,question)
                         (ol ,@(map (lambda (ans) `(li ,ans))
                                    (if p (append options (list "Other:____")) options)))
                         (form ([method "POST"]
                                [action ,k-url])
                               (input ([type "text"]
                                       [name "number"]
                                       [size "8"]))
                               (input ([type "submit"]
                                       [value "Submit"])))))))))))))])
    (if num
      (if (equal? num (add1 (length options)))
          (list question (read/string))
          (list question (list-ref options (- num 1))))
      (read/number question options p))))


; Void -> String
(define (read/string)
  (let ([str (bytes->string/utf-8
               (binding:form-value
                (bindings-assq
                 #"string"
                 (request-bindings/raw
                  (send/suspend
                   (λ (k-url)
                     (response/xexpr
                      `(html
                        (body
                         (p "Please enter your custom answer:")
                         (form ([method "POST"]
                                [action ,k-url])
                               (input ([type "text"]
                                       [name "string"]
                                       [size "8"]))
                               (input ([type "submit"]
                                       [value "Submit"]))))))))))))])
    (if str
      str
      (read/string))))


; (Listof (Listof String)) -> response
(define (display/web items)
  (response/xexpr
   `(html
     (body
      (p "You're done!")
      (p "Your responses:")
      (ul ,@(map (lambda (ans) `(li ,(list-ref ans 0) " " ,(list-ref ans 1))) (reverse items)))))))


