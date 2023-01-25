#lang plait

(print-only-errors #t)

; Design Recipe
; 1. Figure out how to represent the information the function operates over as data
; 2a. Write a natural-language purpose statement for your function
; 2b. Write the contract of the function
; 3. Write functional examples about the usage of your function
; 4. Take inventory and make template
; 5. Fill in the template with our implementation
; 6. Make sure that implementation is consistent with examples


; check-temps1

; 1. temps is a list of numbers, and the return value is a Boolean
; 2a. Checks whether all temperature values are between 5 and 95 degrees
; 2b. (Listof Number) -> Boolean
; 4.
#;
(define (check-temps1 temps)
  ....)
; 5.
(check-temps1 : ((Listof Number) -> Boolean))
(define (check-temps1 temps)
  (cond
    [(empty? temps) #t]
    [(empty? (append (filter (lambda(n) (< n 5)) temps) (filter (lambda(n) (> n 95)) temps))) #t]
    [else #f]
    ))
; 3.
(test (check-temps1 empty) #t)
(test (check-temps1 (list 0 15 20)) #f)
(test (check-temps1 (list 5)) #t)
(test (check-temps1 (list 95)) #t)
(test (check-temps1 (list 5 50 70 95 100)) #f)


; check-temps

; 1. temps is a list of numbers, low and high are numbers, and the return is a boolean
; 2a. checks whether are temps in the list are between low and high degrees
; 2b. (Listof Number) Number Number -> Boolean
; 4.
#;
(define (check-temps temps low high)
  ....)
; 5.
(check-temps : ((Listof Number) Number Number -> Boolean))
(define (check-temps temps low high)
  (cond
    [(empty? temps) #t]
    [(empty? (append (filter (lambda(n) (< n low)) temps) (filter (lambda(n) (> n high)) temps))) #t]
    [else #f]
    ))
; 3.
(test (check-temps empty 0 15) #t)
(test (check-temps (list 0 15 20) 0 100) #t)
(test (check-temps (list 0 15 20) 5 15) #f)
(test (check-temps (list 0 15 20) 10 10) #f)
(test (check-temps (list 10) 10 10) #t)


; convert

; 1. digits is a list of numbers, and the return value is a number
; 2a. takes a list of digits (0 to 9) and returns a corresponding number
;        with the first digit being the least significant.
; 2b. (Listof Number) -> Number
; 4.
#;
(define (convert digits)
  ....)
; 5.
(convert : ((Listof Number) -> Number))
(define (convert digits)
  (foldr (lambda(n r) (+ (* 10 r) n)) 0 digits))
; 3.
(test (convert empty) 0)
(test (convert (list 0 1)) 10)
(test (convert (list 1 2 3)) 321)
(test (convert (list 6 5 4)) 456)
(test (convert (list 1 2 3 4 5 6 7 8 9)) 987654321)


; average-price

; 1. prices are a listof numbers, and the average is a number
; 2a. takes a list of prices and computes the average price
; 2b. (Listof Number) -> Number
; 4.
#;
(define (average-price prices)
  ....)
; 5.
(average-price : ((Listof Number) -> Number))
(define (average-price prices)
  (cond
    [(empty? prices) 0]
    [else (/ (foldl + 0 prices)(length prices))]))
; 3.
(test (average-price empty) 0)
(test (average-price (list 5 10 15)) 10)
(test (average-price (list 10)) 10)
(test (average-price (list 5 10 15 20 25)) 15)
(test (average-price (list 5 15)) 10)


; convertFC

; 1. the input is a list of fahrenheit numbers, and the output is a list of celsius numbers
; 2a. converts a list of fahrenheit measurements to a list of celsius measurements
; 2b. (Listof Number) -> (Listof Number)
; 4.
#;
(define (convertFC fahrenheits)
  ....)
; 5.
(convertFC : ((Listof Number) -> (Listof Number)))
(define (convertFC fahrenheits)
  (map (lambda(t)(* 5/9 (- t 32))) fahrenheits))
; 3.
(test (convertFC empty) empty)
(test (convertFC (list 15 32 50 100)) (list -85/9 0 10 340/9))


; eliminate-exp

; 1. ua is a number, lop and the return are lists of numbers
; 2a. removes all prices greater than ua from the list lop
; 2b. Number (Listof Number) -> (listof Number)
; 4.
#;
(define (eliminate-exp ua lop)
  ....)
; 5.
(eliminate-exp : (Number (Listof Number) -> (Listof Number)))
(define (eliminate-exp ua lop)
  (filter (lambda(x)(>= ua x)) lop))
; 3.
(test (eliminate-exp 5 empty) empty)
(test (eliminate-exp 12 (list 15 20)) empty)
(test (eliminate-exp 15 (list 5 10 15 20 25)) (list 5 10 15))


; compose-func

; 1. before, after, and the result are functions
; 2a. composes the before and after functions into a single function
; 2b. ('a -> 'b) ('c -> 'd) -> ('a -> 'd)
; 4.
#;
(define (compose-func after before)
  ....)
; 5.
(compose-func : (('a -> 'b) ('c -> 'd) -> ('a -> 'd)))
(define (compose-func after before)
  (lambda (n) (after (before n))))
; 3.
(test ((compose-func add1 sub1) 4) 4)
(test ((compose-func add1 add1) 4) 6)
(test ((compose-func string-length symbol->string) 'hi) 2)


; flatten

; 1. loloa is a listof listof anything, and the return is a list of anything
; 2a. returns a list of everything in the list of lists
; 2b. (Listof (Listof 'a)) -> (Listof 'a)
; 4.
#;
(define (flatten loloa)
  ....)
; 5.
(flatten : ((Listof (Listof 'a)) -> (Listof 'a)))
(define (flatten loloa)
  (cond
    [(empty? loloa) empty]
    [else (append (first loloa) (flatten (rest loloa)))]))
; 3.
(test (flatten empty) empty)
(test (flatten (list empty)) empty)
(test (flatten (list (list 1 2) (list 3 4 5) (list 6))) (list 1 2 3 4 5 6))
(test (flatten (list (list 0) empty)) (list 0))


; flatten-foldr

; 1. loloa is a listof listof anything, and the return is a list of anything
; 2a. returns a list of everything in the list of lists
; 2b. (Listof (Listof 'a)) -> (Listof 'a)
; 4.
#;
(define (flatten-foldr loloa)
  ....)
; 5.
(flatten-foldr : ((Listof (Listof 'a)) -> (Listof 'a)))
(define (flatten-foldr loloa)
  (foldr append empty loloa))
; 3.
(test (flatten-foldr empty) empty)
(test (flatten-foldr (list empty)) empty)
(test (flatten-foldr (list (list 1 2) (list 3 4 5) (list 6))) (list 1 2 3 4 5 6))
(test (flatten-foldr (list (list 0) empty)) (list 0))


; bucket

; 1. lon is a listof numbers, we will return a listof listof numbers
; 2a. returns a list of sublists of any adjacent equal numbers
; 2b. (Listof Number) -> ((Listof (Listof Number))
; 4.
#;
(define (bucket lon)
  ....)
; 5.
(bucket : ((Listof Number) -> (Listof (Listof Number))))
(define (bucket lon)
  (foldr (lambda (n r) (cond
                         [(empty? r) (cons (list n) r)]
                         [(= n (first (first r))) (cons (cons n (first r)) (rest r))]
                         [else (cons (list n) r)])) empty lon))
; 3.
(test (bucket empty) empty)
(test (bucket (list 1 1 2 2 2 3 1 1 1 2 3 3))
      (list (list 1 1) (list 2 2 2) (list 3) (list 1 1 1) (list 2) (list 3 3)))
(test (bucket (list 1 2 3 4)) (list (list 1) (list 2) (list 3) (list 4)))


(define-type Pedigree
  (person [name : String]
          [birth-year : Number]
          [eye-color : Symbol]
          [father : Pedigree]
          [mother : Pedigree])
  (unknown))


; tree-map

; 1. f is a function (String -> String), pedigree is a Pedigree, and the return is also a Pedigree
; 2a. returns a pedigree where f has been applied to every person’s name in pedigree
; 4.
#;
(define (tree-map f pedigree)
  ....)
; 5.
(tree-map : ((String -> String) Pedigree -> Pedigree))
(define (tree-map f pedigree)
  (type-case Pedigree pedigree
    [(unknown) (unknown)]
    [(person n b-y e-c fa mo) (person (f n) b-y e-c (tree-map f fa) (tree-map f mo))]))
; 3.
; helper function for testing
(append-hi : (String -> String))
(define (append-hi str)
  (string-append str "hi"))
(test (tree-map append-hi (unknown)) (unknown))
(test (tree-map append-hi (person "Jack" 1990 'B (unknown) (unknown)))
      (person "Jackhi" 1990 'B (unknown) (unknown)))
(test (tree-map append-hi (person "Jack" 1990 'B (person "Mary" 1950 'W (unknown) (unknown)) (unknown)))
      (person "Jackhi" 1990 'B (person "Maryhi" 1950 'W (unknown) (unknown)) (unknown)))


; add-last-name

; 1. last name is a string, and the input pedigree and the resulting pedigree are of type Pedigree
; 2a. returns a Pedigree where the last name has been appended to every person’s name in the pedigree
; 2b. Pedigree String -> Pedigree
; 4.
#;
(define (add-last-name pedigree last-name)
  ....)
; 5.
(add-last-name : (Pedigree String -> Pedigree))
(define (add-last-name pedigree last-name)
  (tree-map (lambda (x) (string-append x (string-append " " last-name))) pedigree))
; 3.
(test (add-last-name (unknown) "ln") (unknown))
(test (add-last-name (person "Jack" 1990 'B (unknown) (unknown)) "ln")
      (person "Jack ln" 1990 'B (unknown) (unknown)))
(test (add-last-name (person "Jack" 1990 'B (person "Mary" 1950 'W (unknown) (unknown)) (unknown)) "ln")
      (person "Jack ln" 1990 'B (person "Mary ln" 1950 'W (unknown) (unknown)) (unknown)))





