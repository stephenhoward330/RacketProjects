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
    [(< (first temps) 5) #f]
    [(> (first temps) 95) #f]
    [else (check-temps1 (rest temps))]))
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
    [(< (first temps) low) #f]
    [(> (first temps) high) #f]
    [else (check-temps (rest temps) low high)]))
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
  (cond
    [(empty? digits) 0]
    [else (+ (first digits) (* 10 (convert (rest digits))))]))
; 3.
(test (convert empty) 0)
(test (convert (list 0 1)) 10)
(test (convert (list 1 2 3)) 321)
(test (convert (list 6 5 4)) 456)
(test (convert (list 1 2 3 4 5 6 7 8 9)) 987654321)


; average-price

; helpers: list-size and list-sum
(define (list-size list)
  (cond
    [(empty? list) 0]
    [else (+ 1 (list-size (rest list)))]))
(define (list-sum list)
  (cond
    [(empty? list) 0]
    [else (+ (first list) (list-sum (rest list)))]))
(test (list-size (list 0 1 2 3)) 4)
(test (list-sum (list 0 1 2 3)) 6)

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
    [(= (list-size prices) 0) 0]
    [else (/ (list-sum prices) (list-size prices))]))
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
  (cond
    [(empty? fahrenheits) empty]
    [else (cons (* 5/9 (- (first fahrenheits) 32)) (convertFC (rest fahrenheits)))]))
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
  (cond
    [(empty? lop) empty]
    [(<= (first lop) ua) (cons (first lop) (eliminate-exp ua (rest lop)))]
    [else (eliminate-exp ua (rest lop))]))
; 3.
(test (eliminate-exp 5 empty) empty)
(test (eliminate-exp 12 (list 15 20)) empty)
(test (eliminate-exp 15 (list 5 10 15 20 25)) (list 5 10 15))


; suffixes

; 1. takes in a list l and returns a list of lists
; 2a. produces a list of the suffixes of the input list
; 2b. (Listof 'a) -> (Listof (Listof 'a))
; 4.
#;
(define (suffixes l)
  ....)
; 5.
(suffixes : ((Listof 'a) -> (Listof (Listof 'a))))
(define (suffixes l)
  (cond
    [(empty? l) (cons empty empty)]
    [else (cons l (suffixes (rest l)))]))
; 3.
(test (suffixes empty) (list (list)))
(test (suffixes (list 'a 'b 'c 'd)) (list (list 'a 'b 'c 'd) (list 'b 'c 'd) (list 'c 'd) (list 'd) (list)))
(test (suffixes (list 1 2 3 4)) (list (list 1 2 3 4) (list 2 3 4) (list 3 4) (list 4) (list)))
(test (suffixes (list "a" "b" "c")) (list (list "a" "b" "c") (list "b" "c") (list "c") (list)))


(define-type Pedigree
  (person [name : String]
          [birth-year : Number]
          [eye-color : Symbol]
          [father : Pedigree]
          [mother : Pedigree])
  (unknown))


; count-persons

; 1. a pedigree is represented by a Pedigree (above), and the return value is a number
; 2a. Counts the number of known persons in a given pedigree
; 2b. Pedigree -> Number
; 4.
#;
(define (count-persons pedigree)
  ....)
; 5.
(count-persons : (Pedigree -> Number))
(define (count-persons pedigree)
  (type-case Pedigree pedigree
    [(unknown) 0]
    [(person n b-y e-c f m) (+ 1 (+ (count-persons f) (count-persons m)))]))
; 3.
(test (count-persons (unknown)) 0)
(test (count-persons (person "John" 1980 'B (unknown) (unknown))) 1)
(test (count-persons (person "John" 1980 'B (person "Mary" 1955 'W (unknown) (unknown)) (unknown))) 2)
(test (count-persons (person "John" 1980 'B (person "Mary" 1955 'W (person "Jane" 1930 'T (unknown) (unknown))
                                                    (unknown)) (unknown))) 3)


; average-age

; helper: sum-age
(define (sum-age pedigree)
  (type-case Pedigree pedigree
    [(unknown) 0]
    [(person n b-y e-c f m) (+ (- 2020 b-y) (+ (sum-age f) (sum-age m)))]))
(test (sum-age (unknown)) 0)
(test (sum-age (person "John" 1980 'B (unknown) (unknown))) 40)
(test (sum-age (person "John" 1980 'B (person "Mary" 1954 'W (unknown) (unknown)) (unknown))) 106)

; 1. pedigree is represented by a Pedigree and the return value is a number
; 2a. Returns the average age of all known persons in the pedigree as of 2020
; 2b. Pedigree -> Number
; 4.
#;
(define (average-age pedigree)
  ....)
; 5.
(average-age : (Pedigree -> Number))
(define (average-age pedigree)
  (cond
    [(= (count-persons pedigree) 0) 0]
    [else (/ (sum-age pedigree) (count-persons pedigree))]))
; 3.
(test (average-age (unknown)) 0)
(test (average-age (person "John" 1980 'B (unknown) (unknown))) 40)
(test (average-age (person "John" 1980 'B (person "Mary" 1954 'W (unknown) (unknown)) (unknown))) 53)
(test (average-age (person "John" 1980 'B (person "Mary" 1954 'W (person "Jane" 1930 'T (unknown) (unknown))
                                                    (unknown)) (unknown))) 196/3)


; eye-colors

; 1. pedigree is represented by a Pedigree and the return value is a listof symbols
; 2a. produces a list of all eye colors in the given pedigree
; 2b. Pedigree -> (Listof Symbol)
; 4.
#;
(define (eye-colors pedigree)
  ....)
; 5.
(eye-colors : (Pedigree -> (Listof Symbol)))
(define (eye-colors pedigree)
  (type-case Pedigree pedigree
    [(unknown) empty]
    [(person n b-y e-c f m) (cons e-c (append (eye-colors f) (eye-colors m)))]))
; 3.
(test (eye-colors (unknown)) empty)
(test (eye-colors (person "John" 1980 'B (unknown) (unknown))) (list 'B))
(test (eye-colors (person "John" 1980 'B (person "Mary" 1954 'W (unknown) (unknown)) (unknown))) (list 'B 'W))
(test (eye-colors (person "John" 1980 'B (person "Mary" 1954 'W (person "Jane" 1930 'T (unknown) (unknown))
                                                    (unknown)) (unknown))) (list 'B 'W 'T))



