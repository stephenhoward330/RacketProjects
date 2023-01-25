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


; sum-coins

; 1. pennies, nickels, dimes, quarters, and the sum are Numbers
; 2a. We want to generate the sum of a given set of coins
; 2b. Number Number Number Number -> Number
; 4.
#;
(define (sum-coins pennies nickels dimes quarters)
  ....)
; 5.
(sum-coins : (Number Number Number Number -> Number))
(define (sum-coins pennies nickels dimes quarters)
  (+ (+ pennies (* 5 nickels)) (+ (* 10 dimes) (* 25 quarters))))
; 3.
(test (sum-coins 0 0 0 0) 0)
(test (sum-coins 1 0 0 0) 1)
(test (sum-coins 0 1 0 0) 5)
(test (sum-coins 0 0 1 0) 10)
(test (sum-coins 0 0 0 1) 25)
(test (sum-coins 1 1 1 1) 41)


; area-cylinder
(define pi 3.1415926535897)

; 1. base-radius, height, and the area are Numbers
; 2a. We need to determine the surface area of the cylinder given the base-radius and height
; 2b. Number Number -> Number
; 4.
#;
(define (area-cylinder base-radius height)
  ....)
; 5.
(area-cylinder : (Number Number -> Number))
(define (area-cylinder base-radius height)
  (* base-radius (* pi (* 2 (+ height base-radius)))))
; 3.
(test (area-cylinder 1 1) 12.57)
(test (area-cylinder 2 2) 50.27)
(test (area-cylinder 3 4) 131.95)
(test (area-cylinder 5 5) 314.16)


; area-pipe1

; 1. inner-radius, wall-thickness, length, and the area are Numbers
; 2a. Generate the surface area of a pipe given the inner-radius, wall-thickness, and length
; 2b. Number Number Number -> Number
; 4.
#;
(define (area-pipe1 inner-radius wall-thickness length)
  ....)
; 5.
(area-pipe1 : (Number Number Number -> Number))
(define (area-pipe1 inner-radius wall-thickness length)
  (- (+ (* inner-radius (* pi (* 2 (+ length inner-radius))))
     (* (+ wall-thickness inner-radius) (* pi (* 2 (+ length (+ wall-thickness inner-radius))))))
  (* 2 (* pi (* inner-radius inner-radius)))))
; 3.
(test (area-pipe1 1 1 1) 43.99)
(test (area-pipe1 2 2 2) 175.93)
(test (area-pipe1 1 2 3) 131.95)


; area-pipe2

; 1. inner-radius, wall-thickness, length, and the area are Numbers
; 2a. Generate the surface area of a pipe given the inner-radius, wall-thickness, and length
;       We will use area-cylinder and area-circle helper functions
; 2b. Number Number Number -> Number
; 4.
#;
(define (area-pipe2 inner-radius wall-thickness length)
  ....)
; 5.
(area-circle : (Number -> Number))
(define (area-circle radius)
  (* pi (* radius radius)))

(area-pipe2 : (Number Number Number -> Number))
(define (area-pipe2 inner-radius wall-thickness length)
  (- (+ (area-cylinder inner-radius length)
     (area-cylinder (+ wall-thickness inner-radius) length))
  (* 2 (area-circle inner-radius))))
; 3.
(test (area-pipe2 1 1 1) 43.99)
(test (area-pipe2 2 2 2) 175.93)
(test (area-pipe2 1 2 3) 131.95)


; tax

; 1. gross-pay and tax are Numbers
; 2a. Generate the amount of tax owed given gross-pay
; 2b. Number -> Number
; 4.
#;
(define (tax gross-pay)
  ....)
; 5.
(tax : (Number -> Number))
(define (tax gross-pay)
  (cond
    [(> gross-pay 480)
     (+ 36 (* .28 (- gross-pay 480)))]
    [(> gross-pay 240)
     (* .15 (- gross-pay 240))]
    [else 0]))
; 3.
(test (tax 0) 0)
(test (tax 240) 0)
(test (tax 480) 36)
(test (tax 720) 103.2)
(test (tax 1000) 181.6)


; net-pay

; 1. hours-worked, hourly-wage, and pay are Numbers
; 2a. Generate the amount of net pay given hours-worked and hourly-wage, after tax
; 2b. Number Number -> Number
; 4.
#;
(define (net-pay hours-worked hourly-wage)
  ....)
; 5.
(net-pay : (Number Number -> Number))
(define (net-pay hours-worked hourly-wage)
  (- (* hours-worked hourly-wage) (tax (* hours-worked hourly-wage))))
; 3.
(test (net-pay 0 0) 0)
(test (net-pay 5 5) 25)
(test (net-pay 24 10) 240)
(test (net-pay 100 5) 458.4)
(test (net-pay 55 10) 494.4)


; what-kind

; 1. a, b, and c are Numbers, and we will return a Symbol
; 2a. Determine whether the quadratic equation is degenerate,
;       and if not, return whether there will be 0, 1, or 2 solutions
; 2b. Number Number Number -> Symbol
; 4.
#;
(define (what-kind a b c)
  ....)
; 5.
(what-kind : (Number Number Number -> Symbol))
(define (what-kind a b c)
  (cond
    [(= a 0) 'degenerate]
    [(> (- (* b b) (* 4 (* a c))) 0) 'two]
    [(= (- (* b b) (* 4 (* a c))) 0) 'one]
    [else 'none]))
; 3.
(test (what-kind 0 1 1) 'degenerate)
(test (what-kind 2 1 2) 'none)
(test (what-kind 4 4 1) 'one)
(test (what-kind 1 5 2) 'two)


; time-diff
(define-type Time
  (hms [hours : Number] [minutes : Number] [seconds : Number]))

; 1. t2 and t2 are of Time type, the time difference is a Number
; 2a. Give the difference in seconds between time one and time two
; 2b. Time Time -> Number
; 4.
#;
(define (time-diff t1 t2)
  ....)
; 5.
(time-diff : (Time Time -> Number))
(define (time-diff t1 t2)
  (+ (+ (* 3600 (- (hms-hours t2) (hms-hours t1)))
     (* 60 (- (hms-minutes t2) (hms-minutes t1))))
     (- (hms-seconds t2) (hms-seconds t1))))
; 3.
(test (time-diff (hms 10 25 0) (hms 10 25 0)) 0)
(test (time-diff (hms 10 25 0) (hms 10 25 10)) 10)
(test (time-diff (hms 10 25 0) (hms 10 26 10)) 70)
(test (time-diff (hms 10 25 0) (hms 11 26 10)) 3670)
(test (time-diff (hms 10 25 0) (hms 9 24 50)) -3610)


; area
(define-type Position (position [x : Number] [y : Number]))
(define-type Shape
  (circle [center : Position]
          [radius : Number])
  (square [upper-left : Position]
          [side-length : Number])
  (rectangle [upper-left : Position]
             [width : Number]
             [length : Number]))

; 1. shape is a Shape, defined above, while the area is a Number
; 2a. Gives the area of a shape (circle, square, rectangle)
; 2b. Shape -> Number
; 4.
#;
(define (area shape)
  ....)
; 5.
(area : (Shape -> Number))
(define (area shape)
  (type-case Shape shape
    [(circle center radius) (* pi (* radius radius))]
    [(square upper-left side-length) (* side-length side-length)]
    [(rectangle upper-left width length) (* width length)]))
; 3.
(test (area (circle (position 0 1) 3)) 28.27)
(test (area (square (position 2 3) 3)) 9)
(test (area (rectangle (position 4 5) 3 4)) 12)


; translate-shape

; 1. shape is a Shape, and a Shape is returned, and delta is a Number
; 2a. Gives a shape that is moved by delta units in the x direction
; 2b. Shape Number -> Shape
; 4.
#;
(define (translate-shape shape delta)
  ...)
; 5.
(translate-shape : (Shape Number -> Shape))
(define (translate-shape shape delta)
  (type-case Shape shape
    [(circle p r) (circle (position (+ (position-x p) delta) (position-y p)) r)]
    [(square p l) (square (position (+ (position-x p) delta) (position-y p)) l)]
    [(rectangle p w l) (rectangle (position (+ (position-x p) delta) (position-y p)) w l)]))
; 3.
(test (translate-shape (circle (position 0 0) 2) 1) (circle (position 1 0) 2))
(test (translate-shape (square (position 1 0) 2) 1) (square (position 2 0) 2))
(test (translate-shape (rectangle (position 2 0) 2 2) 2) (rectangle (position 4 0) 2 2))


; in-shape?

; 1. shape is a Shape, p is a Position, and we return a boolean
; 2a. we determine if the position p is in the shape given (one the edge counts as NOT in the shape)
; 2b. Shape Position -> Boolean
; 4.
#;
(define (in-shape? shape p)
  ....)
; 5.
(in-shape? : (Shape Position -> Boolean))
(define (in-shape? shape p)
  (type-case Shape shape
    [(circle pos r) (cond
                      [(< (+ (* (- (position-x p) (position-x pos)) (- (position-x p) (position-x pos)))
                             (* (- (position-y p) (position-y pos)) (- (position-y p) (position-y pos))))
                          (* r r)) #t]
                      [else #f])]
    [(square sp l) (cond
                      [(<= (position-x p) (position-x sp)) #f]
                      [(>= (position-x p) (+(position-x sp) l)) #f]
                      [(>= (position-y p) (position-y sp)) #f]
                      [(<= (position-y p) (-(position-y sp) l)) #f]
                      [else #t])]
    [(rectangle rp w l) (cond
                      [(<= (position-x p) (position-x rp)) #f]
                      [(>= (position-x p) (+(position-x rp) w)) #f]
                      [(>= (position-y p) (position-y rp)) #f]
                      [(<= (position-y p) (-(position-y rp) l)) #f]
                      [else #t])]))
; 3.
(test (in-shape? (circle (position 0 0) 2) (position 1 1)) #true)
(test (in-shape? (circle (position 0 0) 2) (position 2 2)) #false)
(test (in-shape? (square (position 0 2) 2) (position 1 1)) #true)
(test (in-shape? (square (position 0 2) 2) (position 2 2)) #false)
(test (in-shape? (rectangle (position 0 3) 2 3) (position 1 2)) #true)
(test (in-shape? (rectangle (position 0 3) 2 3) (position 5 0)) #false)














