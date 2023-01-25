#lang plait

(print-only-errors #t)


(define-type Binding
  (binding [x : Symbol] [bound-expr : CFWAE]))


(define-type CFWAE
  (num [n : Number])
  (binop [op : (Number Number -> Number)] [lhs : CFWAE] [rhs : CFWAE])
  (with [lob : (Listof Binding)] [body : CFWAE])
  (id [x : Symbol])
  (if0 [c : CFWAE] [t : CFWAE] [e : CFWAE])
  (fun [args : (Listof Symbol)] [body : CFWAE])
  (app [f : CFWAE] [args : (Listof CFWAE)]))


; parse helper functions
(bind : (S-Exp -> Binding))
(define (bind s-exp)
  (let ([s-exps (s-exp->list s-exp)])
    (binding (s-exp->symbol (list-ref s-exps 0))
             (parse (list-ref s-exps 1)))))

(dupe-check : ((Listof Symbol) -> Boolean))
(define (dupe-check l)
  (cond
    [(empty? l) #f]
    [(= (length l) 1) #f]
    [(member (first l) (rest l)) #t]
    [else (dupe-check (rest l))]))

(get-symbol : (S-Exp -> Symbol))
(define (get-symbol s-exp)
  (let ([s-exps (s-exp->list s-exp)])
    (s-exp->symbol (first s-exps))))


(parse : (S-Exp -> CFWAE))
(define (parse s-exp)
  (cond
    [(s-exp-match? `NUMBER s-exp)
     (num (s-exp->number s-exp))]

    [(s-exp-match? `SYMBOL s-exp)
     (cond
       [(equal? (s-exp->symbol s-exp) '+) (error 'parse "bad id")]
       [(equal? (s-exp->symbol s-exp) '-) (error 'parse "bad id")]
       [(equal? (s-exp->symbol s-exp) '*) (error 'parse "bad id")]
       [(equal? (s-exp->symbol s-exp) '/) (error 'parse "bad id")]
       [(equal? (s-exp->symbol s-exp) 'with) (error 'parse "bad id")]
       [(equal? (s-exp->symbol s-exp) 'if0) (error 'parse "bad id")]
       [(equal? (s-exp->symbol s-exp) 'fun) (error 'parse "bad id")]
       [else (id (s-exp->symbol s-exp))])]

    [(s-exp-match? `(with ([SYMBOL ANY] ...) ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (if (dupe-check (map get-symbol (s-exp->list (list-ref s-exps 1))))
           (error 'parse "duplicate identifiers")
       (with (map bind (s-exp->list (list-ref s-exps 1)))
              (parse (list-ref s-exps 2)))))]

    [(s-exp-match? `(fun (SYMBOL ...) ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (if (dupe-check (s-exp->list (list-ref s-exps 1)))
           (error 'parse "duplicate identifiers")
       (fun (map s-exp->symbol (s-exp->list (list-ref s-exps 1)))
              (parse (list-ref s-exps 2)))))]

    [(s-exp-match? `(if0 ANY ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (if0 (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))
                (parse (list-ref s-exps 3))))]
    
    [(s-exp-match? `(+ ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (binop + (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))))]
    
    [(s-exp-match? `(- ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (binop - (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))))]

    [(s-exp-match? `(* ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (binop * (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))))]
    
    [(s-exp-match? `(/ ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (binop / (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))))]

    [(s-exp-match? `(ANY ANY ...) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (app (parse (list-ref s-exps 0))
              (map parse (rest s-exps))))]
    
    [else
     (error 'parse "bad syntax")]))


; PARSE TESTS -- should succeed
(test (parse `10) (num 10))
(test (parse `x) (id 'x))
(test (parse `(+ 10 10)) (binop + (num 10) (num 10)))
(test (parse `(if0 0 x y)) (if0 (num 0) (id 'x) (id 'y)))
(test (parse `(with ([x 5] [y 10]) x)) (with (list (binding 'x (num 5)) (binding 'y (num 10))) (id 'x)))
(test (parse `(fun (x y) (- x y))) (fun (list 'x 'y) (binop - (id 'x) (id 'y))))
(test (parse `(4)) (app (num 4) empty))
(test (parse `(a x y z)) (app (id 'a) (list (id 'x) (id 'y) (id 'z))))
(test (parse `((fun (x y) (+ x y)) 5 4)) (app (fun (list 'x 'y) (binop + (id 'x) (id 'y)))
                                              (list (num 5) (num 4))))

; PARSE TESTS -- should fail
(test/exn (parse `(+ 4)) "bad id")
(test/exn (parse `(with x)) "bad id")
(test/exn (parse `(fun (x x) 10)) "duplicate identifiers")
(test/exn (parse `(with ([x 10] [x 20]) 50)) "duplicate identifiers")
(test/exn (parse `()) "bad syntax")


(define-type Env
  (mtEnv)
  (anEnv [name : Symbol] [value : CFWAE-Value] [env : Env]))


(define-type CFWAE-Value
  (numV [n : Number])
  (closureV [params : (Listof Symbol)]
            [body : CFWAE]
            [env : Env]))


; interp helper functions
(extend-env : ((Listof Binding) Env -> Env))
(define (extend-env lob env)
  (if (cons? lob)
      (extend-env (rest lob)
                  (anEnv (binding-x (first lob)) (interp (binding-bound-expr (first lob)) env) env))
      env))

(lookup-env : (Symbol Env -> CFWAE-Value))
(define (lookup-env x env)
  (type-case Env env
    [(mtEnv) (error 'lookup-env "undefined variable")]
    [(anEnv n v e) (if (equal? n x) v (lookup-env x e))]))


(interp : (CFWAE Env -> CFWAE-Value))
(define (interp exp env)
  (type-case CFWAE exp
    [(num n) (numV n)]
    [(binop op lhs rhs)
     (type-case CFWAE-Value (interp lhs env)
       [(numV m)
        (type-case CFWAE-Value (interp rhs env)
          [(numV n) (if (and (equal? op /) (zero? n))
                        (error 'interp "division by zero!")
                        (numV (op m n)))]
          [(closureV p b e) (error 'interp "bad binop")])]
       [(closureV p1 b1 e1) (error 'interp "bad binop")])]
    [(with lob body) (interp body (extend-env lob env))]
    [(id x) (lookup-env x env)]
    [(if0 c t e)
     (type-case CFWAE-Value (interp c env)
       [(numV n) 
       (if (zero? n)
         (interp t env)
         (interp e env))]
       [(closureV p b e) (error 'interp "bad if")])]
    [(fun args body) (closureV args body env)]
    [(app f args)
     (type-case CFWAE-Value (interp f env)
       [(closureV p b e)
        (if (= (length p) (length args))
            (interp b (extend-env (map2 (lambda (x y) (binding x y)) p args) e))
            (error 'interp "bad app: args/params aren't equal"))]
       [else (error 'interp "bad app: not a func")])]))


; INTERP TESTS -- should pass
(test (extend-env (list (binding 'x (num 5)) (binding 'y (num 10))) (mtEnv))
      (anEnv 'y (numV 10) (anEnv 'x (numV 5) (mtEnv))))
(test (interp (parse `10) (mtEnv)) (numV 10))
(test (interp (parse `(+ 10 15)) (mtEnv)) (numV 25))
(test (interp (parse `(- 15 10)) (mtEnv)) (numV 5))
(test (interp (parse `(- (* (+ 2 4) (/ 6 2)) 1)) (mtEnv)) (numV 17))
(test (interp (parse `(with ([x 5]) x)) (mtEnv)) (numV 5))
(test (interp (parse `(with ([x (* 2 3)]) x)) (mtEnv)) (numV 6))
(test (interp (parse `(with ([x 10] [y 5]) (/ x y))) (mtEnv)) (numV 2))
(test (interp (parse `(with ([x 5]) (with ([x 10]) x))) (mtEnv)) (numV 10))
(test (interp (parse `(if0 0 1 2)) (mtEnv)) (numV 1))
(test (interp (parse `(if0 (- 5 0) 1 2)) (mtEnv)) (numV 2))
(test (interp (parse `((fun (x y) (+ x y)) 5 4)) (mtEnv)) (numV 9))
(test (interp (parse `(with ([z 5]) ((fun (x y) (+ x y)) z 4))) (mtEnv)) (numV 9))
(test (interp (parse `(fun (x) (+ 1 x))) (mtEnv)) (closureV (list 'x) (binop + (num 1) (id 'x)) (mtEnv)))
(test (interp (parse `(with ([add1 (fun (x) (+ 1 x))]) (add1 1))) (mtEnv)) (numV 2))

(test (interp (parse `((with ([x 10]) (fun () 12)))) (mtEnv)) (numV 12))
(test (interp (parse `(with ([x 10]) ((with ((x 12)) (fun () x))))) (mtEnv)) (numV 12))
(test (interp (parse `(with ([x 5]) ((if0 (/ x x) (fun () 25) (fun () 32))))) (mtEnv)) (numV 32))
(test (interp (parse `(with ((f (fun () 10))) (f))) (mtEnv)) (numV 10))
(test (interp (parse `(with ([f (with ([x 10]) (fun () x))]) (with ([x 12]) (f)))) (mtEnv)) (numV 10))

; INTERP TESTS -- should fail
(test/exn (interp (parse `x) (mtEnv)) "undefined variable")
(test/exn (interp (parse `(/ 27 0)) (mtEnv)) "division by zero!")
(test/exn (interp (parse `((fun (x y) (+ x y)) 5 4 3 2)) (mtEnv)) "bad app: args/params aren't equal")
(test/exn (interp (parse `(4)) (mtEnv)) "bad app: not a func")
(test/exn (interp (parse `(5 (fun (x y) (+ x y)) 4)) (mtEnv)) "bad app: not a func")
(test/exn (interp (parse `(if0 (fun (x) x) 2 3)) (mtEnv)) "bad if")
(test/exn (interp (parse `(+ 5 (fun (x) x))) (mtEnv)) "bad binop")
(test/exn (interp (parse `(+ (fun (x) x) 5)) (mtEnv)) "bad binop")









