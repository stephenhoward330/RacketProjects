#lang plait
(print-only-errors #t)

(define-type Expr
  (num [n : Number])
  (id [x : Symbol])
  (bool [b : Boolean])
  (bin-num-op [op : (Number Number -> Number)] [lhs : Expr] [rhs : Expr])
  (iszero [e : Expr])
  (bif [test : Expr] [then : Expr] [else : Expr])
  (with [bound-x : Symbol] [bound-body : Expr] [body : Expr])
  (fun [arg-x : Symbol]
       [arg-type : Type]
       [body : Expr]
       [body-type : Type])
  (app [fun : Expr] [arg : Expr])
  (nempty)
  (ncons [first : Expr] [rest : Expr])
  (nfirst [e : Expr])
  (nrest [e : Expr])
  (isnempty [e : Expr]))

(define-type Type
  (t-num)
  (t-bool)
  (t-nlist)
  (t-fun [arg : Type] [result : Type]))

(typer : (S-Exp -> Type))
(define (typer s-exp)
  (cond
    [(s-exp-match? `number s-exp)
     (t-num)]
    [(s-exp-match? `boolean s-exp)
     (t-bool)]
    [(s-exp-match? `nlist s-exp)
     (t-nlist)]
    [(s-exp-match? `(ANY -> ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (t-fun (typer (list-ref s-exps 0)) (typer (list-ref s-exps 2))))]
    [else (error 'parse "bad type")]
    ))

;(parse : (S-Exp -> CFWAE))
(parse : (S-Exp -> Expr))
(define (parse s-exp)
  (cond
    [(s-exp-match? `NUMBER s-exp)
     (num (s-exp->number s-exp))]

;     (cond
;       [(equal? (s-exp->symbol s-exp) '+) (error 'parse "bad id")]
;       [(equal? (s-exp->symbol s-exp) '-) (error 'parse "bad id")]
;       [(equal? (s-exp->symbol s-exp) '*) (error 'parse "bad id")]
;       [(equal? (s-exp->symbol s-exp) '/) (error 'parse "bad id")]
;       [(equal? (s-exp->symbol s-exp) 'with) (error 'parse "bad id")]
;       [(equal? (s-exp->symbol s-exp) 'if0) (error 'parse "bad id")]
;       [(equal? (s-exp->symbol s-exp) 'fun) (error 'parse "bad id")]
;       [else (id (s-exp->symbol s-exp))])]

    [(s-exp-match? `true s-exp)
     (bool #t)]

    [(s-exp-match? `false s-exp)
     (bool #f)]

    [(s-exp-match? `null s-exp)
     (nempty)]

    [(s-exp-match? `(zero? ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (iszero (parse (list-ref s-exps 1))))]

    [(s-exp-match? `(with (SYMBOL ANY) ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (let ([sym-any (s-exp->list (list-ref s-exps 1))])
         (with (s-exp->symbol (list-ref sym-any 0)) (parse (list-ref sym-any 1)) (parse (list-ref s-exps 2)))))]

    #;[(s-exp-match? `(with ([SYMBOL ANY] ...) ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (if (dupe-check (map get-symbol (s-exp->list (list-ref s-exps 1))))
           (error 'parse "duplicate identifiers")
       (with (map bind (s-exp->list (list-ref s-exps 1)))
              (parse (list-ref s-exps 2)))))]

    [(s-exp-match? `(fun (SYMBOL : ANY) : ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (let ([sym-any (s-exp->list (list-ref s-exps 1))])
         (fun (s-exp->symbol (list-ref sym-any 0)) (typer (list-ref sym-any 2)) (parse (list-ref s-exps 4)) (typer (list-ref s-exps 3)))))]

    [(s-exp-match? `(if ANY ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (bif (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))
                (parse (list-ref s-exps 3))))]
    
    [(s-exp-match? `(+ ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (bin-num-op + (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))))]
    
    [(s-exp-match? `(- ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (bin-num-op - (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))))]

    [(s-exp-match? `(* ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (bin-num-op * (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))))]

    [(s-exp-match? `(cons ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (ncons (parse (list-ref s-exps 1))
              (parse (list-ref s-exps 2))))]

    [(s-exp-match? `(first ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (nfirst (parse (list-ref s-exps 1))))]

    [(s-exp-match? `(rest ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (nrest (parse (list-ref s-exps 1))))]

    [(s-exp-match? `(empty? ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (isnempty (parse (list-ref s-exps 1))))]

    [(s-exp-match? `(ANY ANY) s-exp)
     (let ([s-exps (s-exp->list s-exp)])
       (app (parse (list-ref s-exps 0))
              (parse (list-ref s-exps 1))))]

    [(s-exp-match? `SYMBOL s-exp)
     (id (s-exp->symbol s-exp))]
    
    [else
     (error 'parse "bad syntax")]))


; PARSE TESTS -- should succeed
(test (parse `10) (num 10))
(test (parse `x) (id 'x))
(test (parse `(+ 10 10)) (bin-num-op + (num 10) (num 10)))
(test (parse `(if 0 x y)) (bif (num 0) (id 'x) (id 'y)))
(test (parse `(with (x 5) x)) (with 'x (num 5) (id 'x)))
(test (parse `(fun (x : number) : number (- x 5))) (fun 'x (t-num) (bin-num-op - (id 'x) (num 5)) (t-num)))
(test (parse `(fun (x : boolean) : number (- x 5))) (fun 'x (t-bool) (bin-num-op - (id 'x) (num 5)) (t-num)))
(test (parse `(fun (x : number) : (number -> number) (- x 5))) (fun 'x (t-num) (bin-num-op - (id 'x) (num 5)) (t-fun (t-num) (t-num))))
;(test (parse `(4)) (app (num 4) empty))
(test (parse `(a x)) (app (id 'a) (id 'x)))
(test (parse `null) (nempty))
(test (parse `true) (bool #t))
(test (parse `false) (bool #f))
(test (parse `(zero? 5)) (iszero (num 5)))
(test (parse `(cons 5 x)) (ncons (num 5) (id 'x)))
(test (parse `(first 5)) (nfirst (num 5)))
(test (parse `(rest 5)) (nrest (num 5)))
(test (parse `(empty? 5)) (isnempty (num 5)))
;(test (parse `((fun (x y) (+ x y)) 5 4)) (app (fun (list 'x 'y) (binop + (id 'x) (id 'y)))
;                                              (list (num 5) (num 4))))

; PARSE TESTS -- should fail
(test/exn (parse `()) "bad syntax")


(define-type Env
  (mtEnv)
  (anEnv [name : Symbol] [value : Type] [env : Env]))

(extend-env : (Symbol Type Env -> Env))
(define (extend-env s t env)
  (anEnv s t env))

(lookup-env : (Symbol Env -> Type))
(define (lookup-env x env)
  (type-case Env env
    [(mtEnv) (error 'lookup-env "undefined variable")]
    [(anEnv n v e) (if (equal? n x) v (lookup-env x e))]))

(type-of : (Expr -> Type))
(define (type-of expr)
  (do-type-of expr (mtEnv)))

(do-type-of : (Expr Env -> Type))
(define (do-type-of expr env)
  (type-case Expr expr
    [(num n) (t-num)]
    [(id x) (lookup-env x env)]
    [(bool b) (t-bool)]
    [(bin-num-op op lhs rhs)
     (if (equal? (do-type-of lhs env) (t-num))
         (if (equal? (do-type-of rhs env) (t-num))
         (t-num)
         (error 'type-of "Number expected"))
         (error 'type-of "Number expected"))]
    [(iszero e) (if (equal? (do-type-of e env) (t-num)) (t-bool) (error 'type-of "Number expected"))]
    [(bif test then e) (if (equal? (do-type-of test env) (t-bool))
                              (if (equal? (do-type-of then env) (do-type-of e env))
                                  (do-type-of then env)
                                  (error 'type-of "Then and else type mismatch"))
                              (error 'type-of "Boolean expected"))]
    [(with bound-x bound-body body) (let ([new-env (extend-env bound-x (do-type-of bound-body env) env)])
                                      (do-type-of body new-env))]
    [(fun arg-x arg-type body body-type) (let ([new-env (extend-env arg-x arg-type env)])
                                      (if (equal? (do-type-of body new-env) body-type) (t-fun arg-type body-type)
                                          (error 'type-of "Body and body-type mismatch")))]
    [(app fun arg) (let ([derek (do-type-of fun env)] [john (do-type-of arg env)])
                     (type-case Type derek
                       [(t-fun a b) (if (equal? john a) b (error 'type-of "Function input arg mismatch"))]
                       [else (error 'type-of "Function expected")]))]
    [(nempty) (t-nlist)]
    [(ncons first rest) (if (equal? (do-type-of first env) (t-num)) (if (equal? (do-type-of rest env) (t-nlist)) (t-nlist) (error 'type-of "List expected")) (error 'type-of "Number expected"))]
    [(nfirst e) (if (equal? (do-type-of e env) (t-nlist)) (t-num) (error 'type-of "List expected"))]
    [(nrest e) (if (equal? (do-type-of e env) (t-nlist)) (t-nlist) (error 'type-of "List expected"))]
    [(isnempty e) (if (equal? (do-type-of e env) (t-nlist)) (t-bool) (error 'type-of "List expected"))]
    ))


(test (type-of (parse `10)) (t-num))
(test (type-of (parse `(+ 10 10))) (t-num))
(test (type-of (parse `(if true 5 10))) (t-num))
(test (type-of (parse `(with (x 5) x))) (t-num))
(test (type-of (parse `(fun (x : number) : number (- x 5)))) (t-fun (t-num) (t-num)))
(test (type-of (parse `((fun (x : number) : number (- x 5)) 25))) (t-num))
(test (type-of (parse `null)) (t-nlist))
(test (type-of (parse `true)) (t-bool))
(test (type-of (parse `false)) (t-bool))
(test (type-of (parse `(zero? 5))) (t-bool))
(test (type-of (parse `(cons 5 null))) (t-nlist))
(test (type-of (parse `(first null))) (t-num))
(test (type-of (parse `(rest null))) (t-nlist))
(test (type-of (parse `(empty? null))) (t-bool))


(test/exn (type-of (parse `x)) "lookup-env: undefined variable")
(test/exn (type-of (parse `(fun (x : boolean) : number (- x 5)))) "type-of: Number expected")
(test/exn (type-of (parse `(2 5))) "type-of: Function expected")
(test/exn (type-of (parse `((fun (x : number) : number (- x 5)) true))) "type-of: Function input arg mismatch")
(test/exn (type-of (parse `(+ true false))) "type-of: Number expected")
(test/exn (type-of (parse `(fun (x : number) : boolean (+ x 1)))) "type-of: Body and body-type mismatch")
(test/exn (type-of (parse `(cons 5 5))) "type-of: List expected")
(test/exn (type-of (parse `(first 5))) "type-of: List expected")
(test/exn (type-of (parse `(rest 5))) "type-of: List expected")
(test/exn (type-of (parse `(empty? 5))) "type-of: List expected")
(test/exn (type-of (parse `(zero? null))) "type-of: Number expected")
(test/exn (type-of (parse `(if 1 1 1))) "type-of: Boolean expected")
(test/exn (type-of (parse `(with (x 5) y))) "lookup-env: undefined variable")
(test/exn (type-of (parse `(if true 1 true))) "type-of: Then and else type mismatch")



