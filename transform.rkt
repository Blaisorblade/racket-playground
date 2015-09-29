#lang racket
(require "symbol-utils.rkt")
(require "a-normalize.rkt")
(module+ test
  (require rackunit))

(module+ test
  (check-equal? ((symbol-format "foo~a") 'bar) 'foobar))

(define (name-mapper fun)
  (let ([hash (make-hash)])
    (λ (name) (hash-ref! hash name (gensym-preserving (fun name))))))

(define/match (der-namer args)
  [((cons fName resName))
   (string->symbol (format "der_~a_~a" fName resName))])
(define d-mapper (name-mapper (symbol-format "d_~a")))
(define pair-name-mapper (name-mapper (symbol-format "~a_p")))
(define der-mapper (name-mapper der-namer))

; Racket-specific, to compute arity of primitives like + in example.
(define base-namespace (make-base-namespace))
(define (quoted-primitive-arity prim) (procedure-arity (eval prim base-namespace)))

; We need a type-checker to check if applications are full... or at least, an arity-checker.
; Handle global functions:
(define arity-hash (make-hash))
(define (set-arity! fun arity) (hash-set! arity-hash fun arity))
(define (check-arity? fun arity) (arity-includes? (hash-ref arity-hash fun (quoted-primitive-arity fun)) arity))
; What about partially-applied functions? Not that they're possible in this syntax...

(define d-pair-name-mapper (name-mapper (symbol-format "d_~a_p")))
(define der-mapper-p (name-mapper (compose (symbol-format "~a'") der-namer)))

(module+ test
  (define (mapped-foo) (d-mapper 'foo))
  (check-equal? (mapped-foo) (mapped-foo))
  (check-pred (λ (sym) (regexp-match #rx"^d_foo" (symbol->string sym))) (mapped-foo)))

(define (var? v) (symbol? v))
(define (derivePVar sym) (d-mapper sym))
(define (deriveP t)
  (match t
    [(? var?) (derivePVar t)]
    [`(let ([,x (,f . ,args)]) ,body)
     `(let ([,(derivePVar x) (,(der-mapper (cons f x)) ,@(map deriveP args))]) ,(deriveP body))]
    [`(let ([,x ,(? Value? v)]) ,body)
     `(let ([,(derivePVar x) ,(deriveP v)]) ,(deriveP body))]
    [(? Value?)
     t] ; XXX That's how we derive primitive values. Apparently, we're using replacement values always :-D.
    ))

; Currently params only contains derivatives, not base values.
; This will probably fail for non-self-maintainable primitives, unless replacement changes are involved.
(define (deriveAutomatonGoExpr name t)
  (let go ([t t] [params '()] [k (λ (x) x)])
    (match t
      [`(let ([,x (,f ,args ...)]) ,body) #:when (check-arity? f (length args)) ; XXX
       (let ([dxp (d-pair-name-mapper x)]
             [dx (d-mapper x)]
             [derX (der-mapper (cons f x))]
             [derXp (der-mapper-p (cons f x))])
         (go body (cons (cons derX derXp) params)
             (λ (content)
               (k `(let ([,dxp (,derX ,@(map deriveP args))]
                         [,dx (car ,dxp)]
                         [,derXp (cdr ,dxp)])
                     ,content)))))]
      [(? var?) (values (k `(cons ,(d-mapper t) (,name ,@(map cdr params)))) (map car params))] ; XXX apply name to updated params
      [(? Value?) (values (k t) (map car params))])))

#;(define (deriveAutomaton t)
  (let ([name (gensym 'automaton)])
    `(letrec ([,name ,(deriveAutomatonGoExpr name t)])
       ,name)))

; Changed for the "automaton" variant.
(define (deriveDecl t)
  (match t
    [`(λ (,args ...) ,fun-body)
     (let*-values ([(name) (gensym-preserving 'automaton)]
                   [(automaton-body params) (deriveAutomatonGoExpr name fun-body)]
                   [(automaton) `(λ (,@(append params (map derivePVar args))) #;,(deriveP fun-body) ,automaton-body)])
       `(letrec ([,name ,automaton])
          (,name . ,params)))
     ;`(λ (,@(map derivePVar args)) ,(deriveP fun-body))
     ]))
; (fix automaton.
;  (λ (fvs) (dArgs) derivative... (dRes, automaton newFVs)) fvs)

(define (cacheExpr toDerive t)
  (let go ([t t])
    (match t
      ; XXX Restrict this to the case f is fully applied and isn't a special primitive (see Value?)
      [`(let ([,x (,f ,args ...)]) ,body) #:when (check-arity? f (length args))
       (let ([xp (pair-name-mapper x)]
             [derX (der-mapper (cons f x))])
         `(let ([,xp (,f ,@args)]
                [,x (car ,xp)]
                [,derX (cdr ,xp)])
            ,(go body)))]
      [(? var?) `(cons ,t ,(deriveDecl toDerive))])))

(define (cacheDecl t)
  (match t
    [`(let ([,f (λ (,args ...) ,fun-body)]) ,body)
     (set-arity! f (length args))
     `(let ([,f (λ (,@args) ,(cacheExpr `(λ (,@args) ,fun-body) fun-body))]) ,(cacheDecl body))]
    [else (cacheExpr `(λ (,(gensym-preserving 'unit)) #f) t)])) ;#f is just a dummy.

;; Examples

(deriveP '(let ([x (+ 1 2)]) x))
(define intermediate (normalize-term '(+ 1 (+ 2 3) (+ 4 5))))
intermediate ; ==>
'(let ((g1 (+ 2 3))) (let ((g2 (+ 4 5))) (+ 1 g1 g2)))
; Since standard-a-normal-form is disabled, we can run directly:
(deriveP intermediate)

(cacheDecl '(let ([f (λ (x1 x2) (let ([res (+ x1 x2)]) res))]) f))
; ==>
#; '(let ((f
        (λ (x1 x2)
          (let ((res_p (+ x1 x2)) (res (car res_p)) (der_+_res (cdr res_p)))
            (cons res (λ (d_x1 d_x2) (let ((d_res (der_+_res d_x1 d_x2))) d_res)))))))
   (cons f (λ (d_unit) #f)))

#|
; Example of desired output:

let y = iszero? x
in y
=>
let
 y_p = iszero? x
 y = fst y_p
 der_iszero?_y = snd y_p
in (y, letrec aut =
  (λ der_iszero?_y. λ d_x. ; Also x was bound -- that was the result of lambda-lifting.
    let
      d_y_p = der_iszero?_y d_x
      d_y = fst d_y_p
      der_iszero?_y' = snd d_y_p
    in
      (cons d_y (aut der_iszero?_y'))) ; Also x ⊕ d_x was passed.
  in aut)
|#
