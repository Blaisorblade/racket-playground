#lang racket
(require "symbol-utils.rkt")
(require "a-normalize.rkt")
(module+ test
  (require rackunit))

(module+ test
  (check-equal? ((symbol-format "foo~a") 'bar) 'foobar))

(define (name-mapper fun)
  (let ([hash (make-hash)]) ; make-hasheq only works here if the input is hash-consed, e.g. for symbols, but breaks for der-mapper.
    (λ (name)
      (hash-ref! hash name (gensym-preserving (fun name))))))

(define/match (der-namer args)
  [((cons fName resName))
   (string->symbol (format "der_~a_~a" fName resName))])
(define d-mapper (name-mapper (symbol-format "d_~a")))
(define pair-name-mapper (name-mapper (symbol-format "~a_p")))
(define der-mapper (name-mapper der-namer))

(define func-mapper-hash (make-hasheq))
; XXX duplication from name-mapper!
(define func-mapper
  (let ([fun (symbol-format "~a/cached")]
        [hash func-mapper-hash])
    (λ (name)
      (hash-ref! hash name (gensym-preserving (fun name))))))

; Racket-specific, to compute arity of primitives like + in example.
(define base-namespace (make-base-namespace)) ; This is just racket/base

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
  (let go ([t t] [params '()] [kontext (λ (x) x)])
    (match t
      [`(let ([,x (,f ,args ...)]) ,body)
       #:when (check-arity? f (length args)) ; XXX
       (let ([dxp (d-pair-name-mapper x)]
             [dx (d-mapper x)]
             [derX (der-mapper (cons f x))]
             [derXp (der-mapper-p (cons f x))])
         (go body (cons (cons derX derXp) params)
             (λ (content)
               (kontext
                `(let ([,dxp (,derX ,@(map deriveP args))])
                   (let ([,dx (car ,dxp)])
                     (let ([,derXp (cdr ,dxp)])
                       ,content)))))))]
      [(? var?) (values (kontext `(cons ,(d-mapper t) (,name ,@(map cdr params)))) (map car params))]
      [(? Value?) (values (kontext t) (map car params))])))

; Changed for the "automaton" variant.
(define (deriveDecl t)
  (match t
    [`(λ (,args ...) ,fun-body)
     (let*-values ([(name) (gensym-preserving 'make-automaton)]
                   [(automaton-body params) (deriveAutomatonGoExpr name fun-body)])
       `(letrec ([,name (λ (,@params) (λ (,@(map derivePVar args)) #;,(deriveP fun-body) ,automaton-body))])
          (,name . ,params)))
     ]))
; (fix automaton.
;  (λ (fvs) (dArgs) derivative... (dRes, automaton newFVs)) fvs)

(define (cacheExpr toDerive t)
  (let go ([t t])
    (match t
      ; XXX Restrict this to the case f is fully applied and isn't a special primitive (see Value?)
      [`(let ([,x (,f ,args ...)]) ,body)
       #:when (check-arity? f (length args))
       (let ([xp (pair-name-mapper x)]
             [derX (der-mapper (cons f x))])
         `(let ([,xp (,(func-mapper f) ,@args)])
            (let ([,x (car ,xp)])
              (let ([,derX (cdr ,xp)])
                ,(go body)))))]
      [(? var?)
       (let ([mapped-var-name (hash-ref func-mapper-hash t t)]) ; We need to only map function names, but nothing else.
         `(cons ,mapped-var-name ,(deriveDecl toDerive)))])))

(define (cacheDecl t)
  (match t
    [`(let ([,f (λ (,args ...) ,fun-body)]) ,body)
     (set-arity! f (length args))
     `(let ([,(func-mapper f) (λ (,@args) ,(cacheExpr `(λ (,@args) ,fun-body) fun-body))]) ,(cacheDecl body))]

    [else (cacheExpr `(λ (,(gensym-preserving 'unit)) #f) t)])) ;#f is just a dummy.

;; Examples

(module+ test
  (define intermediate (normalize-term '(+ 1 (+ 2 3) (+ 4 5))))
  ;intermediate ; ==>
  ;'(let ((g1 (+ 2 3))) (let ((g2 (+ 4 5))) (+ 1 g1 g2)))
  ; Since standard-a-normal-form is disabled, we can run directly:
  (define d-intermediate (deriveP intermediate)))

; Derivative of plus. By chance, this works with either replacement changes or additive changes.
(define (d_+ d_a d_b) (+ d_a d_b))
; "Caching +" (with an empty cache), similar to the output produced by the transformation. Many primitives could be similarly incrementalized.
; Since there's no state, we could turn make-automaton into a constant.
(define (+/cached a b)
  (cons
   (+ a b)
   (letrec ([make-automaton (λ ()
                              (λ (d_a d_b)
                                (cons (d_+ d_a d_b) (make-automaton))))])
     (make-automaton))))

(module+ test
  (check-equal? (car (+/cached 1 2)) 3)
  (check-equal? (car ((cdr (+/cached 1 2)) 4 5)) 9))

; XXX Hack to ensure the output uses +/cached as the name of the augmented +, instead of +/cached:NNN.
(hash-set! func-mapper-hash '+ '+/cached)

; XXX Load the namespace of the current module. Currently we just need +/cached in.
(define-namespace-anchor this-module-namespace-anchor)

(define this-module-namespace
  (namespace-anchor->namespace this-module-namespace-anchor))

(define example-1
  (cacheDecl
   '(let ([f (λ (x1 x2)
               (let ([res (+ x1 x2)])
                 res))])
      f)))
; ==>
'(let ((f/cached
        (λ (x1 x2)
          (let* ((res_p (+/cached x1 x2))
                 (res (car res_p))
                 (der_+_res (cdr res_p)))
            (cons
             res
             (letrec ([make-automaton (λ (der_+_res)
                                        (λ (d_x1 d_x2)
                                          (let* ((d_res_p (der_+_res d_x1 d_x2))
                                                 (d_res (car d_res_p))
                                                 (|der_+_res'| (cdr d_res_p)))
                                            (cons d_res (make-automaton |der_+_res'|)))))])
               (make-automaton der_+_res)))))))
   (cons f/cached (letrec ((make-automaton-2 (λ () (λ (d_unit) #f))))
                    (make-automaton-2))))

(define example-2
  (cacheDecl
   '(let ([g (λ (x1 x2)
               (let ([res (+ x1 x2)])
                 (let ([foo (+ res x1)])
                   foo)))])
      g)))
(define (eval-example s-expr)
  (eval s-expr this-module-namespace))

example-1
example-2
(define example-1-eval (eval-example example-1))
(define example-2-eval (eval-example example-2))

; For testing.
(define (step example-eval)
  (let*
      ([step-1 ((car example-eval) 1 2)]
       [step-2 ((cdr step-1) 5 6)]
       [step-3 ((cdr step-2) 7 8)])
    (values step-1 step-2 step-3)))


(module+ test
  (define-values (ex-1-step-1 ex-1-step-2 ex-1-step-3) (step example-1-eval))
  (define-values (ex-2-step-1 ex-2-step-2 ex-2-step-3) (step example-2-eval))
  ex-1-step-1 ex-1-step-2 ex-1-step-3
  ex-2-step-1 ex-2-step-2 ex-2-step-3
  )

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
