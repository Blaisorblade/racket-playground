#lang racket
(require "symbol-utils.rkt")
(require "a-normalize.rkt")
(module+ test
  (require rackunit))

(module+ test
  (check-equal? ((symbol-format "foo~a") 'bar) 'foobar))

(define (merge-maps x1 x2 maps)
  (match-let ([fresh (gensym)]
              [(cons map1 map2) maps])
    (cons (hash-set map1 x1 fresh) (hash-set map2 x2 fresh))))

(define (merge-maps* xs1 xs2 maps)
  (for/fold ([maps* maps])
            ([x1 xs1] [x2 xs2])
    (merge-maps x1 x2 maps*)))

; Restricted to our object language.
(define (alpha-equiv t1 t2)
  (let alpha-equiv-go ([t1 t1] [t2 t2] [maps (cons (make-immutable-hasheq) (make-immutable-hasheq))])
    (match* (t1 t2)
      [(`(let ([,x1 ,t1]) ,body1) `(let ([,x2 ,t2]) ,body2))
       (let ([maps* (merge-maps x1 x2 maps)])
         (and (alpha-equiv-go t1 t2 maps)
              (alpha-equiv-go body1 body2 maps*)))]
      [(`(letrec ((,x1 ,t1)) ,body1) `(letrec ((,x2 ,t2)) ,body2))
       (let ([maps* (merge-maps x1 x2 maps)])
         (and (alpha-equiv-go t1 t2 maps*)
              (alpha-equiv-go body1 body2 maps*)))]
      [(`(λ (,xs1 ...) ,body1) `(λ (,xs2 ...) ,body2))
       (let ([maps* (merge-maps* xs1 xs2 maps)])
         (and (equal? (length xs1) (length xs2))
              (alpha-equiv-go body1 body2 maps*)))]
      [(`(,ops1 ...) `(,ops2 ...))
       (and (equal? (length ops1) (length ops2))
            (andmap (λ (op1 op2) (alpha-equiv-go op1 op2 maps)) ops1 ops2))]
      [((? var?) (? var?))
       (match-let ([(cons map1 map2) maps])
         (equal? (hash-ref map1 t1 t1) (hash-ref map2 t2 t2)))]
      [((? Value?) (? Value?))
       (equal? t1 t2)]
      [(else else)
       (printf "alpha-equivalence can't compare ~a and ~a" t1 t2)
       #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name transformers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((name-mapper* fun hash) name)
  (hash-ref! hash name (gensym-preserving (fun name))))
(define (name-mapper fun) (name-mapper* fun (make-hasheq)))

(define d-mapper (name-mapper (symbol-format "d_~a")))
(define pair-name-mapper (name-mapper (symbol-format "~a_p")))
(define d-pair-name-mapper (name-mapper (symbol-format "d_~a_p")))

; Trickier name transformers
(define/match (der-namer args)
  [((cons fName resName))
   (string->symbol (format "der_~a_~a" fName resName))])
(define der-mapper (name-mapper* der-namer (make-hash))) ; make-hasheq only works here if the input is hash-consed, e.g. for symbols, but breaks here, so use make-hash.
(define der-mapper-p (name-mapper* (compose (symbol-format "~a'") der-namer) (make-hash))) ;Ditto.

; Later we need access to this hash, so we expose it.
(define func-mapper-hash (make-hasheq))
(define func-mapper (name-mapper* (symbol-format "~a/cached") func-mapper-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computing arities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Racket-specific, to compute arity of primitives like + in example.
(define base-namespace (make-base-namespace)) ; This is just racket/base

(define (quoted-primitive-arity prim) (procedure-arity (eval prim base-namespace)))

; We need a type-checker to check if applications are full... or at least, an arity-checker.
; Handle global functions:
(define arity-hash (make-hash))
(define (set-arity! fun arity) (hash-set! arity-hash fun arity))
(define (check-arity? fun arity) (arity-includes? (hash-ref arity-hash fun (quoted-primitive-arity fun)) arity))
; What about partially-applied functions? Not that they're possible in this syntax...

(module+ test
  (define (mapped-foo) (d-mapper 'foo))
  (check-equal? (mapped-foo) (mapped-foo))
  (check-pred (λ (sym) (regexp-match #rx"^d_foo" (symbol->string sym))) (mapped-foo)))

(define (var? v) (symbol? v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual transforms.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(define (deriveAutomatonGoExpr automaton-name t)
  (let go ([t t] [automaton-state-old-new-var-pairs '()] [kontext (λ (x) x)])
    (match t
      [`(let ([,x (,f ,args ...)]) ,body)
       #:when (check-arity? f (length args)) ; XXX
       (let ([dxp (d-pair-name-mapper x)]
             [dx (d-mapper x)]
             [derX (der-mapper (cons f x))]
             [derXp (der-mapper-p (cons f x))])
         (go body (cons (cons derX derXp) automaton-state-old-new-var-pairs)
             (λ (content)
               (kontext
                `(let ([,dxp (,derX ,@(map deriveP args))])
                   (let ([,dx (car ,dxp)])
                     (let ([,derXp (cdr ,dxp)])
                       ,content)))))))]
      [(? var?) (values (kontext `(cons ,(d-mapper t) (,automaton-name ,@(map cdr automaton-state-old-new-var-pairs)))) (map car automaton-state-old-new-var-pairs))]
      [(? Value?) (values (kontext t) (map car automaton-state-old-new-var-pairs))])))

; Changed for the "automaton" variant.
(define (deriveDecl t)
  (match t
    [`(λ (,args ...) ,fun-body)
     (let*-values ([(automaton-name) (gensym-preserving 'make-automaton)]
                   [(automaton-body automaton-state-vars) (deriveAutomatonGoExpr automaton-name fun-body)])
       `(letrec ([,automaton-name (λ (,@automaton-state-vars) (λ (,@(map derivePVar args)) ,automaton-body))])
          (,automaton-name . ,automaton-state-vars)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(module+ test
  (check
   alpha-equiv
   example-1
   '(let ((f/cached
           (λ (x1 x2)
             (let ((res_p (+/cached x1 x2)))
               (let ((res (car res_p)))
                 (let ((der_+_res (cdr res_p)))
                   (cons
                    res
                    (letrec ([make-automaton (λ (der_+_res)
                                               (λ (d_x1 d_x2)
                                                 (let ((d_res_p (der_+_res d_x1 d_x2)))
                                                   (let ((d_res (car d_res_p)))
                                                     (let ((|der_+_res'| (cdr d_res_p)))
                                                       (cons d_res (make-automaton |der_+_res'|)))))))])
                      (make-automaton der_+_res)))))))))
      (cons f/cached (letrec ((make-automaton-2 (λ () (λ (d_unit) #f))))
                       (make-automaton-2))))))

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
