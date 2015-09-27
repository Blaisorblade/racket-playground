#lang racket
(require "a-normalize.rkt")
(module+ test
  (require rackunit))

(define ((symbol-format formatStr) symbol) (string->symbol (format formatStr symbol)))

(module+ test
  (check-equal? ((symbol-format "foo~a") 'bar) 'foobar))

(define (name-mapper fun)
  (let ([hash (make-hash)])
    (λ (name) (hash-ref! hash name (gensym ((symbol-format "~a:") (fun name)))))))

(define (der-namer args)
  (match args
    [`(,fName . ,resName)
     (string->symbol (format "der_~a_~a" fName resName))]))
(define d-mapper (name-mapper (symbol-format "d_~a")))
(define pair-name-mapper (name-mapper (symbol-format "~a_p")))
(define der-mapper (name-mapper der-namer))

(module+ test
  (define (mapped-foo) (d-mapper 'foo))
  (check-equal? (mapped-foo) (mapped-foo))
  (check-pred (λ (sym) (regexp-match #rx"^d_foo" (symbol->string sym))) (mapped-foo)))

(define (var? v) (symbol? v))
(define (derivePVar sym) (d-mapper sym))
(define (deriveP t)
  (match t
    [(? var?) (derivePVar t)]
    [`(let ([,x (,fn . ,args)]) ,body)
     `(let ([,(derivePVar x) (,(der-mapper (cons fn x)) ,@(map deriveP args))]) ,(deriveP body))]
    [`(let ([,x ,(? Value? v)]) ,body)
     `(let ([,(derivePVar x) ,(deriveP v)]) ,(deriveP body))]
    [(? Value?)
     t] ; XXX That's how we derive primitive values. Apparently, we're using replacement values always :-D.
    ))

(deriveP '(let ([x (+ 1 2)]) x))
(normalize-term '(+ 1 (+ 2 3) (+ 4 5))) ; ==>
'(let ((g1 (+ 2 3))) (let ((g2 (+ 4 5))) (+ 1 g1 g2)))
(deriveP '(let ((g1 (+ 2 3))) (let ((g2 (+ 4 5))) (let ([g3 (+ 1 g1 g2)]) g3))))

(define (deriveAutomatonGoExpr name t)
  (match t
    [(? var?) `(cons ,t ,name)]))
; XXX to change for the "automaton" variant.
(define (deriveDecl t)
  (match t
    [`[,f (λ (,args ...) ,fun-body)]
     `(λ (,@(map derivePVar args)) ,(deriveP fun-body))]))
; (fix automaton.
;  (λ (fvs) (dArgs) derivative... (dRes, automaton newFVs)) fvs)

(define (cacheExpr toDerive t)
  (let go ([t t])
    (match t
      ; XXX Restrict this to the case f is fully applied and isn't a special primitive (see Value?)
      [`(let ([,x (,f ,args ...)]) ,body)
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
     `(let ([,f (λ (,@args) ,(cacheExpr `[,f (λ (,@args) ,fun-body)] fun-body))]) ,(cacheDecl body))]
    [else (cacheExpr `[,(gensym) (λ (,(gensym 'unit)) #f)] t)])) ;XXX f is just a dummy.

(cacheDecl '(let ([f (λ (x1 x2) (let ([res (+ x1 x2)]) res))]) f))
; ==>
#; '(let ((f
        (λ (x1 x2)
          (let ((res_p (+ x1 x2)) (res (car res_p)) (der_+_res (cdr res_p)))
            (cons res (λ (d_x1 d_x2) (let ((d_res (der_+_res d_x1 d_x2))) d_res)))))))
   (cons f (λ (d_unit) #f)))
