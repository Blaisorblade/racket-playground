#lang racket
; Adapted from http://matt.might.net/articles/a-normalization/, with some cosmetic changes.
(provide (all-defined-out))
(define (Value? M)
 (match M
   [`(quote ,_) #t]
   [(? number?) #t]
   [(? boolean?) #t]
   [(? string?) #t]
   [(? char?) #t]
   [(? symbol?) #t]
   [(or '+ '- '* '/ '=) #t]
   [else #f]))

(define (normalize-term M)
  (normalize M (λ (x) x)))

; If #t, we allow expressions in tail position, rather than just identifiers.
(define standard-a-normal-form #f)
(define (normalize M k)
  (match M
    [`(λ ,params ,body)
     (k `(λ ,params ,(normalize-term body)))]
    [`(let ([,x ,M1] ,M2))
     (normalize M1 (λ (N1)
                     `(let ([,x ,N1]) ,(normalize M2 k))))]
    [`(,ids ...)
     (normalize-name*
      ids
      (λ (t*)
        (if standard-a-normal-form
            (k t*)
            (insert-name `(,@t*) k))))]
    [(? Value?)
     (k M)]
    ))

(define (insert-name N k)
  (let ([t (gensym)])
    `(let ([,t ,N]) ,(k t))))

(define (normalize-name M k)
  (normalize
   M
   (λ (N)
     (if (Value? N)
         (k N)
         (insert-name N k)))))

(define (normalize-name* M* k)
  (if (null? M*)
      (k '())
      (normalize-name
       (first M*)
       (λ (t)
         (normalize-name*
          (rest M*)
          (λ (t*)
            (k `(,t . ,t*))))))))

(module+ test
  (normalize-term '(+ 1 (+ 2 3) (+ 4 5))))
