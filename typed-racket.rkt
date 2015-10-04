#lang typed/racket/base
(require typed-racket/types/numeric-tower)

(: a Positive-Integer)
(define a 15)
;(: c Positive-Integer)
(: b Positive-Integer)
(define b 20)
;(: c Positive-Integer)
;(define c (- b a))
(: d Positive-Integer)
(define d (assert (- b a) positive?))
;(: flexible-length (-> (U String (Listof Any)) Integer))

(: id-positive (-> Positive-Integer Positive-Integer))
(define (id-positive x) x)

(: my-positive? (-> Real Boolean : #:+ Positive-Real #:- Nonpositive-Real))
(define (my-positive? x)
  (positive? x))

(: signcase-id (-> Integer Integer))
(define (signcase-id num)
  (if (my-positive? num)
      (id-positive num)
      num))


(: id-positive-real (-> Positive-Real Positive-Real))
(define (id-positive-real x) x)

(: signcase-id-real (-> Real Real))
(define (signcase-id-real num)
  (if (positive? num)
      (id-positive-real num)
      num))