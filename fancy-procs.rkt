#lang racket

(require "util.rkt")

(module+ test
  (show f)
  (newline))

#|
(define (f arg1 [arg2 #f] arg3 #:kw kw #:kw2 [kw2 #f])) ; Invalid, arg3 has no default and comes after an argument with defaults.
|#

(define (f arg1 [arg2 #f] [arg3 ""] #:kw kw #:kw2 [kw2 #f])
  (show arg1)
  (show arg2)
  (show arg3)
  (show kw)
  (show kw2)
  (newline))

(module+ test
  ; (f 1) ; Invalid, "required keyword argument not supplied" 
  (f 1 #:kw 2)
  (f 1 2 #:kw 3)
  (f 1 2 'foo #:kw 3 #:kw2 4))
