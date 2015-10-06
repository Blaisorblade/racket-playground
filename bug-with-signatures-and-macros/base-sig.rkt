#lang racket/base
(require racket/unit)
;(require (only-in racket/function thunk)) ; Omitting this line crashes the compiler
 
(define-signature reify^
  (reify-thunk
   (define-syntaxes (reify)
     (syntax-rules ()
       [(_ e)
        (reify-thunk (thunk e))]))
))
 
(provide reify^)
