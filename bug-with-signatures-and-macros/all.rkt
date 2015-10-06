#lang racket
(module base-sig racket/base
  (require racket/unit)
  ;(require (only-in racket/function thunk)) ; Omitting this line crashes the compiler
  
  (define-signature reify^
    (reify-thunk
     (define-syntaxes (reify)
       (syntax-rules ()
         [(_ e)
          (reify-thunk (thunk e))]))
     ))
  
  (provide reify^))

(module base racket/base
  (require racket/unit)
  (require racket/control)
  (require (submod ".." base-sig))
  
  (define-unit reify@
    (import)
    (export reify^)
    (define (reify-thunk computation)
      (reset (computation)))
    )
  (provide reify@))

(module client racket
  (require (submod ".." base))
  (define-values/invoke-unit/infer reify@)
  (reify 0)
  )