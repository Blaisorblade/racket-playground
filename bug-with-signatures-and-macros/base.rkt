#lang racket/base
(require racket/unit)
(require racket/control)
(require "base-sig.rkt")

(define-unit reify@
  (import)
  (export reify^)
  (define (reify-thunk computation)
    (reset (computation)))
)
(provide reify@)