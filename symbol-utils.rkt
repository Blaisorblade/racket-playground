#lang racket
(provide (all-defined-out))

(define ((symbol-format formatStr) symbol) (string->symbol (format formatStr symbol)))

(define (gensym-preserving orig)
  (gensym ((symbol-format "~a:") orig)))
