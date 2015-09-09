#lang racket

(provide (all-defined-out))
(define-syntax show
  (syntax-rules ()
    [(show expr)
     (printf "~a: \"~a\"\n" 'expr expr)]))

(define-syntax show-def
  (syntax-rules ()
    [(show-def def)
     (begin
       (printf "~a\n" 'def)
       def)]))

#;(define-syntax define-show
  (syntax-rules ()
    [(define-show id val)
     (begin
       (show-def (define id val))
       (show id))]))

(define-syntax define-show
  (syntax-rules ()
    [(define-show id val)
     (begin
       (define id val)
       (printf "~a -> \"~a\"\n" '(define id val) id))]))