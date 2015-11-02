#lang racket

(provide (all-defined-out))
(define-syntax-rule (format-show expr)
  (format "~a: \"~a\"" 'expr expr))

(define-syntax-rule (show expr)
  (printf "~a\n"(format-show expr)))

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

(module+ test
  (require rackunit)
  (check-equal? (format-show (+ 1 2)) "(+ 1 2): \"3\""))
