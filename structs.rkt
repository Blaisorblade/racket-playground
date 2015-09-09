#lang racket

(require "util.rkt")
(struct posn (x y))

; Constructor
(show (posn 1 2))
;(show-def (define p (posn 1 2)))
;(show p)
(define-show p (posn 1 2))
; Type-Test
(show (posn? p))
; Accessors
(show (posn-x p))
(show (posn-y p))

(define-show p2 (struct-copy posn p [x 3]))
(show (posn-x p2))
(show (posn-y p2))

; Deprecated syntax
(define-struct posn2 (x y))

; We get an extra constructor, with a "make-" prefix, hence called make-posn2 instead of posn2.
(show (make-posn2 1 2))
(show (posn2 1 2))
