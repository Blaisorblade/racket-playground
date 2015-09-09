#lang racket

(module cake racket
  (provide print-cake)
  
  ; draws a cake with n candles
  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))
  
  (define (show fmt n ch)
    (printf fmt (make-string n ch))
    (newline)))

;(require 'cake) ;Works

(module random-cake racket
  (require (submod ".." cake))
  
  (print-cake (random 30)))