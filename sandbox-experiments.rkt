#lang racket
(require 2htdp/image)
(require racket/sandbox)

(sandbox-input        #f)
(sandbox-output       #f)
(sandbox-error-output #f)

(define evaler (make-module-evaluator '(module foo racket (require htdp/image) (require net/http-client) 1) #:language 'racket #:allow-for-require '(2htdp/image)))
(define image (evaler '(begin (require htdp/image) (require net/http-client) (circle 20 "solid" "red"))))
image
(image? image)
