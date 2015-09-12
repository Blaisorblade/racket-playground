#lang slideshow
(define c (circle 10))
(define r (rectangle 10 20))
(define (square n)
  (filled-rectangle n n))
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))
(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

(checkerboard (square 20))
;(four (circle 10))
(require slideshow/code)
(require pict/flash)
(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

;(rainbow (square 5))
(apply vc-append (append '(5) (rainbow (square 10))))
(circle 10)
(apply vc-append `(5 . ,(rainbow (square 10))))
(circle 10)
(apply vc-append 5 (rainbow (square 10)))

(code (circle 10))

(require racket/class
         racket/gui/base)
(define f (new frame% [label "Chess"]
                      [width 300]
                      [height 300]
                      [alignment '(center center)]))
(send f show #t)
(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
                 [style '(border)]
                 [paint-callback (lambda (self dc)
                                   (drawer dc 0 0))])))
(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))
(add-drawing (checkerboard (square 30)))
#|
(add-drawing (pict+code (circle 10)))
(add-drawing (pict+code (colorize (filled-flash 50 30) "yellow")))
(add-drawing (pict+code (colorize (filled-flash 50 30) "black")))
|#