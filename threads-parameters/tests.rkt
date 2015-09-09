#lang racket

#|
(define tls (make-thread-cell 1))


(displayln "This is the original thread")
(thread (lambda () (displayln "This is a new thread.")))
(thread (lambda () (displayln "This is another new thread.")))

(define worker (thread (lambda ()
                         (let loop ()
                           (displayln "Working...")
                           (sleep 0.2)
                           (loop)))))
(sleep 2.5)
(kill-thread worker)
|#

; From http://docs.racket-lang.org/reference/threadcells.html#%28def._%28%28quote._~23~25kernel%29._thread-cell-values~3f%29%2

(define cnp (make-thread-cell '(nerve) #f))
(define cp (make-thread-cell '(cancer) #t))
(define ch (make-channel))
#|
(thread-cell-ref cnp)
;'(nerve)
(thread-cell-ref cp)
;'(cancer)
(thread-cell-set! cnp '(nerve nerve))
(thread-cell-set! cp '(cancer cancer))
(thread-cell-ref cnp)
;'(nerve nerve)
(thread-cell-ref cp)
;'(cancer cancer)
(thread (lambda ()
            (channel-put ch (thread-cell-ref cnp))
            (channel-put ch (thread-cell-ref cp))
            (channel-get ch)
            (channel-put ch (thread-cell-ref cp))))
; #<thread>
(channel-get ch)
; '(nerve)
(channel-get ch)
; '(cancer cancer)
(thread-cell-set! cp '(cancer cancer cancer))
(thread-cell-ref cp)
; '(cancer cancer cancer)
(channel-put ch 'ok)
(channel-get ch)
; '(cancer cancer)
|#