#lang racket
(require net/url)
(require net/head)

(define hostname "ps.informatik.uni-tuebingen.de")
(define path "/teaching/ws15/info1/handinserver/")
(define url-string (format "http://~a~a" hostname path))
(define my-url (string->url url-string))
(system (format "wget ~a" url-string))

;(require net/http-client)
(define-values (status headers response) (http-sendrecv/url my-url))
(define last-modified-textual-date (ormap (lambda (h) (extract-field #"Last-Modified" h)) headers))

#|
(define-values ([status headers response (http-sendrecv hostname path)])
  )
(let* ([hc (http-conn-open hostname)]
       )
  (http-conn-open hc hostname)
  
  )
;http://ps.informatik.uni-tuebingen.de/teaching/ws15/info1/handinserver/
|#