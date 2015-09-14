#lang racket
(require web-server/servlet)
(require web-server/compat/0/coerce)
(require xml)

(define template-path "index.html")
(define (template title content)
  (string-replace
   (string-replace
    (call-with-input-file template-path
      (lambda (in) (port->string in)))
    "TITLE" title)
   "CONTENT" content))

(can-be-response? (template "FOO" "BAR"))

;((response-output (any->response (template "FOO" "BAR"))) (current-output-port))
;(call-with-output-file "output.html" (lambda (out) (print

(define xexprs (list '(head) '(body)))
(define (tags-to-text xexprs)
  (apply string-append (map xexpr->string xexprs)))
(tags-to-text xexprs)
(tags-to-text '((head) (body)))
;(write-xexpr '(html (head)))