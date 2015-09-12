#lang racket
(require net/git-checkout)
(require pkg/lib)

(define (get-pkg-checksum pkg-name)
   (define info
     (for/or ([scope (in-list (get-all-pkg-scopes))])
       (hash-ref (installed-pkg-table #:scope scope) pkg-name #f)))
   (and info (pkg-info-checksum info)))

(equal? (git-checkout "github.com" "ps-tuebingen/handin" #:dest-dir #f #:ref "deploy-production") (get-pkg-checksum "utue-info1-ws15"))