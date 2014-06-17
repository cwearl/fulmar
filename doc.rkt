#lang racket
(require
  racket/contract
  scribble/srcdoc
  (for-doc racket/base
           scribble/manual))

(provide define/doc
         (all-from-out racket/contract
                       scribble/srcdoc))

(define-syntax define/doc
  (syntax-rules ()
    [(define/doc (id a ...) contract doc body)
     (begin
       (provide
        (proc-doc/names
         id
         contract
         (a ...)
         doc))
       (define (id a ...)
         body))]
    [(define/doc id contract doc body)
     (begin
       (provide
        (thing-doc
         id
         contract
         doc))
       (define id body))]))
