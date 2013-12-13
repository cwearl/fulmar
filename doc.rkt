#lang racket
(require
 racket/contract
 scribble/srcdoc
 scribble/manual
 (for-doc racket/base 
          scribble/manual)
 )

(provide define/fmr
         (all-from-out racket/contract
                       scribble/srcdoc 
                       )
         )

(define-syntax define/fmr
  (syntax-rules ()
    [(define/fmr (id a ...) contract doc body)
     (begin
       (provide
        (proc-doc/names
         id
         contract
         (a ...)
         doc
         )
        )
       (define (id a ...)
         body)
       )]
    [(define/fmr id contract doc body)
     (begin
       (provide
        (thing-doc
         id
         contract
         doc
         )
        )
       (define id body)
       )]
    ))


