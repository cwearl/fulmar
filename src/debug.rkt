#lang racket

(define debug-flag #t)

(define-syntax define/debug
  (syntax-rules ()
    ((_ name contract body)
     ;(if (not debug-flag)
         (define/contract name any/c body)
         ;(define name body)
         )))
         ;(define/contract name contract body))))
  ;)
(provide define/debug)

(define (replace-contracts lst)
  (cond [(null? lst) null]
        [else (cons (cons (first (first lst))
                          any/c)
                    (replace-contracts (rest lst)))]))

(define-syntax provide-struct-out/debug
  (syntax-rules ()
    ((_ name contracts)
     ;(if (not debug-flag)
         (let ([new-contracts (replace-contracts contracts)])
           (provide (contract-out (struct name new-contracts))))
         ;(provide (contract-out (struct name (apply replace-contracts contracts))))
         ;(provide (struct-out name))
         )))
         ;(provide (contract-out (struct name contracts))))))
  ;)
(provide provide-struct-out/debug)

;(define-syntax define/debug
;  (syntax-rules ()
;    ((_ name contract body)
;     ;(if (not debug-flag)
;         (define name body)
;         )))
;         ;(define/contract name contract body))))
;  ;)
;(provide define/debug)
;
;(define-syntax provide-struct-out/debug
;  (syntax-rules ()
;    ((_ name contracts)
;     ;(if (not debug-flag)
;         (provide (struct-out name))
;         )))
;         ;(provide (contract-out (struct name contracts))))))
;  ;)
;(provide provide-struct-out/debug)