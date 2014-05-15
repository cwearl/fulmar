#lang racket

(require "private/core-chunk.rkt"
         "private/fulmar-core.rkt"
         "version.rkt"
         "standard-chunk.rkt"
         "doc.rkt")

(provide (except-out (all-from-out racket) #%module-begin #%top-interaction )
         (all-from-out "standard-chunk.rkt" "doc.rkt")
         (rename-out 
          [fulmar-module-begin #%module-begin]
          [fulmar-top-interaction #%top-interaction]))

; Provide a reader supporting at-expressions for srcdoc.
(module reader syntax/module-reader
  #:language 'fulmar
  (require (only-in scribble/reader use-at-readtable))
  (use-at-readtable))

; Compile and print the passed chunks.
(define (print-values vs)
  (for-each displayln
   (reverse (write-chunk vs))))

; This macro manipulates expressions entered in the REPL
(define-syntax-rule (fulmar-top-interaction f ...)
  (print-values (f ...)))

; This macro manipulates the module body.
(define-syntax (fulmar-module-begin stx)
  (syntax-case stx ()
    [(_ a ...)
     (with-syntax ([racket/base (datum->syntax stx 'racket/base)]
                   [scribble/manual (datum->syntax stx 'scribble/manual)])
       #'(module-begin
          ; Use a configure-runtime submodule to print the generated string before module execution.
          (module configure-runtime racket/base
            (require racket/format
                     fulmar/version)
            (display generated-string))
          ; Bring in stuff we need for srcdoc documentation. Because we do want to introduce bindings, we need to use a mixed hygiene macro.
          (require
            (for-doc
             racket/base
             scribble/manual))
          a
          ...))]))

; Everything below taken from collects/racket/private/modbeg.rkt. Wraps expressions but not statements in print-values.
(define-syntaxes (module-begin)
  (lambda (stx)
    (if (eq? 'module-begin (syntax-local-context))
        (void)
        (raise-syntax-error
         #f
         "allowed only around a module body"
         stx))
    (if (symbol? (syntax-e stx))
        (raise-syntax-error
         #f
         "bad syntax" 
         stx)
        (void))
    (let-values ([(l) (syntax->list stx)])
      (if l
          (void)
          (raise-syntax-error
           #f
           "bad syntax (illegal use of `.')" 
           stx))
      (datum->syntax
       stx
       (cons (quote-syntax #%printing-module-begin)
             (map (lambda (e)
                    (list (quote-syntax printing-module-begin)
                          e))
                  (cdr l)))
       stx
       stx))))

(define-syntaxes (printing-module-begin)
  (lambda (stx)
    (let-values ([(r) (cdr (syntax-e stx))])
      (let-values ([(r) (if (syntax? r)
                            (syntax-e r)
                            r)])
        (if (null? r)
            (quote-syntax (void))
            (let-values ([(e) (local-expand (car r)
                                            'module
                                            (syntax->list
                                             (quote-syntax 
                                              (quote 
                                               quote-syntax #%top
                                               lambda case-lambda
                                               let-values letrec-values
                                               begin begin0 set!
                                               with-continuation-mark
                                               if #%app #%expression
                                               define-values define-syntaxes begin-for-syntax
                                               module module*
                                               #%module-begin 
                                               #%require #%provide 
                                               #%variable-reference))))])
              ;; `begin' is special...
              (if (let-values ([(p) (syntax-e e)])
                    (if (pair? p)
                        (if (symbol? (syntax-e (car p)))
                            (if (free-identifier=? (car p) (quote-syntax begin))
                                (syntax->list e)
                                #f)
                            #f)
                        #f))
                  ;; splice `begin'
                  (let-values ([(l) (syntax->list e)])
                    (datum->syntax
                     stx
                     (cons (car l)
                           (append
                            (map (lambda (elem)
                                   (list
                                    (quote-syntax printing-module-begin)
                                    (syntax-track-origin elem e (car l))))
                                 (cdr l))
                            (cdr r)))
                     stx))
                  ;; no need to splice
                  (let-values ([(wrap?)
                                (let-values ([(e) (syntax-e e)])
                                  (if (pair? e)
                                      (let-values ([(a) (car e)])
                                        (if (symbol? (syntax-e a))
                                            (if (ormap (lambda (i)
                                                         (free-identifier=? i a))
                                                       (syntax->list
                                                        (quote-syntax 
                                                         (define-values define-syntaxes begin-for-syntax
                                                           module module*
                                                           #%module-begin 
                                                           #%require #%provide))))
                                                #f
                                                ;; Also check for calls to `void':
                                                (if (free-identifier=? a (quote-syntax #%app))
                                                    (let-values ([(e) (cdr e)])
                                                      (let-values ([(e) (if (syntax? e)
                                                                            (syntax-e e)
                                                                            e)])
                                                        (if (pair? e)
                                                            (if (symbol? (syntax-e (car e)))
                                                                (if (free-identifier=? (car e) (quote-syntax void))
                                                                    #f
                                                                    #t)
                                                                #t)
                                                            #t)))
                                                    #t))
                                            #t))
                                      #t))])
                    (let-values ([(e) (if wrap?
                                          (datum->syntax
                                           (quote-syntax here)
                                           (list (quote-syntax #%app)
                                                 (quote-syntax call-with-values)
                                                 (list (quote-syntax lambda)
                                                       '()
                                                       e)
                                                 (quote-syntax print-values))
                                           e)
                                          e)])
                      (datum->syntax
                       stx
                       (if (null? (cdr r))
                           (list (quote-syntax begin) e)
                           (list (quote-syntax begin)
                                 e
                                 (cons (quote-syntax printing-module-begin)
                                       (cdr r))))
                       stx))))))))))