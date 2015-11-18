#lang racket

(require "private/core-chunk.rkt"
         "private/fulmar-core.rkt"
         "version.rkt"
         "standard-chunk.rkt"
         "doc.rkt"
         syntax/wrap-modbeg)

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
(define-syntax-rule (fulmar-module-begin a ...)
  (fulmar-wrapping-module-begin
    ; use a configure-runtime submodule to print the version info once and only once - not once
    ; per involved #lang fulmar file (as would be the case if we just added a side effect here)
    (module configure-runtime racket/base
      (require racket/format
               fulmar/version)
      (display generated-string))
    a
    ...))

(define-syntax fulmar-wrapping-module-begin
  (make-wrapping-module-begin
    #'print-values))
