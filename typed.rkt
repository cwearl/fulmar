#lang typed-racket/minimal

(require typed/racket
         racket/require
         fulmar/private/fulmar-core
         fulmar/version)

(provide (except-out (all-from-out typed/racket) #%module-begin #%top-interaction empty)
         (rename-out
          [fulmar-module-begin #%module-begin]
          [fulmar-top-interaction #%top-interaction]))

(module reader syntax/module-reader
  #:language 'fulmar/typed)

; This macro manipulates expressions entered in the REPL
(define-syntax-rule (fulmar-top-interaction f ...)
  (print-values (f ...)))

; This macro manipulates the module body.
(define-syntax (fulmar-module-begin stx)
  (syntax-case stx ()
    [(_ a ...)
     (with-syntax ([racket/base (datum->syntax stx 'racket/base)]
                   [fulmar/standard-chunk (datum->syntax stx 'fulmar/standard-chunk)])
       #'(#%module-begin
          ; Use a configure-runtime submodule to print the generated string before module execution.
          (module configure-runtime racket/base
            (require racket/format
                     fulmar/version)
            (display generated-string))
          ; Bring in stuff we need for srcdoc documentation. Because we do want to introduce bindings, we need to use a mixed hygiene macro.
          (require
            fulmar/standard-chunk)
          a
          ...))]))

(current-print print-values)
