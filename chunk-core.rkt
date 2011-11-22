#lang racket

(require "fulmar-core.rkt")

;fulmar core chunks - these directly build nekots

;;;;;;;;;;;;;;;;;;;
;;helper functions;
;;;;;;;;;;;;;;;;;;;

;combine lengths of given values
(define/contract (combine-lengths . values)
  (->* () #:rest (listof (or/c string? symbol? natural-number/c)) natural-number/c)
  (foldl (λ (value total)
           (+ total
              (cond [(integer? value) value]
                    [(string? value) (string-length value)]
                    [(symbol? value) (string-length (symbol->string value))])))
         0
         values))
(provide combine-lengths)

;combine strings
(define/contract (combine-strings . values)
  (->* () #:rest (listof (or/c string? symbol?)) string?)
  (foldl (λ (str total)
           (string-append total
                          (cond [(string? str) str]
                                [(symbol? str) (symbol->string str)])))
         ""
         values))
(provide combine-strings)

;;;;;;;;;;;;;;;;;;;
;;chunk functions;;
;;;;;;;;;;;;;;;;;;;

;empty (no-op) chunk
; only real uses of this chunk are for testing and filing in stubs/empty parameters
(define/contract empty-chunk
  chunk/c
  (λ (context)
    (nekot 'empty null context)))
(provide empty-chunk)

;literal chunk
; simple string
(define/contract (literal-chunk . strings)
  (->* () #:rest (listof (or/c symbol? string?)) chunk/c)
  (λ  (context)
    (nekot 'literal (apply combine-strings strings) context)))
(provide literal-chunk)

;sequence of spaces chunk
; adds some number of spaces
(define/contract (spaces-chunk . lengths)
  (->* () #:rest (listof (or/c string? symbol? natural-number/c)) chunk/c)
  (λ (context)
    (nekot 'spaces (apply combine-lengths lengths) context)))
(provide spaces-chunk)

;TODO: Move this to chunk-standard.rkt when it's ready
; - added for convenience (at last edit, chunk-standard.rkt was not set up
;space chunk
; adds a space
(define/contract space-chunk
  chunk/c
  (spaces-chunk 1))
(provide space-chunk)

;new line chunk
; adds a new line
(define/contract new-line-chunk
  chunk/c
  (λ (context)
    (nekot 'new-line null context)))
(provide new-line-chunk)

;blank lines chunk
; adds n blank lines
(define/contract (blank-lines-chunk . lengths)
  (->* () #:rest (listof natural-number/c) chunk/c)
  (λ (context)
    (nekot 'blank-lines (apply combine-lengths lengths) context)))
(provide blank-lines-chunk)

;TODO: Move this to chunk-standard.rkt when it's ready
; - added for convenience (at last edit, chunk-standard.rkt was not set up
;blank line chunk
; adds a blank line
(define/contract blank-line-chunk
  chunk/c
  (blank-lines-chunk 1))
(provide blank-line-chunk)

;preprocessor directive chunk
; correctly adds # to line
(define/contract pp-directive-chunk
  chunk/c
  (λ (context)
    (nekot 'pp-directive null context)))
(provide pp-directive-chunk)

;concatenation chunk
; sets up a general concatenation chunks
; - attempts to put as many on the same line as possible
; - no spaces added
(define/contract (concat-chunk . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  (λ (context)
    (nekot
           'concat
           (map (λ (chunk) (chunk context)) chunks)
           context)))
(provide concat-chunk)

;TODO: Move this to chunk-standard.rkt when it's ready
; - added for convenience (at last edit, chunk-standard.rkt was not set up
;top-level list of chunks
; each chunk is expanded on its own line with a blank line between them
; - each chunk put on it's own line
; - blank lines added between chunks
(define/contract (top-list-chunk . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  (apply concat-chunk
         (add-between chunks blank-line-chunk)))
(provide top-list-chunk)

;TODO: Move this to chunk-standard.rkt when it's ready
; - added for convenience (at last edit, chunk-standard.rkt was not set up
;enter new environment
; starts with new line (using initial-string of new environment)
; - given chunk is evaluated within context with new environment
(define/contract (enter-env-chunk chunk transform)
  (-> chunk/c (-> context/c context/c) chunk/c)
  (λ (context)
    (chunk (transform context))))
(provide enter-env-chunk)

;TODO: Move this to chunk-standard.rkt when it's ready
; - added for convenience (at last edit, chunk-standard.rkt was not set up
;indent chunk
; increases current indent
(define/contract (indent-chunk chunk . lengths)
  (->* (chunk/c) #:rest (listof (or/c string? symbol? natural-number/c)) chunk/c)
  (λ (context)
    (chunk (reindent (apply combine-lengths lengths) context))))
(provide indent-chunk)
