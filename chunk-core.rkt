#lang racket

(require "fulmar-core.rkt")

;fulmar core chunks - these directly build nekots

;empty (no-op) chunk
; only real uses of this chunk are for testing and filing in stubs/empty parameters
(define/contract empty-chunk
  chunk/c
  (λ (context)
    (nekot 'empty null context)))
(provide empty-chunk)

;literal chunk
; simple string
(define/contract (literal-chunk string)
  (-> string? chunk/c)
  (λ  (context)
    (nekot 'literal string context)))
(provide literal-chunk)

;sequence of spaces chunk
; adds some number of spaces
(define/contract (spaces-chunk length)
  (-> natural-number/c chunk/c)
  (λ (context)
    (nekot 'spaces length context)))
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
(define/contract (blank-lines-chunk length)
  (-> natural-number/c chunk/c)
  (λ (context)
    (nekot 'blank-lines length context)))
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
; #something chunk
(define/contract (pp-directive-chunk string)
  (-> string? chunk/c)
  (λ (context)
    (nekot 'pp-directive string context)))
(provide pp-directive-chunk)

;concatenation chunk
; sets up a general concatenation chunks
; - attempts to put as many on the same line as possible
; - no spaces added
(define/contract (concat-chunk . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  (λ (context)
    (nekot 'concat
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

;TODO: If the current implementation works (or does with modest tweeking),
;    then it should be moved to chunk-standard.rkt since it relies only
;         on the capabilities of concat-chunk
; - this is not a standard use of concat... the context of the concat itself
;   is different from the context of its nekots...
;enter new environment
; starts with new line (using initial-string of new environment)
; - given chunk is evaluated within context with new environment
(define/contract (enter-env-chunk transform chunk)
  (-> (-> context/c context/c) chunk/c chunk/c)
  (λ (context)
    (chunk (transform context))))
(provide enter-env-chunk)
