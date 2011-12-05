#lang racket

(require "fulmar-core.rkt")

;fulmar core chunks - these directly build nekots or use fulmar-core functionality

;;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;fulmar-core definitions
(provide chunk/c)

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot-building chunks;;
;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (->* () #:rest (non-empty-listof (or/c symbol? string?)) chunk/c)
  (λ (context)
    (nekot 'literal (apply combine-strings strings) context)))
(provide literal-chunk)

;sequence of spaces chunk
; adds some number of spaces
(define/contract (spaces-chunk . lengths)
  (->* () #:rest (listof (or/c string? symbol? natural-number/c)) chunk/c)
  (λ (context)
    (nekot 'spaces (apply combine-lengths lengths) context)))
(provide spaces-chunk)

;new line chunk
; adds a new line
(define/contract new-line-chunk
  chunk/c
  (λ (context)
    (nekot 'new-line null context)))
(provide new-line-chunk)

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
    (nekot 'concat
           (map (λ (chunk) (chunk context)) chunks)
           context)))
(provide concat-chunk)

;bottom level list chunk
; forces all chunks to go on the same line
; - no added spaces or new lines
(define/contract (bot-list-chunk . chunks)
  (->* () #:rest (non-empty-listof chunk/c) chunk/c)
  (λ (context)
    (nekot 'bot-list
           (map (λ (chunk) (chunk context)) chunks)
           context)))
(provide bot-list-chunk)

;low list chunk
; attempts to put chunks on a single line with a space between each chunk
; if that fails, puts chunks on their own lines
(define/contract (low-list-chunk . chunks)
  (->* () #:rest (non-empty-listof chunk/c) chunk/c)
  (λ (context)
    (nekot 'low-list chunks context)))
(provide low-list-chunk)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;context-aware chunks;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;mid-level list of chunks
; each chunk is expanded on its own line
; - each chunk put on it's own line
(define/contract (mid-list-chunk . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  (apply concat-chunk
         (add-between chunks new-line-chunk)))
(provide mid-list-chunk)

;top-level list of chunks
; each chunk is expanded on its own line with a blank line between them
; - each chunk put on it's own line
; - blank lines added between chunks
(define/contract (top-list-chunk . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  (apply concat-chunk
         (add-between chunks (concat-chunk new-line-chunk
                                           new-line-chunk))))
(provide top-list-chunk)

;comment block chunk
; puts chunks in a comment block environment
(define/contract (comment-env-chunk chunk)
  (-> chunk/c chunk/c)
  (λ (context)
    (chunk (enter-comment-env context))))
(provide comment-env-chunk)

;macro environment chunk
; puts chunks in a macro environment
(define/contract (macro-env-chunk chunk)
  (-> chunk/c chunk/c)
  (λ (context)
    (chunk (enter-macro-env context))))
(provide macro-env-chunk)

;indent chunk
; increases current indent
(define/contract (indent-chunk chunk . lengths)
  (->* (chunk/c) #:rest (listof (or/c string? symbol? natural-number/c)) chunk/c)
  (λ (context)
    (chunk (reindent (apply combine-lengths lengths) context))))
(provide indent-chunk)

;comment-line chunk
; single-line comment chunk
(define/contract (comment-line-chunk . strings)
  (->* () #:rest (non-empty-listof (or/c symbol? string?)) chunk/c)
  (λ (context)
    (let ([env (context-env context)]
          [string (apply combine-strings strings)])
      (concat-chunk (if (macro-env? env)
                        (literal-chunk (string-append "/*"
                                                      string
                                                      "*/"))
                        ; either in empty-env or a comment environment
                        (literal-chunk (string-append "// "
                                                      string)))
                    new-line-chunk))))
(provide comment-line-chunk)
