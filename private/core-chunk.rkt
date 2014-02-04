#lang racket

(require "fulmar-core.rkt")

;fulmar core chunks - these directly build nekots or use fulmar-core functionality

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fulmar-core definitions
(provide flatten*)

(provide (all-defined-out))

;combine lengths of given values
(define (combine-lengths . values)
  (apply + (flatten values)))

;combine strings
(define (combine-strings . values)
  (apply string-append 
         (map (λ (s) 
                (match s 
                  [(? symbol?) (symbol->string s)] 
                   [_ s])) 
              (flatten values))))

;helper for speculative
; (located here for testing)
(define (length-equals-one lst)
  (and (pair? lst)
       (= 1 (length lst))))

; helper for chunk-transform to handle 'comment-env
(define (build-comment-env-nekot chunk char context)
  (define middle 
    (list
     (string char)
     (modify-context chunk enter-comment-env)))
  
  (concat 
   (match (context-env context) 
     [(or (environment 'comment _) (environment 'comment-macro _) (environment 'macro-comment _))
      (list "//" middle)]
     [_
      (list "/*" middle " */")])))

;chunk transformer
; transforms chunks into nekots
(define (chunk-transform chunk context)
  (define nekot-ctx ((curryr nekot) context))
  (define chunk-transform-ctx ((curryr chunk-transform) context))
  
  (match chunk
    [(? string?) 
     (nekot-ctx 'literal chunk)]
    [(? symbol?) 
     (nekot-ctx 'literal (symbol->string chunk))]
    [(? exact-nonnegative-integer?) 
     (nekot-ctx 'spaces chunk)]
    [(s-chunk (and sym (or 'new-line 'pp-directive 'empty)) _) 
     (nekot-ctx sym null)]    
    [(s-chunk 'concat body) 
     (nekot-ctx 'concat (map chunk-transform-ctx body))]
    [(s-chunk 'immediate body) 
     (nekot-ctx 'immediate (chunk-transform-ctx body))]
    [(s-chunk 'speculative (list attempt check backup)) 
     (nekot-ctx 'speculative (list (chunk-transform-ctx attempt) check (chunk-transform-ctx backup)))]
    [(s-chunk 'position-indent body) 
     (nekot-ctx 'position-indent body)]
    [(s-chunk 'modify-context (list chunk modify)) 
     (chunk-transform chunk (modify context))]
    [(s-chunk 'comment-env (list chunk char)) 
     (chunk-transform-ctx (build-comment-env-nekot chunk char context))]
    [_ (error "Unknown chunk subtype; given: " chunk)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot-building chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;literal chunk
; simple string
(define (literal . strings)
  (combine-strings strings))

;sequence of spaces chunk
; adds some number of spaces
(define (spaces . lengths)
  (combine-lengths lengths))

;new line chunk
; adds a new line
(define new-line
  (s-chunk 'new-line null))

;preprocessor directive chunk
; correctly adds # to line
(define pp-directive
  (s-chunk 'pp-directive null))

;empty (no-op) chunk
; only real uses of this chunk are for testing and filing in stubs/empty parameters
(define empty
  (s-chunk 'empty null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;meta-nekot-building chunks;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;concatenation chunk
; sets up a general concatenation chunks
; - attempts to put as many on the same line as possible
; - no spaces added
(define (concat . chunks)
  (s-chunk 'concat (flatten chunks)))

;immediate chunk
; bypasses usual writing rules and writes chunk immediately after preceeding chunk
(define (immediate chunk)
  (s-chunk 'immediate chunk))

;speculative chunk
; attempts different chunks
; run proc on first chunk
; if proc returns true, use results of first chunk
; otherwise,            use results of second chunk
(define (speculative attempt success? backup)
  (s-chunk 'speculative (list attempt success? backup)))

;position indent chunk
; sets indent to current position of line
(define (position-indent chunk)
  (s-chunk 'position-indent chunk))

;modify context chunk
; changes context for given chunk
(define (modify-context chunk modify)
  (s-chunk 'modify-context (list chunk modify)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;context-aware chunks;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;indent chunk
; increases current indent
(define (indent length chunk)
  (modify-context chunk
                  (λ (context)
                    (reindent (combine-lengths length)
                              context))))

;comment env chunk
; puts chunks in a comment env environment
(define (comment-env-chunk chunk [char #\ ])
  (s-chunk 'comment-env (list chunk char)))
