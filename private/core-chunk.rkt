#lang typed/racket

(require "fulmar-core.rkt")

;fulmar core chunks - these directly build nekots or use fulmar-core functionality

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fulmar-core definitions
#;(provide flatten*)

(provide (all-defined-out))

;combine lengths of given values
(: combine-lengths (Number * -> Number))
(define (combine-lengths . values)
  (apply + values))

;combine strings
(: combine-strings ((U Symbol String) * -> String))
(define (combine-strings . values)
  (apply string-append 
         (map (λ: ([s : (U Symbol String)]) 
                (if (symbol? s) (symbol->string s)
                    s)) 
              values)))

;helper for speculative
; (located here for testing)
(: length-equals-one (All (a) ((Listof a) -> Boolean)))
(define (length-equals-one lst)
  (and (pair? lst)
       (= 1 (length lst))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot-building chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;literal chunk
; simple string
(: literal ((U Symbol String) * -> String))
(define (literal . strings)
  (apply combine-strings strings))

;sequence of spaces chunk
; adds some number of spaces
(: spaces (Number * -> Number))
(define (spaces . lengths)
  (apply combine-lengths lengths))

;new line chunk
; adds a new line
(define new-line
  new-line-chunk)

;preprocessor directive chunk
; correctly adds # to line
(define pp-directive "#")

;empty (no-op) chunk
; only real uses of this chunk are for testing and filing in stubs/empty parameters
(define empty
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;meta-nekot-building chunks;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;concatenation chunk
; sets up a general concatenation chunks
; - attempts to put as many on the same line as possible
; - no spaces added
(: concat (Nekot * -> Concat))
(define (concat . chunks)
  (Concat chunks))

;immediate chunk
; bypasses usual writing rules and writes chunk immediately after preceeding chunk
(define: (immediate [chunk : Nekot]) : Immediate
  (Immediate chunk))

;speculative chunk
; attempts different chunks
; run proc on first chunk
; if proc returns true, use results of first chunk
; otherwise,            use results of second chunk
(: speculative (Nekot ((Listof String) -> Boolean) Nekot -> Speculative))
(define (speculative attempt success? backup)
  (Speculative attempt success? backup))

;position indent chunk
; sets indent to current position of line
(define: (position-indent [chunk : Nekot]) : Position-indent
  (Position-indent chunk))

;indent chunk
; increases current indent
(define: (indent [length : Integer] [chunk : Nekot]) : Indent
  (Indent chunk length))

;comment env chunk
; puts chunks in a comment env environment
(define: (comment-env-chunk [chunk : Nekot]) : Concat
  (concat "/* " chunk " */"))
