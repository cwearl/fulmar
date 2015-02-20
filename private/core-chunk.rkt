#lang typed/racket

(require "fulmar-core.rkt"
         "doc/document.rkt")

;fulmar core chunks - these directly build nekots or use fulmar-core functionality

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fulmar-core definitions
(provide flatten*
         Chunk
         NestofChunks)

(provide (all-defined-out))

;combine strings
(: combine-strings ((U Symbol String) * -> String))
(define (combine-strings . values)
  (apply string-append 
         (map (λ: ([s : (U Symbol String)]) 
                (cond
                  [(symbol? s) (symbol->string s)]
                  [else s]))
              values)))

;helper for speculative
; (located here for testing)
(: length-equals-one (All (a) ((Listof a) -> Boolean)))
(define (length-equals-one lst)
  (and (pair? lst)
       (= 1 (length lst))))


(document not-ends-in->>
"Helper function for speculative that will return false if the given chunks
 end in >>.")
(: not-ends-in->> ((Listof String) -> Boolean))
(define (not-ends-in->> lst)
  (match lst
    [`(,(regexp #rx">>$") . ,_) #f]
    [_ #t]))

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
(: space Chunk)
(define space (Space))

;new line chunk
; adds a new line
(define new-line
  (Newline))

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
(: concat (NestofChunks * -> Concat))
(define (concat . chunks)
  (Concat (flatten* chunks)))

;immediate chunk
; bypasses usual writing rules and writes chunk immediately after preceeding chunk
(define: (immediate [chunk : Chunk]) : Immediate
  (Immediate chunk))

;speculative chunk
; attempts different chunks
; run proc on first chunk
; if proc returns true, use results of first chunk
; otherwise,            use results of second chunk
(: speculative (Chunk ((Listof String) -> Boolean) Chunk -> Speculative))
(define (speculative attempt success? backup)
  (Speculative attempt success? backup))

;position indent chunk
; sets indent to current position of line
(: position-indent (Chunk -> Position-indent))
(define (position-indent chunk)
  (Position-indent chunk))

;indent chunk
; increases current indent
(: indent (Integer Chunk -> Indent))
(define (indent length chunk)
  (Indent chunk length))

;comment env chunk
; puts chunks in a comment env environment
(: comment-env-chunk (case-> [Chunk -> Comment]
                             [Chunk Char -> Comment]))
(define (comment-env-chunk chunk [init-char #\space])
  (Comment chunk init-char))
