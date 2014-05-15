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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot-building chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;literal chunk
; simple string
(define (literal . strings)
  (combine-strings strings))

;a single space chunk
(define space (s-chunk 'space null))

;new line chunk
; adds a new line
(define new-line
  (s-chunk 'new-line null))

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

;indent chunk
; increases current indent
(define (indent length chunk)
  (s-chunk 'indent (list chunk length)))

;comment env chunk
; puts chunks in a comment env environment
(define (comment-env-chunk chunk [init-char #\space])
  (s-chunk 'comment (list chunk init-char)))
