#lang racket

(require "fulmar-core.rkt")

;fulmar core chunks - these directly build nekots or use fulmar-core functionality

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fulmar-core definitions
(provide chunk/c)
(provide chunk-list/c)
(provide nullable-chunk-list/c)
(provide flatten*)

;TODO: increase tests to handle new contracts for combine-lengths and combine-strings (and a good number of chunks: ones that contain "*-list/c")
;combine lengths of given values
(define/contract (combine-lengths . values)
  (->* () #:rest length-list/c natural-number/c)
  (foldl (λ (value total)
           (+ total
              (cond [(integer? value) value]
                    [(string? value) (string-length value)]
                    [(symbol? value) (string-length (symbol->string value))]
                    [(pair? value) (apply combine-lengths value)])))
         0
         values))
(provide combine-lengths)

;combine strings
(define/contract (combine-strings . values)
  (->* () #:rest string-list/c string?)
  (foldl (λ (str total)
           (string-append total
                          (cond [(string? str) str]
                                [(symbol? str) (symbol->string str)]
                                [(pair? str) (apply combine-strings str)])))
         ""
         values))
(provide combine-strings)

;helper for speculative-chunk
; (located here for testing)
(define/contract (length-equals-one lst)
  (-> written-lines/c boolean?)
  (and (pair? lst)
       (= 1 (length lst))))
(provide length-equals-one)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot-building chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;literal chunk
; simple string
(define/contract (literal-chunk . strings)
  (->* () #:rest string-list/c chunk/c)
  (λ (context)
    (nekot 'literal (combine-strings strings) context)))
(provide literal-chunk)

;sequence of spaces chunk
; adds some number of spaces
(define/contract (spaces-chunk . lengths)
  (->* () #:rest length-list/c chunk/c)
  (λ (context)
    (nekot 'spaces (combine-lengths lengths) context)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;meta-nekot-building chunks;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;empty (no-op) chunk
; only real uses of this chunk are for testing and filing in stubs/empty parameters
(define/contract empty-chunk
  chunk/c
  (λ (context)
    (nekot 'empty null context)))
(provide empty-chunk)

;concatenation chunk
; sets up a general concatenation chunks
; - attempts to put as many on the same line as possible
; - no spaces added
(define/contract (concat-chunk . chunks)
  (->* () #:rest chunk-list/c chunk/c)
  (λ (context)
    (nekot 'concat
           (map (λ (chunk) (chunk context))
                (flatten chunks))
           context)))
(provide concat-chunk)

;immediate chunk
; bypasses usual writing rules and writes chunk immediately after preceeding chunk
(define/contract (immediate-chunk chunk)
  (-> chunk/c chunk/c)
  (λ (context)
    (nekot 'immediate
           (chunk context)
           context)))
(provide immediate-chunk)

;speculative chunk
; attempts different chunks
; run proc on first chunk
; if proc returns true, use results of first chunk
; otherwise,            use results of second chunk
(define/contract (speculative-chunk attempt success? backup)
  (-> chunk/c (-> written-lines/c boolean?) chunk/c chunk/c)
  (λ (context)
    (nekot 'speculative
           (list (attempt context)
                 success?
                 (backup context))
           context)))
(provide speculative-chunk)

;position indent chunk
; sets indent to current position of line
(define/contract (position-indent-chunk chunk)
  (-> chunk/c chunk/c)
  (λ (context)
    (nekot 'position-indent
           chunk
           context)))
(provide position-indent-chunk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;context-aware chunks;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;indent chunk
; increases current indent
(define/contract (indent-chunk length chunk)
  (-> length-list/c chunk/c chunk/c)
  (λ (context)
    (chunk (reindent (if (pair? length)
                         (combine-lengths length)
                         length)
                     context))))
(provide indent-chunk)

;comment block chunk
; puts chunks in a comment block environment
(define/contract (comment-env-chunk chunk [char #\ ])
  (->* (chunk/c) (char?) chunk/c)
  (λ (context)
    (let ([env (context-env context)])
      (nekot 'concat
             (if (or (comment-env? env)
                     (comment-macro-env? env)
                     (macro-comment-env? env))
                 (list ((literal-chunk "//" (string char)) context)
                       (chunk (enter-comment-env context)))
                 (list ((literal-chunk "/*" (string char)) context)
                       (chunk (enter-comment-env context))
                       ((literal-chunk "*/") context)))
             context))))
(provide comment-env-chunk)

;comment-line chunk
; single-line comment chunk
(define/contract (comment-line-chunk . chunk-lists)
  (->* () #:rest chunk-list/c chunk/c)
  (let ([chunks (flatten chunk-lists)])
    (λ (context)
      (let ([env (context-env context)])
        ((cond [;in macro environment
                (macro-env? env)
                (immediate-chunk (concat-chunk (literal-chunk "/*")
                                               chunks
                                               (literal-chunk "*/")))]
               [;in empty, comment, comment-macro or macro-comment environment
                (or (empty-env? env)
                    (comment-env? env)
                    (comment-macro-env? env)
                    (macro-comment-env? env))
                (immediate-chunk (concat-chunk (literal-chunk "//")
                                               chunks))])
         context)))))
(provide comment-line-chunk)

;macro environment chunk
; puts chunks in a macro environment
(define/contract (macro-env-chunk chunk)
  (-> chunk/c chunk/c)
  (λ (context)
    (chunk (enter-macro-env context))))
(provide macro-env-chunk)
