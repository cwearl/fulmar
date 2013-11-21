#lang racket

(require "fulmar-core.rkt")

;fulmar core chunks - these directly build nekots or use fulmar-core functionality

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fulmar-core definitions
(provide flatten*)

;TODO: increase tests to handle new contracts for combine-lengths and combine-strings (and a good number of chunks: ones that contain "*-list/c")
;combine lengths of given values
(define (combine-lengths . values)
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
(define (combine-strings . values)
  (foldl (λ (str total)
            (string-append total
                           (cond [(string? str) str]
                                 [(symbol? str) (symbol->string str)]
                                 [(pair? str) (apply combine-strings str)])))
         ""
         values))
(provide combine-strings)

;helper for speculative
; (located here for testing)
(define (length-equals-one lst)
  (and (pair? lst)
       (= 1 (length lst))))
(provide length-equals-one)

;chunk transformer
; transforms chunks into nekots
(define (chunk-transform chunk context)
  (cond [(string? chunk) (nekot 'literal
                                chunk
                                context)]
        [(symbol? chunk) (nekot 'literal
                                (symbol->string chunk)
                                context)]
        [(exact-nonnegative-integer? chunk) (nekot 'spaces
                                                   chunk
                                                   context)]
        [(s-chunk? chunk) (let ([name (s-chunk-name chunk)]
                                [body (s-chunk-body chunk)])
                            (match name
                                   ['new-line       (nekot 'new-line
                                                           null
                                                           context)]
                                   ['pp-directive   (nekot 'pp-directive
                                                           null
                                                           context)]
                                   ['empty          (nekot 'empty
                                                           null
                                                           context)]
                                   ['concat         (nekot 'concat
                                                           (map (λ (chunk)
                                                                   (chunk-transform chunk
                                                                                    context))
                                                                body)
                                                           context)]
                                   ['immediate      (nekot 'immediate
                                                           (chunk-transform body context)
                                                           context)]
                                   ['speculative    (nekot 'speculative
                                                           (list (chunk-transform (first body)
                                                                                  context)
                                                                 (second body)
                                                                 (chunk-transform (third body)
                                                                                  context))
                                                           context)]
                                   ['position-indent (nekot 'position-indent
                                                            body
                                                            context)]
                                   ['modify-context  (chunk-transform (first body)
                                                                      ((second body) context))]
                                   ['comment-env   (chunk-transform (let ([env (context-env context)])
                                                                      (concat (if (or (comment-env? env)
                                                                                      (comment-macro-env? env)
                                                                                      (macro-comment-env? env))
                                                                                  (list "//"
                                                                                        (string (second body))
                                                                                        (modify-context (first body)
                                                                                                        enter-comment-env))
                                                                                  (list "/*"
                                                                                        (string (second body))
                                                                                        (modify-context (first body)
                                                                                                        enter-comment-env)
                                                                                        " */"))))
                                                                    context)]
                                   [_ (error "Unknown type of s-chunk; given: " chunk)]))]
        [else (error "Unknown chunk subtype; given: " chunk)]))
(provide chunk-transform)

;transform-chunks
(define (transform-chunks chunks context)
  (chunk-transform (concat (add-between (flatten* chunks)
                                        (concat new-line new-line)))
                   context))
(provide transform-chunks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot-building chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;literal chunk
; simple string
(define (literal . strings)
  (combine-strings strings))
(provide literal)

;sequence of spaces chunk
; adds some number of spaces
(define (spaces . lengths)
  (combine-lengths lengths))
(provide spaces)

;new line chunk
; adds a new line
(define new-line
  (s-chunk 'new-line null))
(provide new-line)

;preprocessor directive chunk
; correctly adds # to line
(define pp-directive
  (s-chunk 'pp-directive null))
(provide pp-directive)

;empty (no-op) chunk
; only real uses of this chunk are for testing and filing in stubs/empty parameters
(define empty
  (s-chunk 'empty null))
(provide empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;meta-nekot-building chunks;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;concatenation chunk
; sets up a general concatenation chunks
; - attempts to put as many on the same line as possible
; - no spaces added
(define (concat . chunks)
  (s-chunk 'concat (flatten chunks)))
(provide concat)

;immediate chunk
; bypasses usual writing rules and writes chunk immediately after preceeding chunk
(define (immediate chunk)
  (s-chunk 'immediate chunk))
(provide immediate)

;speculative chunk
; attempts different chunks
; run proc on first chunk
; if proc returns true, use results of first chunk
; otherwise,            use results of second chunk
(define (speculative attempt success? backup)
  (s-chunk 'speculative (list attempt success? backup)))
(provide speculative)

;position indent chunk
; sets indent to current position of line
(define (position-indent chunk)
  (s-chunk 'position-indent chunk))
(provide position-indent)

;modify context chunk
; changes context for given chunk
(define (modify-context chunk modify)
  (s-chunk 'modify-context (list chunk modify)))
(provide modify-context)

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
(provide indent)

;comment env chunk
; puts chunks in a comment env environment
(define (comment-env-chunk chunk [char #\ ])
  (s-chunk 'comment-env (list chunk char)))
(provide comment-env-chunk)

;macro environment chunk
; puts chunks in a macro environment
(define (macro-env-chunk chunk)
  (modify-context chunk enter-macro-env))
(provide macro-env-chunk)
