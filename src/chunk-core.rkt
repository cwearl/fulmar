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

;chunk transformer
; transforms chunks into nekots
(define/contract (chunk-transform chunk context)
  (-> chunk/c context/c nekot/c)
  (cond [(procedure? chunk) (chunk context)]
        [(s-chunk? chunk) (let ([name (s-chunk-name chunk)]
                                [body (s-chunk-body chunk)])
                            (nekot name
                                   (match name
                                     ['new-line       null]
                                     ['pp-directive   null]
                                     ['empty          null]
                                     ['concat         (map (λ (chunk)
                                                           (chunk-transform chunk
                                                                            context))
                                                         body)]
                                     ['immediate      (chunk-transform body)]
                                     ['speculative    (list (chunk-transform (first body)
                                                                           context)
                                                          (second body)
                                                          (chunk-transform (third body)
                                                                           context))]
                                     ['position-indent (chunk-transform body
                                                                        context)]
                                     ['modify-context  (chunk-transform (first body)
                                                                        ((second body) context))]
                                     ['comment         (list (chunk-transform (first body)
                                                                              context)
                                                             (second body))]
                                     [_ (error "Unknown type of s-chunk; given: " chunk)])
                                   context))]
        [(string? chunk) (nekot 'literal
                                chunk
                                context)]
        [(symbol? chunk) (nekot 'literal
                                (symbol->string chunk)
                                context)]
        [(exact-nonnegative-integer? chunk) (nekot 'spaces
                                                   chunk
                                                   context)]
        [else (error "Unknown chunk subtype; given: " chunk)]))
(provide chunk-transform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot-building chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;literal chunk
; simple string
(define/contract (literal-chunk . strings)
  (->* () #:rest string-list/c chunk/c)
  (combine-strings strings))
(provide literal-chunk)

;sequence of spaces chunk
; adds some number of spaces
(define/contract (spaces-chunk . lengths)
  (->* () #:rest length-list/c chunk/c)
  (combine-lengths lengths))
(provide spaces-chunk)

;new line chunk
; adds a new line
(define/contract new-line-chunk
  chunk/c
  (s-chunk 'new-line
           null))
(provide new-line-chunk)

;preprocessor directive chunk
; correctly adds # to line
(define/contract pp-directive-chunk
  chunk/c
  (s-chunk 'pp-directive
           null))
(provide pp-directive-chunk)

;empty (no-op) chunk
; only real uses of this chunk are for testing and filing in stubs/empty parameters
(define/contract empty-chunk
  chunk/c
  (s-chunk 'empty
           null))
(provide empty-chunk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;meta-nekot-building chunks;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;concatenation chunk
; sets up a general concatenation chunks
; - attempts to put as many on the same line as possible
; - no spaces added
(define/contract (concat-chunk . chunks)
  (->* () #:rest chunk-list/c chunk/c)
  (s-chunk 'concat
           (flatten chunks)))
(provide concat-chunk)

;immediate chunk
; bypasses usual writing rules and writes chunk immediately after preceeding chunk
(define/contract (immediate-chunk chunk)
  (-> chunk/c chunk/c)
  (s-chunk 'immediate
           chunk))
(provide immediate-chunk)

;speculative chunk
; attempts different chunks
; run proc on first chunk
; if proc returns true, use results of first chunk
; otherwise,            use results of second chunk
(define/contract (speculative-chunk attempt success? backup)
  (-> chunk/c (-> written-lines/c boolean?) chunk/c chunk/c)
  (s-chunk 'speculative
           (list attempt
                 success?
                 backup)))
(provide speculative-chunk)

;position indent chunk
; sets indent to current position of line
(define/contract (position-indent-chunk chunk)
  (-> chunk/c chunk/c)
  (s-chunk 'position-indent
           chunk))
(provide position-indent-chunk)

;modify context chunk
; changes context for given chunk
(define/contract (modify-context-chunk chunk modify)
  (-> chunk/c (-> context/c context/c) chunk/c)
  (s-chunk 'modify-context
           (list chunk
                 modify)))
(provide modify-context-chunk)

;comment block chunk
; puts chunks in a comment block environment
(define/contract (comment-env-chunk chunk [char #\ ])
  (->* (chunk/c) (char?) chunk/c)
  (s-chunk 'comment
           (list chunk
                 char)))
(provide comment-env-chunk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;context-aware chunks;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;indent chunk
; increases current indent
(define/contract (indent-chunk length chunk)
  (-> length-list/c chunk/c chunk/c)
  (modify-context-chunk chunk
                        (λ (context)
                          (reindent (combine-lengths length)
                                    context))))
(provide indent-chunk)

;macro environment chunk
; puts chunks in a macro environment
(define/contract (macro-env-chunk chunk)
  (-> chunk/c chunk/c)
  (modify-context-chunk chunk
                        enter-macro-env))
(provide macro-env-chunk)
