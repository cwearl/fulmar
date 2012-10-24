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
(provide (contract-out (combine-lengths (->* () #:rest length-list/c natural-number/c))))

;combine strings
(define (combine-strings . values)
  (foldl (λ (str total)
           (string-append total
                          (cond [(string? str) str]
                                [(symbol? str) (symbol->string str)]
                                [(pair? str) (apply combine-strings str)])))
         ""
         values))
(provide (contract-out (combine-strings (->* () #:rest string-list/c string?))))

;helper for speculative-chunk
; (located here for testing)
(define (length-equals-one lst)
  (and (pair? lst)
       (= 1 (length lst))))
(provide (contract-out (length-equals-one (-> written-lines/c boolean?))))

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
                                                                   (concat-chunk (if (or (comment-env? env)
                                                                                         (comment-macro-env? env)
                                                                                         (macro-comment-env? env))
                                                                                     (list "//"
                                                                                           (string (second body))
                                                                                           (modify-context-chunk (first body)
                                                                                                                 enter-comment-env))
                                                                                     (list "/*"
                                                                                           (string (second body))
                                                                                           (modify-context-chunk (first body)
                                                                                                                 enter-comment-env)
                                                                                           " */"))))
                                                                 context)]
                              [_ (error "Unknown type of s-chunk; given: " chunk)]))]
        [else (error "Unknown chunk subtype; given: " chunk)]))
(provide (contract-out (chunk-transform (-> chunk/c context/c nekot/c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot-building chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;literal chunk
; simple string
(define (literal-chunk . strings)
  (combine-strings strings))
(define literal-chunk/c (->* () #:rest string-list/c chunk/c))
(provide literal-chunk/c
         (contract-out (literal-chunk literal-chunk/c)))

;sequence of spaces chunk
; adds some number of spaces
(define (spaces-chunk . lengths)
  (combine-lengths lengths))
(define spaces-chunk/c (->* () #:rest length-list/c chunk/c))
(provide spaces-chunk/c
         (contract-out (spaces-chunk spaces-chunk/c)))

;new line chunk
; adds a new line
(define new-line-chunk
  (s-chunk 'new-line
           null))
(provide (contract-out (new-line-chunk chunk/c)))

;preprocessor directive chunk
; correctly adds # to line
(define pp-directive-chunk
  (s-chunk 'pp-directive
           null))
(provide (contract-out (pp-directive-chunk chunk/c)))

;empty (no-op) chunk
; only real uses of this chunk are for testing and filing in stubs/empty parameters
(define empty-chunk
  (s-chunk 'empty
           null))
(provide (contract-out (empty-chunk chunk/c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;meta-nekot-building chunks;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;concatenation chunk
; sets up a general concatenation chunks
; - attempts to put as many on the same line as possible
; - no spaces added
(define (concat-chunk . chunks)
  (s-chunk 'concat
           (flatten chunks)))
(define concat-chunk/c (->* () #:rest chunk-list/c chunk/c))
(provide concat-chunk/c
         (contract-out (concat-chunk concat-chunk/c)))

;immediate chunk
; bypasses usual writing rules and writes chunk immediately after preceeding chunk
(define (immediate-chunk chunk)
  (s-chunk 'immediate
           chunk))
(define immediate-chunk/c (-> chunk/c chunk/c))
(provide immediate-chunk/c
         (contract-out (immediate-chunk immediate-chunk/c)))

;speculative chunk
; attempts different chunks
; run proc on first chunk
; if proc returns true, use results of first chunk
; otherwise,            use results of second chunk
(define (speculative-chunk attempt success? backup)
  (s-chunk 'speculative
           (list attempt
                 success?
                 backup)))
(define speculative-chunk/c (-> chunk/c (-> written-lines/c boolean?) chunk/c chunk/c))
(provide speculative-chunk/c
         (contract-out (speculative-chunk speculative-chunk/c)))

;position indent chunk
; sets indent to current position of line
(define (position-indent-chunk chunk)
  (s-chunk 'position-indent
           chunk))
(define position-indent-chunk/c (-> chunk/c chunk/c))
(provide position-indent-chunk/c
         (contract-out (position-indent-chunk position-indent-chunk/c)))

;modify context chunk
; changes context for given chunk
(define (modify-context-chunk chunk modify)
  (s-chunk 'modify-context
           (list chunk
                 modify)))
(define modify-context-chunk/c (-> chunk/c (-> context/c context/c) chunk/c))
(provide modify-context-chunk/c
         (contract-out (modify-context-chunk modify-context-chunk/c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;context-aware chunks;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;indent chunk
; increases current indent
(define (indent-chunk length chunk)
  (modify-context-chunk chunk
                        (λ (context)
                          (reindent (combine-lengths length)
                                    context))))
(define indent-chunk/c (-> length-list/c chunk/c chunk/c))
(provide indent-chunk/c
         (contract-out (indent-chunk indent-chunk/c)))

;comment env chunk
; puts chunks in a comment env environment
(define (comment-env-chunk chunk [char #\ ])
  (s-chunk 'comment-env
           (list chunk
                 char)))
(define comment-env-chunk/c (->* (chunk/c) (char?) chunk/c))
(provide comment-env-chunk/c
         (contract-out (comment-env-chunk comment-env-chunk/c)))

;macro environment chunk
; puts chunks in a macro environment
(define (macro-env-chunk chunk)
  (modify-context-chunk chunk
                        enter-macro-env))
(provide (contract-out (macro-env-chunk (-> chunk/c chunk/c))))
