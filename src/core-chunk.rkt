#lang racket

(require "fulmar-core.rkt")

;fulmar core chunks - these directly build nekots or use fulmar-core functionality

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fulmar-core definitions
(provide flatten*)
(provide chunk?)
(provide chunk-list?)
(provide nullable-chunk-list?)

;TODO: remove combine-strings, perhaps?
;combine strings
(define/contract (combine-strings . values)
  (->* () #:rest literal-list? string?)
  (foldl (λ (str total)
           (string-append total
                          (cond [(string? str) str]
                                [(symbol? str) (symbol->string str)]
                                [(pair? str) (apply combine-strings str)])))
         ""
         values))
(provide combine-strings)

;combine lengths of given values
(define/contract (combine-lengths . values)
  (->* () #:rest literal-list? natural-number/c)
  (foldl (λ (value total)
           (+ total
              (cond [(integer? value) value]
                    [(string? value) (string-length value)]
                    [(symbol? value) (string-length (symbol->string value))]
                    [(pair? value) (apply combine-lengths value)])))
         0
         values))
(provide combine-lengths)

;helper for speculative-chunk
; (located here for testing)
(define/contract (length-equals-one lst)
  (-> written-lines? boolean?)
  (and (pair? lst)
       (= 1 (length lst))))
(provide length-equals-one)

;chunk transformer
; transforms chunks into nekots
(define/contract (chunk-transform chunk context)
  (-> chunk? context? logical-block?)
  (cond [(chunk-literal? chunk) (logical-block (logical-line (literal-chunk->logical-line-IR chunk)))]
        ;chunk-struct
        [(chunk-struct? chunk) (let ([name (chunk-struct-name chunk)]
                                     [body (chunk-struct-body chunk)])
                                 (match name
                                   ['new-line]
                                   ['pp-directive]
                                   ;TODO: Remove and replace with 0?
                                   ['empty]
                                   ['concat]
                                   ;TODO: Remove?
                                   ['immediate]
                                   ;TODO: Remove?
                                   ['speculative]
                                   ['position-indent]
                                   ['indent]
                                   ['modify-context]
                                   ;TODO: Determine if this is needed
                                   ['comment-env]
                                   [_ (error "Unknown type of s-chunk; given: " chunk)]))]
        [else (error "Unknown chunk subtype; given: " chunk)]))
(provide chunk-transform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot-building chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;literal chunk
; simple string
(define/contract (literal-chunk . strings)
  (->* () #:rest literal-list? chunk?)
  (combine-strings strings))
(provide literal-chunk)

;sequence of spaces chunk
; adds some number of spaces
(define/contract (spaces-chunk . lengths)
  (->* () #:rest literal-list? chunk?)
  (combine-lengths lengths))
(provide spaces-chunk)

;new line chunk
; adds a new line
(define/contract new-line-chunk
  chunk?
  (chunk-struct 'new-line
                null))
(provide new-line-chunk)

;preprocessor directive chunk
; correctly adds # to line
(define/contract pp-directive-chunk
  chunk?
  (chunk-struct 'pp-directive
                null))
(provide pp-directive-chunk)

;TODO: Remove and replace with 0, perhaps?
;empty (no-op) chunk
; only real uses of this chunk are for testing and filing in stubs/empty parameters
(define/contract empty-chunk
  chunk?
  (chunk-struct 'empty
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
  (->* () #:rest chunk-list? chunk?)
  (chunk-struct 'concat
                (flatten* chunks)))
(provide concat-chunk)

;TODO: Remove as unnecessary with pivots, perhaps?
;immediate chunk
; bypasses usual writing rules and writes chunk immediately after preceeding chunk
(define/contract (immediate-chunk chunk)
  (-> chunk? chunk?)
  (chunk-struct 'immediate
                chunk))
(provide immediate-chunk)

;TODO: Remove as unnecessary with pivots, perhaps?
;speculative chunk
; attempts different chunks
; run proc on first chunk
; if proc returns true, use results of first chunk
; otherwise,            use results of second chunk
(define/contract (speculative-chunk attempt success? backup)
  (-> chunk? (-> written-lines? boolean?) chunk? chunk?)
  (chunk-struct 'speculative
                (list attempt
                      success?
                      backup)))
(provide speculative-chunk)

;position indent chunk
; sets indent to current position of line
(define/contract (position-indent-chunk chunk)
  (-> chunk? chunk?)
  (chunk-struct 'position-indent
                chunk))
(provide position-indent-chunk)

;modify context chunk
; changes context for given chunk
(define/contract (modify-context-chunk chunk modify)
  (-> chunk? (-> context? context?) chunk?)
  (chunk-struct 'modify-context
                (list chunk
                      modify)))
(provide modify-context-chunk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;context-aware chunks;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;indent chunk
; increases current indent
(define/contract (indent-chunk length chunk)
  (-> length-list? chunk? chunk?)
  (modify-context-chunk chunk
                        (λ (context)
                          (reindent (combine-lengths length)
                                    context))))
(provide indent-chunk)

;TODO: Determine if this is needed
;comment env chunk
; puts chunks in a comment env environment
(define/contract (comment-env-chunk chunk [char #\ ])
  (->* (chunk?) (char?) chunk?)
  (chunk-struct 'comment-env
                (list chunk
                      char)))
(provide comment-env-chunk)
;['comment-env   (chunk-transform (let ([env (context-env context)])
;                                   (concat-chunk (if (or (comment-env? env)
;                                                         (comment-macro-env? env)
;                                                         (macro-comment-env? env))
;                                                     (list "//"
;                                                           (string (second body))
;                                                           (modify-context-chunk (first body)
;                                                                                 enter-comment-env))
;                                                     (list "/*"
;                                                           (string (second body))
;                                                           (modify-context-chunk (first body)
;                                                                                 enter-comment-env)
;                                                           " */"))))
;                                 context)]

;macro environment chunk
; puts chunks in a macro environment
(define/contract (macro-env-chunk chunk)
  (-> chunk? chunk?)
  (modify-context-chunk chunk
                        enter-macro-env))
(provide macro-env-chunk)
