#lang racket

;(require "fulmar-core.rkt") ; TODO: determine if this is this needed?
(require "chunk-core.rkt")

;fulmar standard chunks (non-core chunks that are extremely helpful)

;preprocessor strings

(define pp-define-string "define")
(define pp-ifndef-string "ifndef")
(define pp-endif-string "endif")

;chunk definitions

;open parenthesis chunk
; adds "("
(define/contract open-paren-chunk
  chunk/c
  (literal-chunk "("))
(provide open-paren-chunk)

;close parenthesis chunk
; adds ")"
(define/contract close-paren-chunk
  chunk/c
  (literal-chunk ")"))
(provide close-paren-chunk)

;comma chunk
; adds ","
(define/contract comma-chunk
  chunk/c
  (literal-chunk ","))
(provide comma-chunk)

;preprocessor define chunk
; #define chunk
(define/contract (pp-define-chunk name)
  (-> string? chunk/c)
  (line-chunk (pp-directive-chunk pp-define-string)
              (literal-chunk name)))
(provide pp-define-chunk)

;preprocessor if-not-defined chunk
(define/contract (pp-ifndef-chunk condition)
  (-> string? chunk/c)
  (line-chunk (pp-directive-chunk pp-ifndef-string)
              (literal-chunk condition)))
(provide pp-ifndef-chunk)

;preprocessor endif chunk
(define/contract (pp-endif-chunk condition)
  (-> string? chunk/c)
  (line-chunk (pp-directive-chunk pp-endif-string)
              (comment-string-chunk condition)))
(provide pp-endif-chunk)

;preprocessor h file wrapper chunk
(define/contract (pp-h-file-wrapper . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  ;  (-> (listof chunk/c) chunk/c)
  (λ (context)
    (let* ([wrapper-string (string-append "";(context-project-id context)
                                          "_"
                                          "";(context-file-id context)
                                          "_")]
           [chunk (apply top-list-chunk (append (list (list-chunk (pp-ifndef-chunk wrapper-string)
                                                                  new-line-chunk
                                                                  (pp-define-chunk wrapper-string)))
                                                chunks
                                                (list (pp-endif-chunk wrapper-string))))])
      (chunk context))))
(provide pp-h-file-wrapper)

;comment block chunk
; chunks in a comment block environment
(define/contract (comment-block-chunk . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  ;(-> (listof chunk/c) chunk/c)
  (new-env-chunk enter-comment-env
                 chunks))
(provide comment-block-chunk)

;simple comment chunk
; commented chunks on the same line
(define/contract (comment-line-chunk . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  ;(-> (listof chunk/c) chunk/c)
  (apply list-chunk
         (append (list (literal-chunk "/* "))
                 chunks
                 (list (literal-chunk " */")))))
(provide comment-line-chunk)

;comment string chunk
; commented string
(define/contract (comment-string-chunk string)
  (-> string? chunk/c)
  (comment-line-chunk (literal-chunk string)))
(provide comment-string-chunk)

;macro argument contract
(define macro-arg/c (or/c string?
                          (cons/c string? string?)))
(provide macro-arg/c)

;marco argument chunk
(define/contract (macro-arg-chunk param)
  (-> macro-arg/c chunk/c)
  (if (string? param)
      (literal-chunk param)
      (list-chunk (comment-string-chunk (car param))
                  space-chunk
                  (literal-chunk (cdr param)))))
;(provide macro-arg-chunk)

;macro argument list chunk
; chunks that build argument list for a macro defintion
(define/contract (macro-arg-list-chunk params)
  (-> (listof macro-arg/c) chunk/c)
  (if (empty? params)
      close-paren-chunk
      (apply list-chunk
             (append (list (macro-arg-chunk (car params)))
                     (append-map (λ (param) (list comma-chunk
                                                  new-line-chunk
                                                  (macro-arg-chunk param)))
                                 (cdr params))
                     (list close-paren-chunk)))))
;(provide macro-arg-list-chunk)

;macro header chunk
; sets up define for a macro-definition
(define/contract (macro-header-chunk name params)
  (-> string? (listof macro-arg/c) chunk/c)
  (let ([indent (+ (string-length pp-define-string)
                   1 ; for space after define
                   (string-length name)
                   1 ; for open parenthesis
                   )])
    (list-chunk pp-define-chunk
                (literal-chunk name)
                open-paren-chunk
                (λ (context)
                  (let ([new-context (reindent indent context)])
                    ((macro-arg-list-chunk params) new-context))))))
;(provide macro-header-chunk)

;macro defintion chunk
; a macro definition
(define/contract (macro-definition-chunk name params . chunks)
  (->* (string? (listof macro-arg/c)) #:rest (listof chunk/c) chunk/c)
  ;(-> string? (listof macro-arg/c) (listof chunk/c) chunk/c)
  (new-env-chunk enter-macro-env
                 (cons (macro-header-chunk name params)
                       chunks)))
(provide macro-definition-chunk)


