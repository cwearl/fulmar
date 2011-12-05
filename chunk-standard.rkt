#lang racket

(require "chunk-core.rkt")

;fulmar standard chunks

;;;;;;;;;;;;;;;;;;;;;;
;preprocessor strings;
;;;;;;;;;;;;;;;;;;;;;;

(define pp-define-string "define")
(define pp-ifndef-string "ifndef")
(define pp-endif-string "endif")

;;;;;;;;;;;;;;;;;;;;;;
;core chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(provide empty-chunk
         literal-chunk
         spaces-chunk
         new-line-chunk
         pp-directive-chunk
         concat-chunk
         comment-line-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;standard chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;space chunk
; adds a space
(define/contract space-chunk
  chunk/c
  (spaces-chunk 1))
(provide space-chunk)

;blank lines chunk
; adds n blank lines
(define/contract (blank-lines-chunk . lengths)
  (->* () #:rest (listof natural-number/c) chunk/c)
  (apply concat-chunk (make-list (apply combine-lengths (cons 1 lengths)) new-line-chunk)))
(provide blank-lines-chunk)

;blank line chunk
; adds a blank line
(define/contract blank-line-chunk
  chunk/c
  (blank-lines-chunk 1))
(provide blank-line-chunk)

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
  (concat-chunk pp-directive-chunk
                (literal-chunk pp-define-string)
                space-chunk
                (literal-chunk name)))
(provide pp-define-chunk)

;preprocessor if-not-defined chunk
(define/contract (pp-ifndef-chunk condition)
  (-> string? chunk/c)
  (concat-chunk pp-directive-chunk
                (literal-chunk pp-ifndef-string)
                space-chunk
                (literal-chunk condition)))
(provide pp-ifndef-chunk)

;preprocessor endif chunk
(define/contract (pp-endif-chunk condition)
  (-> string? chunk/c)
  (concat-chunk pp-directive-chunk
                (literal-chunk pp-endif-string)
                space-chunk
                (comment-line-chunk condition)))
(provide pp-endif-chunk)

;preprocessor h file wrapper chunk
(define/contract (pp-h-file-wrapper unique-string . chunks)
  (->* (string?) #:rest (listof chunk/c) chunk/c)
  (mid-list-chunk (pp-ifndef-chunk unique-string)
                  (pp-define-chunk unique-string)
                  (top-list-chunk chunks)
                  (pp-endif-chunk unique-string)))
(provide pp-h-file-wrapper)

;macro argument contract
(define macro-arg/c (or/c string?
                          (cons/c string? string?)))
(provide macro-arg/c)

;marco argument chunk
(define/contract (macro-arg-chunk param)
  (-> macro-arg/c chunk/c)
  (if (string? param)
      (literal-chunk param)
      (concat-chunk (comment-line-chunk (car param))
                    (literal-chunk (cdr param)))))
(provide macro-arg-chunk)

;macro argument list chunk
; chunks that build argument list for a macro defintion
(define/contract (macro-arg-list-chunk . params)
  (->* () #:rest (listof macro-arg/c) chunk/c)
  (cond [(empty? params)
         (bot-list-chunk open-paren-chunk
                          close-paren-chunk)]
        [(= 1 (length params))
         (apply bot-list-chunk (list open-paren-chunk
                                     (car params)
                                     close-paren-chunk))]
        [else
         (apply bot-list-chunk (append (list open-paren-chunk)
                                       (apply low-list-chunk (append (map (λ (chunk) (bot-list-chunk chunk
                                                                                                     comma-chunk))
                                                                          (take params (- (length params)
                                                                                          1)))
                                                                     (list (last params))))
                                       (list close-paren-chunk)))]))
(provide macro-arg-list-chunk)

;macro header chunk
; sets up define for a macro-definition
(define/contract (macro-header-chunk name params)
  (-> string? (listof macro-arg/c) chunk/c)
  (let ([indent (+ (string-length pp-define-string)
                   1 ; for space after define
                   (string-length name)
                   1 ; for open parenthesis
                   )])
    (concat-chunk pp-define-chunk
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
  (macro-env-chunk (cons (macro-header-chunk name params)
                         chunks)))
(provide macro-definition-chunk)


