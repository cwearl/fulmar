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

(provide literal-chunk)
(provide spaces-chunk)
(provide new-line-chunk)
(provide empty-chunk)
(provide concat-chunk)
(provide immediate-chunk)
(provide speculative-chunk)
(provide position-indent-chunk)
(provide indent-chunk)
(provide comment-env-chunk)
(provide comment-line-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;list chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;immediate list chunk
; forces all chunks to go on the same line
; - new-line-chunks are exception
;   but they must be explicitly in the chunks
;   not implied by the normal writer's rules
;TODO: Determine if imm-list-chunk needed anywhere?
(define/contract (imm-list-chunk . chunks)
  (->* () #:rest (non-empty-listof chunk/c) chunk/c)
  (immediate-chunk (apply concat-chunk chunks)))
(provide imm-list-chunk)

;argument list chunk
; attempts to put chunks on a single line with a space between each chunk
; if that fails, puts chunks on their own lines
(define/contract (arg-list-chunk . chunks)
  (->* () #:rest (non-empty-listof chunk/c) chunk/c)
  (speculative-chunk (apply concat-chunk (add-between chunks (spaces-chunk 1)))
                     (λ (lines) (= 1 (length lines)))
                     (position-indent-chunk (apply concat-chunk (add-between chunks new-line-chunk)))))
(provide arg-list-chunk)

;statement line list of chunks
; each chunk is expanded on its own line
; - each chunk put on it's own line
(define/contract (smt-list-chunk . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  (apply concat-chunk
         (add-between chunks new-line-chunk)))
(provide smt-list-chunk)

;definition list of chunks
; each chunk is expanded on its own line with a blank line between them
; - each chunk put on it's own line
; - blank lines added between chunks
(define/contract (dfn-list-chunk . chunks)
  (->* () #:rest (listof chunk/c) chunk/c)
  (apply concat-chunk
         (add-between chunks (concat-chunk new-line-chunk
                                           new-line-chunk))))
(provide dfn-list-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;character chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;space chunk
; adds a space
(define/contract space-chunk
  chunk/c
  (spaces-chunk 1))
(provide space-chunk)

;immediate space chunk
; adds a space immediately
(define/contract imm-space-chunk
  chunk/c
  (immediate-chunk space-chunk))
(provide imm-space-chunk)

;blank lines chunk
; adds n blank lines
(define/contract (blank-lines-chunk . lengths)
  (->* () #:rest (listof exact-positive-integer?) chunk/c)
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

;immediate open parenthesis chunk
; adds "(" immediately
(define/contract imm-open-paren-chunk
  chunk/c
  (immediate-chunk open-paren-chunk))
(provide imm-open-paren-chunk)

;close parenthesis chunk
; adds ")"
(define/contract close-paren-chunk
  chunk/c
  (literal-chunk ")"))
(provide close-paren-chunk)

;immediate close parenthesis chunk
; adds ")" immediately
(define/contract imm-close-paren-chunk
  chunk/c
  (immediate-chunk close-paren-chunk))
(provide imm-close-paren-chunk)

;open curly bracket chunk
; adds "{"
(define/contract open-crbr-chunk
  chunk/c
  (literal-chunk "{"))
(provide open-crbr-chunk)

;immediate open curly bracket chunk
; adds "{" immediately
(define/contract imm-open-crbr-chunk
  chunk/c
  (immediate-chunk open-crbr-chunk))
(provide imm-open-crbr-chunk)

;close curly bracket chunk
; adds "}"
(define/contract close-crbr-chunk
  chunk/c
  (literal-chunk "}"))
(provide close-crbr-chunk)

;immediate close curly bracket chunk
; adds "}" immediately
(define/contract imm-close-crbr-chunk
  chunk/c
  (immediate-chunk close-crbr-chunk))
(provide imm-close-crbr-chunk)

;open angle-bracket chunk
; adds "<"
(define/contract open-anbr-chunk
  chunk/c
  (literal-chunk "<"))
(provide open-anbr-chunk)

;immediate open angle-bracket chunk
; adds "<" immediately
(define/contract imm-open-anbr-chunk
  chunk/c
  (immediate-chunk open-anbr-chunk))
(provide imm-open-anbr-chunk)

;close angle-bracket chunk
; adds ">"
(define/contract close-anbr-chunk
  chunk/c
  (literal-chunk ">"))
(provide close-anbr-chunk)

;immediate close angle-bracket chunk
; adds ">" immediately
(define/contract imm-close-anbr-chunk
  chunk/c
  (immediate-chunk close-anbr-chunk))
(provide imm-close-anbr-chunk)

;comma chunk
; adds ","
(define/contract comma-chunk
  chunk/c
  (literal-chunk ","))
(provide comma-chunk)

;immediate comma chunk
; adds "," immediately
(define/contract imm-comma-chunk
  chunk/c
  (immediate-chunk comma-chunk))
(provide imm-comma-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;preprocessor chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;preprocessor define chunk
; #define chunk
(define/contract (pp-define-chunk name)
  (-> string-value/c chunk/c)
  (concat-chunk pp-directive-chunk
                (literal-chunk pp-define-string)
                space-chunk
                (literal-chunk name)))
(provide pp-define-chunk)

;preprocessor if-not-defined chunk
(define/contract (pp-ifndef-chunk condition)
  (-> string-value/c chunk/c)
  (concat-chunk pp-directive-chunk
                (literal-chunk pp-ifndef-string)
                space-chunk
                (literal-chunk condition)))
(provide pp-ifndef-chunk)

;preprocessor endif chunk
(define/contract (pp-endif-chunk condition)
  (-> string-value/c chunk/c)
  (concat-chunk pp-directive-chunk
                (literal-chunk pp-endif-string)
                space-chunk
                (comment-line-chunk condition)))
(provide pp-endif-chunk)

;preprocessor h file wrapper chunk
(define/contract (pp-header-file-chunk unique-string . chunks)
  (->* (string-value/c) #:rest (listof chunk/c) chunk/c)
  (concat-chunk (pp-ifndef-chunk unique-string)
                new-line-chunk
                (indent-chunk 3 (pp-define-chunk unique-string))
                blank-line-chunk
                (indent-chunk 3 (apply dfn-list-chunk chunks))
                blank-line-chunk
                (pp-endif-chunk unique-string)))
(provide pp-header-file-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;macro chunks;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;macro argument contract
(define macro-arg/c (or/c string-value/c
                          (list/c string-value/c string-value/c)))
;(provide macro-arg/c)

;marco argument chunk
(define/contract (macro-arg-chunk param)
  (-> macro-arg/c chunk/c)
  (if (or (string? param)
          (symbol? param))
      (literal-chunk param)
      (concat-chunk (comment-line-chunk (car param))
                    new-line-chunk
                    (literal-chunk (cadr param)))))
;(provide macro-arg-chunk)

;macro argument list chunk
; chunks that build argument list for a macro defintion
(define/contract (macro-arg-list-chunk . params)
  (->* () #:rest (listof macro-arg/c) chunk/c)
  (cond [;no parameters
         (empty? params)
         empty-chunk]
        [;one parameter - no commas
         (= 1 (length params))
         (apply concat-chunk (list imm-open-paren-chunk
                                   (macro-arg-chunk (car params))
                                   imm-close-paren-chunk))]
        [;more than one parameter - commas!
         else
         (concat-chunk imm-open-paren-chunk
                       (apply arg-list-chunk (append (map (λ (arg) (concat-chunk (macro-arg-chunk arg) imm-comma-chunk))
                                                          (take params (- (length params) 1)))
                                                     (list (concat-chunk (macro-arg-chunk (last params)) imm-close-paren-chunk)))))]))
;(provide macro-arg-list-chunk)

;macro header chunk
; sets up define for a macro-definition
(define/contract (macro-header-chunk name params)
  (-> string-value/c (listof macro-arg/c) chunk/c)
  (concat-chunk (pp-define-chunk name)
                (apply macro-arg-list-chunk params)))
;(provide macro-header-chunk)

;TODO: Determine if chunks that build macro-defintion-chunk should be provided and/or tested directly
;macro defintion chunk
; a macro definition
(define/contract (macro-definition-chunk name params chunk)
  (-> string-value/c (listof macro-arg/c) chunk/c chunk/c)
  ;(-> string? (listof macro-arg/c) (listof chunk/c) chunk/c)
  (speculative-chunk (concat-chunk (macro-header-chunk name params)
                                   space-chunk
                                   chunk)
                     length-equals-one
                     (macro-env-chunk (concat-chunk (macro-header-chunk name params)
                                                    new-line-chunk
                                                    (indent-chunk 3 chunk)))))
(provide macro-definition-chunk)

