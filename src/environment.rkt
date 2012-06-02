#lang racket

(require "basics.rkt")

;environment structure, procedures, and contracts

;structures

;environment structure
(struct env-struct (description comment-indent) #:transparent)

;predicates

;predicate for environment description
(define/contract (env-description? g)
  pred/c
  (match g
    [(or 'comment
         'macro
         'comment-macro
         'macro-comment) #true]
    [_                   #false]))
(provide env-description?)

;predicate for optional environment description
(define/contract (optional-env-description? g)
  pred/c
  ((or? not
        env-description?) g))
(provide optional-env-description?)

;predicate for empty environment
(define/contract empty-env?
  pred/c
  not)
(provide empty-env?)

;predicate for comment block environment
(define/contract (comment-env? g)
  pred/c
  (and (env-struct? g)
       (equal? 'comment
               (env-struct-description g))))
(provide comment-env?)

;predicate for macro definition environment
(define/contract (macro-env? g)
  pred/c
  (and (env-struct? g)
       (equal? 'macro
               (env-struct-description g))))
(provide macro-env?)

;predicate for commented macro defintion environment
(define/contract (comment-macro-env? g)
  pred/c
  (and (env-struct? g)
       (equal? 'comment-macro
               (env-struct-description g))))
(provide comment-macro-env?)

;predicate for macro definition with embedded comment block environment
(define/contract (macro-comment-env? g)
  pred/c
  (and (env-struct? g)
       (equal? 'macro-comment
               (env-struct-description g))))
(provide macro-comment-env?)

;predicate for environment
(define/contract (env? g)
  pred/c
  ((or? empty-env?
        comment-env?
        macro-env?
        comment-macro-env?
        macro-comment-env?) g))
(provide env?)

;predicate for user-definable environment
(define/contract (user-env? g)
  pred/c
  ((or? empty-env?
        macro-env?
        comment-env?) g))
(provide user-env?)

;constructors

;empty environment
(define/contract empty-env
  empty-env?
  #false)
(provide empty-env? empty-env)

;comment block environment
(define/contract (comment-env indent)
  (-> indent? comment-env?)
  (env-struct 'comment indent))
(provide comment-env)

;macro definition environment
(define/contract macro-env
  macro-env?
  (env-struct 'macro #false))
(provide macro-env)

;commented macro defintion environment
(define/contract (comment-macro-env indent)
  (-> indent? comment-macro-env?)
  (env-struct 'comment-macro indent))
(provide comment-macro-env)

;macro definition with embedded comment block environment
(define/contract (macro-comment-env indent)
  (-> indent? macro-comment-env?)
  (env-struct 'macro-comment indent))
(provide macro-comment-env)

;accessors

;environment description accessor
(define/contract (env-description g)
  (-> env? optional-env-description?)
  (if g
      (env-struct-description g)
      #false))
(provide env-description)

;environment comment indent accessor
(define/contract (env-comment-indent g)
  (-> env? optional-indent?)
  (if g
      (env-struct-comment-indent g)
      #false))
(provide env-comment-indent)

;transitioners

;combine two environments
(define/contract (combine-env old new)
  (-> env? user-env? env?)
  (cond [(empty-env? old) ;was in empty env
         new]
        [(empty-env? new) ;entering empty env
         old]
        [(and (comment-env? new)            ;entering: comment env
              ((or? comment-env?            ;in: comment-type env (comment, macro-comment, or comment-macro)
                    comment-macro-env?
                    macro-comment-env?) old))
         old]
        [(and (comment-env? new)            ;entering: comment env
              (macro-env? old))             ;in: macro env
         (macro-comment-env (env-comment-indent new))]
        [(and (macro-env? new)              ;entering: macro env
              (comment-env? old))           ;in: comment env
         (comment-macro-env (env-comment-indent old))]
        [else ;else error...
         (error "Incompatible environments combined; given: " old new)]))
(provide combine-env)
