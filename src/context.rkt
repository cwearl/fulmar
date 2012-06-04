#lang racket

(require "basics.rkt")
(require "environment.rkt")

;context structure, procedures, and contracts

;structures

;context structure
(struct context-struct (indent mode line-length env) #:transparent)

;predicates

;predicate for divide mode
(define/contract (divide? mode)
  pred/c
  (equal? 'divide mode))
(provide divide?)

;predicate for abut mode
(define/contract (abut? mode)
  pred/c
  (equal? 'abut mode))
(provide abut?)

;predicate for mode
(define/contract (mode? mode)
  pred/c
  ((or? abut?
        divide?) mode))
(provide mode?)

;predicate for context
(define/contract (context? context)
  pred/c
  (and (context-struct? context)
       (indent? (context-struct-indent context))
       (mode? (context-struct-mode context))
       (line-length? (context-struct-line-length context))
       (env? (context-struct-env context))))
(provide context?)

;predicate for context with divide mode
(define/contract (context-divide? context)
  pred/c
  (and (context? context)
       (divide? (context-mode context))))
(provide context-divide?)

;predicate for context with abut mode
(define/contract (context-abut? context)
  pred/c
  (and (context? context)
       (abut? (context-mode context))))
(provide context-abut?)

;constructors

;initial context
(define/contract (initial-context line-length)
  (-> line-length? context?)
  (context-struct 0
                  'divide
                  line-length
                  empty-env))
(provide initial-context)

;accessors

;context indent accessor
(define/contract (context-indent g)
  (-> context? indent?)
  (context-struct-indent g))
(provide context-indent)

;context mode accessor
(define/contract (context-mode g)
  (-> context? mode?)
  (context-struct-mode g))
(provide context-mode)

;context line length accessor
(define/contract (context-line-length g)
  (-> context? line-length?)
  (context-struct-line-length g))
(provide context-line-length)

;context environment accessor
(define/contract (context-env g)
  (-> context? env?)
  (context-struct-env g))
(provide context-env)

;context environment description accessor
(define/contract (context-env-description g)
  (-> context? optional-env-description?)
  (env-description (context-env g)))
(provide context-env-description)

;context environment comment indent accessor
(define/contract (context-comment-indent g)
  (-> context? optional-indent?)
  (env-comment-indent (context-env g)))
(provide context-comment-indent)

;transitioners

;increase indent level context
(define/contract (increase-indent additional-indent context)
  (-> indent? context? context?)
  (struct-copy context-struct context [indent (+ additional-indent
                                                 (context-indent context))]))
(provide increase-indent)

;reset indent level context
(define/contract (reset-indent new-indent context)
  (-> indent? context? context?)
  (struct-copy context-struct context [indent new-indent]))
(provide reset-indent)

;switch to divide mode
(define/contract (to-divide-mode context)
  (-> context? context?)
  (struct-copy context-struct context [mode 'divide]))
(provide to-divide-mode)

;switch to abut mode
(define/contract (to-abut-mode context)
  (-> context? context?)
  (struct-copy context-struct context [mode 'abut]))
(provide to-abut-mode)

;new comment block
(define/contract (enter-comment-env context)
  (-> context? context?)
  (struct-copy context-struct context [env (combine-env (context-env context)
                                                        (comment-env (context-indent context)))]))
(provide enter-comment-env)

;new macro definition
(define/contract (enter-macro-env context)
  (-> context? context?)
  (struct-copy context-struct context [env (combine-env (context-env context)
                                                        macro-env)]))
(provide enter-macro-env)
