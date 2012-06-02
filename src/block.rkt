#lang racket

(require "basics.rkt")
(require "line.rkt")

;structures, functions, and contracts for block

;structures

;block structure
(struct block-struct (IR) #:transparent)

;predicates

;predicate for block internal representation
(define/contract (block-IR? g)
  (-> any/c boolean?)
  (list-of? line? 0 g))
(provide block-IR?)

;predicate for block internal representation
(define/contract (block-input? g)
  (-> any/c boolean?)
  (list-of? (or? line? block?) 0 g))
(provide block-input?)

;predicate for block
(define/contract (block? g)
  (-> any/c boolean?)
  (and (block-struct? g)
       (block-IR? (block-struct-IR g))))
(provide block?)

;constructors

;block constructor
(define/contract (block . lines)
  (->* () #:rest block-input? block?)
  (block-struct (build-block-IR lines)))
(provide block)

;accessors

;block accessor
(define/contract (block-IR lb)
  (-> block? block-IR?)
  (block-struct-IR lb))
(provide block-IR)

;general procedures

;build block internal representation
(define/contract (build-block-IR . g)
  (->* () #:rest block-input? block-IR?)
  (let ([items (flatten* g)])
    (if (null? items)
        (line 0)
        ;foldl (as opposed to foldr) is used to reverse list as it builds it
        (foldl (λ (item bir)
                 (cond [(line? item) (cons item bir)]
                       [(block? item) (append (block-IR item) bir)]
                       [else (error "Unrecognized input for block; unrecognized: " item "; given: " items)]))
               null
               items))))
(provide build-block-IR)
