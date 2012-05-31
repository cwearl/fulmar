#lang racket

;basic procedures, contracts, and the like for fulmar

;basic helper functions

;flattens any list
(define/contract (flatten* . lst)
  (->* () #:rest (listof any/c) (listof any/c))
  (flatten lst))
(provide flatten*)

;predicate for a non-empty list
(define/contract (non-empty-list? lst)
  (-> any/c boolean?)
  (and (list? lst)
       (not (null? lst))))
(provide non-empty-list?)

;predicates

;predicate for indent/position
(define/contract (indent? g)
  (-> any/c boolean?)
  (exact-nonnegative-integer? g))
(provide indent?)

;predicate for optional indent/position
(define/contract (optional-indent? g)
  (-> any/c boolean?)
  (or (not g)
      (indent? g)))
(provide optional-indent?)

;predicate for line length value
(define/contract line-length?
  (-> any/c boolean?)
  exact-positive-integer?)
(provide line-length?)
