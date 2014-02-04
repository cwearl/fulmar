#lang racket

(provide (all-defined-out))

(define (flatten* . lst)
  (flatten lst))

;structure chunk definition
(struct s-chunk (name body) #:transparent)