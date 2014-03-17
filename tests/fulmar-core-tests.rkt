#lang racket

(require rackunit)
(require "../private/fulmar-core.rkt")

;unit tests for fulmar-core.rkt

(define/provide-test-suite test-flatten*
 (test-case
  "Test flatten*"
  (check-equal? (flatten*) null)
  (check-equal? (flatten* 'a 'b '(c d e) 'f) '(a b c d e f))))
