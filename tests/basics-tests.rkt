#lang racket

(require rackunit)
(require "../src/basics.rkt")

;unit tests for basics.rkt

(define/provide-test-suite test-flatten*
 (test-case
  "Test flatten*"
  (check-equal? (flatten*) null)
  (check-equal? (flatten* 'a 'b (list 'c 'd 'e) 'f) (list 'a 'b 'c 'd 'e 'f))))

(define/provide-test-suite test-non-empty-list?
  (test-case
   "Test non-empty-list?"
   (check-true (non-empty-list? (list 'a)))
   (check-false (non-empty-list? (list)))))

(define/provide-test-suite test-indent?
  (test-case
   "Test indent?"
   (check-true (indent? 3))
   (check-true (indent? 0))
   (check-false (indent? -1))))

(define/provide-test-suite test-optional-indent?
  (test-case
   "Test optional-indent?"
   (check-true (optional-indent? #false))
   (check-true (optional-indent? 3))
   (check-true (optional-indent? 0))
   (check-false (optional-indent? -1))))

(define/provide-test-suite test-line-length?
  (test-case
   "Test line-length?"
   (check-true (line-length? 3))
   (check-false (line-length? 0))
   (check-false (line-length? -1))))
