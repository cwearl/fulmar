#lang racket

(require rackunit)
(require "../src/basics.rkt")

;unit tests for basics.rkt

(define/provide-test-suite test-flatten*
 (test-case
  "Test flatten*"
  (check-equal? (flatten*) null)
  (check-equal? (flatten* (list null (list null (list null null)))) null)
  (check-equal? (flatten* 'a 'b (list 'c 'd 'e) 'f) (list 'a 'b 'c 'd 'e 'f))))

(define/provide-test-suite test-non-empty-list?
  (test-case
   "Test non-empty-list?"
   (check-true (non-empty-list? (list 'a)))
   (check-false (non-empty-list? (list)))))

(define/provide-test-suite test-or?
  (test-case
   "Test or?"
   (check-true ((or? positive? even?) 4))
   (check-true ((or? positive? even?) 3))
   (check-true ((or? positive? even?) 0))
   (check-false ((or? positive? even?) -1))))

(define/provide-test-suite test-and?
  (test-case
   "Test and?"
   (check-true ((and? positive? even?) 4))
   (check-false ((and? positive? even?) 3))
   (check-false ((and? positive? even?) 0))
   (check-false ((and? positive? even?) -1))))

(define/provide-test-suite test-list-of?
  (test-case
   "Test list-of? - uncurried - test length"
   (check-true (list-of? char? 0 #\a (list #\b #\c) #\d))
   (check-true (list-of? char? 0 #\a))
   (check-true (list-of? char? 4 #\a (list #\b #\c) #\d))
   (check-false (list-of? char? 5 #\a (list #\b #\c) #\d)))
  (test-case
   "Test list-of? - curried - test length"
   (check-true ((list-of? char? 0) #\a (list #\b #\c) #\d))
   (check-true ((list-of? char? 0) #\a))
   (check-true ((list-of? char? 4) #\a (list #\b #\c) #\d))
   (check-false ((list-of? char? 1)))
   (check-false ((list-of? char? 5) #\a (list #\b #\c) #\d)))
  (test-case
   "Test list-of? - uncurried - test predicate"
   (check-true (list-of? char? 0 #\a (list #\b #\c) #\d))
   (check-true (list-of? integer? 0 2 1 5 10 1000))
   (check-false (list-of? char? 0 #\a (list #\b #\c) #\d 0))
   (check-false (list-of? char? 0 2 1 6)))
  (test-case
   "Test list-of? - curried - test predicate"
   (check-true ((list-of? char? 0) #\a (list #\b #\c) #\d))
   (check-true ((list-of? integer? 0) 2 1 5 10 1000))
   (check-false ((list-of? char? 0) #\a (list #\b #\c) #\d 0))
   (check-false ((list-of? char? 0) 2 1 6))))

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
