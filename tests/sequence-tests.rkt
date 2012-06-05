#lang racket

(require rackunit)
(require "../src/sequence.rkt")

;unit tests for sequence.rkt

(define/provide-test-suite test-seq-IR?
  (test-case
   "Test seq-IR?"
   (check-true (seq-IR? (list #\a #\b)))
   (check-true (seq-IR? (list 3 4)))
   (check-true (seq-IR? (list #\a 3 #\b)))
   (check-false (seq-IR? #\a))
   (check-false (seq-IR? (list #\a)))
   (check-false (seq-IR? 'other))))

(define/provide-test-suite test-seq-IIR?
  (test-case
   "Test seq-IIR?"
   (check-true (seq-IIR? null))
   (check-true (seq-IIR? (list #\a #\b)))
   (check-true (seq-IIR? (list 3 4)))
   (check-true (seq-IIR? (list #\a 3 #\b)))
   (check-true (seq-IIR? (list #\a)))
   (check-false (seq-IIR? #\a))
   (check-false (seq-IIR? 'other))))

(define/provide-test-suite test-seq-input?
  (test-case
   "Test seq-input?"
   (check-true (seq-input? null))
   (check-true (seq-input? (list null null)))
   (check-true (seq-input? #\a))
   (check-true (seq-input? 3))
   (check-true (seq-input? (seq #\a #\b)))
   (check-true (seq-input? (list #\a)))
   (check-true (seq-input? (list #\a #\b)))
   (check-true (seq-input? (list #\a #\b #\c)))
   (check-false (seq-input? -1))
   (check-false (seq-input? 'other))))

(define/provide-test-suite test-seq?
  (test-case
   "Test seq?"
   (check-false (seq? (seq)))
   (check-false (seq? (seq null)))
   (check-false (seq? (seq (list null null))))
   (check-false (seq? (seq 3)))
   (check-false (seq? (seq #\a)))
   (check-true (seq? (seq #\a #\b)))
   (check-true (seq? (seq #\a #\b)))
   (check-true (seq? (seq #\a 3)))
   (check-true (seq? (seq #\a #\b)))
   (check-true (seq? (seq (seq #\a #\b))))
   (check-false (seq? 'other))))

(define/provide-test-suite test-seq-output?
  (test-case
   "Test seq-output?"
   (check-true (seq-output? (seq)))
   (check-true (seq-output? (seq null)))
   (check-true (seq-output? (seq (list null null))))
   (check-true (seq-output? (seq 3)))
   (check-true (seq-output? (seq #\a)))
   (check-true (seq-output? (seq #\a #\b)))
   (check-true (seq-output? (seq #\a #\b)))
   (check-true (seq-output? (seq #\a 3)))
   (check-true (seq-output? (seq #\a #\b)))
   (check-true (seq-output? (seq (seq #\a #\b))))
   (check-false (seq-output? 'other))))

(define/provide-test-suite test-seq
  (test-case
   "Test seq"
   (check-equal? (seq) 0)
   (check-equal? (seq null) 0)
   (check-equal? (seq (list null null)) 0)
   (check-equal? (seq 3) 3)
   (check-equal? (seq 3 4) 7)
   (check-equal? (seq #\a) #\a)
   (check-true (seq? (seq #\a #\b)))
   (check-true (seq? (seq #\a #\b)))
   (check-equal? (seq-IR (seq 3 #\a 4 #\b 0)) (list #\b 4 #\a 3))
   (check-equal? (seq-IR (seq #\a #\b)) (list #\b #\a))
   (check-equal? (seq-IR (seq (seq #\a #\b))) (list #\b #\a))))

(define/provide-test-suite test-seq-IR
  (test-case
   "Test seq-IR"
   (check-equal? (seq-IR (seq #\a #\b))
                 (list #\b #\a))
   (check-equal? (seq-IR (seq #\a #\b #\c))
                 (list #\c #\b #\a))))

(define/provide-test-suite test-build-seq-IIR
  (test-case
   "Test build-seq-IIR"
   (check-true (seq-IIR? (build-seq-IIR (list #\a #\b))))
   (check-equal? (build-seq-IIR (list #\a)) (list #\a))
   (check-equal? (build-seq-IIR null) null)
   (check-equal? (build-seq-IIR (list #\a #\b))
                 (list #\b #\a))
   (check-equal? (build-seq-IIR (list 3 #\a 4)) (list 4 #\a 3))))

(define/provide-test-suite test-simplify-seq-IIR
  (test-case
   "Test simplify-seq-IIR"
   (check-equal? (simplify-seq-IIR null) null)
   (check-equal? (simplify-seq-IIR (list #\a #\b)) (list #\a #\b))
   (check-equal? (simplify-seq-IIR (list 3 5)) (list 8))
   (check-equal? (simplify-seq-IIR (list 3 5 #\a)) (list 8 #\a))
   (check-equal? (simplify-seq-IIR (list 3 5 #\a 3)) (list 8 #\a 3))))
