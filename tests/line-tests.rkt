#lang racket

(require rackunit)
(require "../src/sequence.rkt")
(require "../src/line.rkt")

;unit tests for line.rkt

(define/provide-test-suite test-print-item?
  (test-case
   "Test print-item?"
   (check-true (print-item? #\a))
   (check-true (print-item? 3))
   (check-true (print-item? (seq #\a 3)))
   (check-true (print-item? (pivot (line #\c #\b #\a)
                                   (line #\c #\b #\a))))
   (check-false (print-item? -1))
   (check-false (print-item? "asdf"))
   (check-false (print-item? 'asdf))))

(define/provide-test-suite test-line-IR?
  (test-case
   "Test line-IR?"
   (check-true (line-IR? (list #\a)))
   (check-true (line-IR? (list 3)))
   (check-true (line-IR? (list 0)))
   (check-true (line-IR? (list (seq #\a 3))))
   (check-true (line-IR? (list (pivot (line #\c #\b #\a)
                                      (line #\c #\b #\a)))))
   (check-true (line-IR? (list #\a #\b #\c #\d)))
   (check-true (line-IR? (list 0 1 2 3 4)))
   (check-true (line-IR? (list (pivot (line #\c #\b #\a)
                                      (line #\c #\b #\a))
                               (pivot (line #\c #\b #\a)
                                      (line #\c #\b #\a)))))
   (check-true (line-IR? (list (pivot (line #\c #\b #\a)
                                      (line #\c #\b #\a))
                               #\a
                               3
                               (seq #\a #\b #\c))))
   (check-false (line-IR? #\a))
   (check-false (line-IR? 3))
   (check-false (line-IR? null))))

(define/provide-test-suite test-line-IIR?
  (test-case
   "Test line-IIR?"
   (check-true (line-IIR? (list #\a)))
   (check-true (line-IIR? (list 3)))
   (check-true (line-IIR? (list 0)))
   (check-true (line-IIR? (list (seq #\a 3))))
   (check-true (line-IIR? (list (pivot (line #\c #\b #\a)
                                      (line #\c #\b #\a)))))
   (check-true (line-IIR? (list #\a #\b #\c #\d)))
   (check-true (line-IIR? (list 0 1 2 3 4)))
   (check-true (line-IIR? (list (pivot (line #\c #\b #\a)
                                      (line #\c #\b #\a))
                               (pivot (line #\c #\b #\a)
                                      (line #\c #\b #\a)))))
   (check-true (line-IIR? (list (pivot (line #\c #\b #\a)
                                      (line #\c #\b #\a))
                               #\a
                               3
                               (seq #\a #\b #\c))))
   (check-true (line-IIR? null))
   (check-false (line-IIR? #\a))
   (check-false (line-IIR? 3))))

(define/provide-test-suite test-line-input?
  (test-case
   "Test line-input?"
   (check-true (line-input? #\a))
   (check-true (line-input? 3))
   (check-true (line-input? (seq #\a 3)))
   (check-true (line-input? (pivot (line #\c #\b #\a)
                                   (line #\c #\b #\a))))
   (check-true (line-input? (line #\a #\b #\c)))
   (check-true (line-input? (line 2 3 4 5 5)))
   (check-true (line-input? (line (pivot (line 3 #\a)
                                   (line #\a 3)))))
   (check-true (line-input? (line #\a 3 (pivot (line 3 #\a)
                                         (line #\a 3)))))
   (check-true (line-input? (list #\a
                                  3
                                  (seq 3 #\a)
                                  (pivot (line #\c #\b #\a)
                                         (line #\c #\b #\a))
                                  (line #\a #\b #\c)
                                  (line 2 3 4 5 5)
                                  (line (pivot (line 3 #\a)
                                               (line #\a 3)))
                                  (line #\a 3 (pivot (line 3 #\a)
                                                     (line #\a 3))))))
   (check-false (line-input? -1))
   (check-false (line-input? 'other))
   (check-false (line-input? "other"))))

(define/provide-test-suite test-line?
  (test-case
   "Test line?"
   (check-true (line? (line #\a #\b #\c)))
   (check-true (line? (line 2 3 4 5 5)))
   (check-true (line? (line (pivot (line 3 #\a)
                                   (line #\a 3)))))
   (check-true (line? (line #\a 3 (seq #\a 3) (pivot (line 3 #\a)
                                                     (line #\a 3)))))
   (check-false (line? 'other))
   (check-false (line? "other"))))

(define/provide-test-suite test-line
  (test-case
   "Test line"
   (check-true (line? (line)))
   (check-true (line? (line #\a #\b #\c)))
   (check-true (line? (line (line #\a #\b #\c))))
   (check-equal? (line-IR (line #\a #\b #\c)) (list #\c #\b #\a))))

(define/provide-test-suite test-line-IR
  (test-case
   "Test line-IR"
   (check-equal? (line-IR (line)) (list 0))
   (check-equal? (line-IR (line #\a #\b #\c)) (list #\c #\b #\a))
   (check-equal? (line-IR (line #\a 3 (seq #\a 3) (pivot (line #\a)
                                                         (line #\b))))
                 (list (pivot (line #\a)
                              (line #\b))
                       (seq #\a 3)
                       3
                       #\a))))

(define/provide-test-suite test-line-last
  (test-case
   "Test line-last"
   (check-equal? (line-last (line)) 0)
   (check-equal? (line-last (line #\a)) #\a)
   (check-equal? (line-last (line #\a #\b)) #\b)
   (check-equal? (line-last (line #\a #\b #\c)) #\c)))

(define/provide-test-suite test-line-rest
  (test-case
   "Test line-rest"
   (check-true (line? (line-rest (line))))
   (check-equal? (line-IR (line-rest (line))) 0)
   (check-true (line? (line-rest (line (line #\a)))))
   (check-equal? (line-IR (line-rest (line (line #\a)))) 0)
   (check-equal? (line-rest (line #\a #\b)) (line #\a))
   (check-equal? (line-IR (line-rest (line #\a #\b))) (list #\a))
   (check-equal? (line-rest (line #\a #\b #\c)) (line #\a #\b))
   (check-equal? (line-rest (line #\a #\b #\c #\d)) (line #\a #\b #\c))))

(define/provide-test-suite test-build-line-IIR
  (test-case
   "Testcase build-line-IIR"
   (check-equal? (build-line-IIR) null)
   (check-equal? (build-line-IIR null) null)
   (check-equal? (build-line-IIR (list null null)) null)
   (check-equal? (build-line-IIR #\a) (list #\a))
   (check-equal? (build-line-IIR #\a 3 (seq #\a 3) (pivot (line #\a)
                                                         (line 3)))
                 (list (pivot (line #\a)
                              (line 3))
                       (seq #\a 3)
                       3
                       #\a))
   (check-equal? (build-line-IIR (line #\a #\b)) (list #\b #\a))
   (check-equal? (build-line-IIR (line #\a #\b) (line #\c #\d)) (list #\d #\c #\b #\a))
   (check-equal? (build-line-IIR (line #\a #\b) #\c (line #\d #\e)) (list #\e #\d #\c #\b #\a))))

(define/provide-test-suite test-simplify-line-IIR
  (test-case
   "Test simplify-line-IIR"
   (check-equal? (simplify-line-IIR (list 0)) null)
   (check-equal? (simplify-line-IIR (list 0 0)) null)
   (check-equal? (simplify-line-IIR (list 3 7 1 0 2)) (list 13))
   (check-equal? (simplify-line-IIR (list 3 7 #\a 1 0 2)) (list 10 #\a 3))
   (check-equal? (simplify-line-IIR (list 0 3 2 1 0 #\a (pivot (line #\a)
                                                              (line 3))))
                 (list 6 #\a (pivot (line #\a)
                                    (line 3))))))

(define/provide-test-suite test-pivot-IR?
  (test-case
   "Test pivot-IR?"
   (check-true (pivot-IR? (list (line #\c #\a)
                                (line #\c #\a))))
   (check-true (pivot-IR? (list (line #\c #\a)
                                (line #\c #\a)
                                (line #\c #\a))))
   (check-false (pivot-IR? (list (line #\c #\a))))))

(define/provide-test-suite test-pivot-IIR?
  (test-case
   "Test pivot-IIR?"
   (check-true (pivot-IIR? null))
   (check-true (pivot-IIR? (list (line #\c #\a))))
   (check-true (pivot-IIR? (list (line #\c #\a)
                                 (line #\c #\a))))
   (check-true (pivot-IIR? (list (line #\c #\a)
                                 (line #\c #\a)
                                 (line #\c #\a))))
   (check-false (pivot-IIR? (line #\c #\a)))))

(define/provide-test-suite test-pivot-input?
  (test-case
   "Test pivot-input?"
   (check-true (pivot-input? null))
   (check-true (pivot-input? (list null null)))
   (check-true (pivot-input? (line #\c #\a)))
   (check-true (pivot-input? (list (line #\c #\a))))
   (check-true (pivot-input? (list (line #\c #\a)
                                   (line #\c #\a))))
   (check-true (pivot-input? (list (line #\c #\a)
                                   (line #\c #\a)
                                   (line #\c #\a))))
   (check-false (pivot-input? 'other))))

(define/provide-test-suite test-pivot?
  (test-case
   "Test pivot?"
   (check-true (pivot? (pivot (line #\a) (line #\b))))
   (check-true (pivot? (pivot (line #\a) (line #\b) (line #\c))))
   (check-false (pivot? (pivot (line #\a))))
   (check-false (pivot? (pivot)))
   (check-false (pivot? (line #\a)))
   (check-false (pivot? (list (line #\a) (line #\b))))))

(define/provide-test-suite test-pivot-output?
  (test-case
   "Test pivot-output?"
   (check-true (pivot-output? (pivot)))
   (check-true (pivot-output? (pivot (line #\a))))
   (check-true (pivot-output? (pivot (line #\a) (line #\b))))
   (check-false (pivot-output? -1))
   (check-false (pivot-output? 'other))))

(define/provide-test-suite test-pivot
  (test-case
   "Test pivot"
   (check-true (pivot? (pivot (line #\a) (line #\b))))
   (check-true (line? (pivot (line #\a))))
   (check-equal? (pivot) 0)
   (check-equal? (pivot null) 0)
   (check-equal? (pivot (list null null)) 0)
   (check-equal? (pivot-IR (pivot (line #\a) (line #\b)))
                 (list (line #\b) (line #\a)))
   (check-equal? (pivot (pivot (line #\a) (line #\b))) (pivot (line #\a) (line #\b)))
   (check-equal? (pivot (line #\a) (pivot (line #\b) (line #\c)) (line #\d))
                 (pivot (line #\a) (line #\b) (line #\c) (line #\d)))
   (check-equal? (pivot-length (pivot (line #\a) (line #\b))) 3)
   (check-equal? (pivot-length (pivot (line #\a) (line #\b #\c))) 4)
   (check-equal? (pivot-length (pivot (line #\a) (line #\b #\c) (line 5 3))) 13)))

(define/provide-test-suite test-pivot-IR
  (test-case
   "Test pivot-IR"
   (check-equal? (pivot-IR (pivot (line #\a) (line #\b)))
                 (list (line #\b) (line #\a)))
   (check-equal? (pivot-IR (pivot (line #\a) (line #\b) (line #\c)))
                 (list (line #\c) (line #\b) (line #\a)))))

(define/provide-test-suite test-pivot-length
  (test-case
   "Test pivot-length"
   (check-equal? (pivot-length (pivot (line #\a) (line #\b))) 3)
   (check-equal? (pivot-length (pivot (line #\a) (line #\b) (line #\c))) 5)
   (check-equal? (pivot-length (pivot (line #\a) (line #\b #\c) (line 5 3))) 13)))
                 
(define/provide-test-suite test-build-pivot-IIR
  (test-case
   "Test build-pivot-IIR"
   (check-true (pivot-IR? (build-pivot-IIR (list (line #\a) (line #\b)))))
   (check-equal? (build-pivot-IIR (list (line #\a)))
                 (list (line #\a)))
   (check-equal? (build-pivot-IIR null) null)
   (check-equal? (build-pivot-IIR (list (line #\a) (line #\b)))
                 (list (line #\b) (line #\a)))))

(define/provide-test-suite test-pivot-full-line-length
  (test-case
   "Test pivot-full-line-length"
   (check-equal? (pivot-full-line-length (list (line #\a))) 1)
   (check-equal? (pivot-full-line-length (list (line 4))) 4)
   (check-equal? (pivot-full-line-length (list (line #\a) (line #\b))) 3)
   (check-equal? (pivot-full-line-length (list (line #\a) (line #\b) (line #\c))) 5)
   (check-equal? (pivot-full-line-length (list (line #\a) (line #\b) (line 4))) 8)
   (check-equal? (pivot-full-line-length (list (line (pivot (line #\a) (line #\b))) (line #\c))) 5)
   (check-equal? (pivot-full-line-length (list (line (pivot (line #\a) (line #\b)))
                                               (line (pivot (line #\a) (line #\b)))
                                               (line #\c)))
                 9)))

(define/provide-test-suite test-pivot-last
  (test-case
   "Test pivot-last"
   (check-equal? (pivot-last (pivot (line #\a) (line #\b))) (line #\b))
   (check-equal? (pivot-last (pivot (line #\a) (line #\b) (line #\c))) (line #\c))))

(define/provide-test-suite test-pivot-rest
  (test-case
   "Test pivot-rest"
   (check-equal? (pivot-rest (pivot (line #\a) (line #\b))) (line #\a))
   (check-equal? (pivot-rest (pivot (line #\a) (line #\b) (line #\c))) (pivot (line #\a) (line #\b)))))

(define/provide-test-suite test-full-line-length
  (test-case
   "Test full-line-length"
   (check-equal? (full-line-length (line #\a)) 1)
   (check-equal? (full-line-length (line #\a #\b)) 2)
   (check-equal? (full-line-length (line #\a 3)) 4)
   (check-equal? (full-line-length (line #\a 3 (pivot (line #\a) (line #\b)))) 7)))

(define/provide-test-suite test-full-seq-length
  (test-case
   "Test full-seq-length"
   (check-equal? (full-seq-length (seq #\a #\b)) 2)
   (check-equal? (full-seq-length (seq #\a #\b #\c)) 3)
   (check-equal? (full-seq-length (seq #\a 3)) 4)
   (check-equal? (full-seq-length (seq #\a 3 #\a #\b)) 6)))
