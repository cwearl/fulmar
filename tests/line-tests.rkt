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
   (check-true (line-IR? #\a))
   (check-true (line-IR? 3))
   (check-true (line-IR? 0))
   (check-true (line-IR? (seq #\a 3)))
   (check-true (line-IR? (pivot (line #\c #\b #\a)
                                (line #\c #\b #\a))))
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
   (check-false (line-IR? -1))
   (check-false (line-IR? null))))

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
   (check-equal? (line-IR (line)) 0)
   (check-equal? (line-IR (line #\a #\b #\c)) (list #\c #\b #\a))
   (check-equal? (line-IR (line #\a 3 (seq #\a 3) (pivot (line #\a)
                                                         (line #\b))))
                 (list (pivot (line #\a)
                              (line #\b))
                       (seq #\a 3)
                       3
                       #\a))))

(define/provide-test-suite test-build-line-IR
  (test-case
   "Test-case"
   (check-equal? (build-line-IR) 0)
   (check-equal? (build-line-IR null) 0)
   (check-equal? (build-line-IR (list null null)) 0)
   (check-equal? (build-line-IR #\a) (list #\a))
   (check-equal? (build-line-IR #\a 3 (seq #\a 3) (pivot (line #\a)
                                                         (line 3)))
                 (list (pivot (line #\a)
                              (line 3))
                       (seq #\a 3)
                       3
                       #\a))
   (check-equal? (build-line-IR (line #\a #\b)) (list #\b #\a))
   (check-equal? (build-line-IR (line #\a #\b) (line #\c #\d)) (list #\d #\c #\b #\a))
   (check-equal? (build-line-IR (line #\a #\b) #\c (line #\d #\e)) (list #\e #\d #\c #\b #\a))))

(define/provide-test-suite test-simplify-line-IR
  (test-case
   "Test simplify-line-IR"
   (check-equal? (simplify-line-IR (list 0)) 0)
   (check-equal? (simplify-line-IR (list 0 0)) 0)
   (check-equal? (simplify-line-IR (list 3 7 1 0 2)) (list 13))
   (check-equal? (simplify-line-IR (list 3 7 #\a 1 0 2)) (list 10 #\a 3))
   (check-equal? (simplify-line-IR (list 0 3 2 1 0 #\a (pivot (line #\a)
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
   (check-equal? (pivot-length (pivot (line #\a) (line #\b) (line #\c))) 5)))
                 
(define/provide-test-suite test-build-pivot
  (test-case
   "Test build-pivot"
   (check-true (pivot? (build-pivot (list (line #\a) (line #\b)))))
   (check-true (line? (build-pivot (list (line #\a)))))
   (check-equal? (build-pivot null) 0)
   (check-equal? (pivot-IR (build-pivot (list (line #\a) (line #\b))))
                 (list (line #\b) (line #\a)))
   (check-equal? (pivot-length (build-pivot (list (line #\a) (line #\b)))) 3)
   (check-equal? (pivot-length (build-pivot (list (line #\a) (line #\b #\c)))) 4)
   (check-equal? (pivot-length (build-pivot (list (line #\a) (line #\b #\c) (line 5 3)))) 13)))

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

(define/provide-test-suite test-full-line-length
  (test-case
   "Test full-line-length"
   (check-equal? (full-line-length (line #\a)) 1)
   (check-equal? (full-line-length (line #\a #\b)) 2)
   (check-equal? (full-line-length (line #\a 3)) 4)
   (check-equal? (full-line-length (line #\a 3 (pivot (line #\a) (line #\b)))) 7)))
