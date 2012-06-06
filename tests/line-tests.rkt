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
   (check-equal? (line-IR (line-rest (line))) (list 0))
   (check-true (line? (line-rest (line (line #\a)))))
   (check-equal? (line-IR (line-rest (line (line #\a)))) (list 0))
   (check-equal? (line-rest (line #\a #\b)) (line #\a))
   (check-equal? (line-IR (line-rest (line #\a #\b))) (list #\a))
   (check-equal? (line-rest (line #\a #\b #\c)) (line #\a #\b))
   (check-equal? (line-rest (line #\a #\b #\c #\d)) (line #\a #\b #\c))))

(define/provide-test-suite test-line-first
  (test-case
   "Test line-first"
   (check-equal? (line-first (line)) 0)
   (check-equal? (line-first (line #\a)) #\a)
   (check-equal? (line-first (line #\a #\b)) #\a)
   (check-equal? (line-first (line #\a #\b #\c)) #\a)))

(define/provide-test-suite test-line-tser
  (test-case
   "Test line-tser"
   (check-true (line? (line-tser (line))))
   (check-equal? (line-IR (line-tser (line))) (list 0))
   (check-true (line? (line-tser (line (line #\a)))))
   (check-equal? (line-IR (line-tser (line (line #\a)))) (list 0))
   (check-equal? (line-tser (line #\a #\b)) (line #\b))
   (check-equal? (line-IR (line-tser (line #\a #\b))) (list #\b))
   (check-equal? (line-tser (line #\a #\b #\c)) (line #\b #\c))
   (check-equal? (line-tser (line #\a #\b #\c #\d)) (line #\b #\c #\d))))

(define/provide-test-suite test-add-to-last
  (test-case
   "Test add-to-last"
   (check-equal? (add-to-last (line #\a) #\b)
                 (line #\a #\b))
   (check-equal? (add-to-last (line #\a #\b) #\c)
                 (line #\a #\b #\c))
   (check-equal? (add-to-last (line #\a 4) 3)
                 (line #\a 7))
   (check-equal? (add-to-last (line #\a) (pivot (line #\b) (line #\c)))
                 (line #\a (pivot (line #\b) (line #\c))))
   (check-equal? (add-to-last (line (pivot (line #\a) (line #\b))) #\c)
                 (line (pivot (line #\a) (line #\b)) #\c))
   (check-equal? (add-to-last (line (pivot (line #\a) (line #\b)))
                              (pivot (line #\c) (line #\d)))
                 (line (pivot (line #\a) (line #\b))
                       (pivot (line #\c) (line #\d))))
   (check-equal? (add-to-last (line (seq #\a #\b)) #\c)
                 (line (seq #\a #\b) #\c))
   (check-equal? (add-to-last (line #\a) (seq #\b #\c))
                 (line #\a (seq #\b #\c)))
   (check-equal? (add-to-last (line (seq #\a #\b)) (seq #\c #\d))
                 (line (seq #\a #\b) (seq #\c #\d)))
   (check-equal? (add-to-last (line (pivot (line #\a) (line #\b)))
                              (seq #\c #\d))
                 (line (pivot (line #\a) (line #\b)) (seq #\c #\d)))
   (check-equal? (add-to-last (line (seq #\a #\b)) (pivot (line #\c) (line #\d)))
                 (line (seq #\a #\b) (pivot (line #\c) (line #\d))))))

(define/provide-test-suite test-add-to-first
  (test-case
   "Test add-to-first"
   (check-equal? (add-to-first #\a (line #\b))
                 (line #\a #\b))
   (check-equal? (add-to-first #\a (line #\b #\c))
                 (line #\a #\b #\c))
   (check-equal? (add-to-first 3 (line 4 #\a))
                 (line 7 #\a))
   (check-equal? (add-to-first (pivot (line #\a) (line #\b)) (line #\c))
                 (line (pivot (line #\a) (line #\b)) #\c))
   (check-equal? (add-to-first #\a (line (pivot (line #\b) (line #\c))))
                 (line #\a (pivot (line #\b) (line #\c))))
   (check-equal? (add-to-first (pivot (line #\a) (line #\b))
                               (line (pivot (line #\c) (line #\d))))
                 (line (pivot (line #\a) (line #\b))
                       (pivot (line #\c) (line #\d))))
   (check-equal? (add-to-first #\a (line (seq #\b #\c)))
                 (line #\a (seq #\b #\c)))
   (check-equal? (add-to-first (seq #\a #\b) (line #\c))
                 (line (seq #\a #\b) #\c))
   (check-equal? (add-to-first (seq #\a #\b) (line (seq #\c #\d)))
                 (line (seq #\a #\b) (seq #\c #\d)))
   (check-equal? (add-to-first (seq #\a #\b)
                               (line (pivot (line #\c) (line #\d))))
                 (line (seq #\a #\b) (pivot (line #\c) (line #\d))))
   (check-equal? (add-to-first (pivot (line #\a) (line #\b)) (line (seq #\c #\d)))
                 (line (pivot (line #\a) (line #\b)) (seq #\c #\d)))))

(define/provide-test-suite test-seq-with-last
  (test-case
   "Test seq-with-last"
   (check-equal? (seq-with-last (line #\a) #\b)
                 (line (seq #\a #\b)))
   (check-equal? (seq-with-last (line #\a #\b) #\c)
                 (line #\a (seq #\b #\c)))
   (check-equal? (seq-with-last (line #\a 4) 3)
                 (line #\a 7))
   (check-equal? (seq-with-last (line #\a) (pivot (line #\b) (line #\c)))
                 (line (pivot (line (seq #\a #\b)) (line #\c))))
   (check-equal? (seq-with-last (line (pivot (line #\a) (line #\b))) #\c)
                 (line (pivot (line #\a) (line (seq #\b #\c)))))
   (check-equal? (seq-with-last (line (pivot (line #\a) (line #\b)))
                                (pivot (line #\c) (line #\d)))
                 (line (pivot (line #\a)
                              (line (seq #\b #\c))
                              (line #\d))))
   (check-equal? (seq-with-last (line (seq #\a #\b)) #\c)
                 (line (seq #\a #\b #\c)))
   (check-equal? (seq-with-last (line #\a) (seq #\b #\c))
                 (line (seq #\a #\b #\c)))
   (check-equal? (seq-with-last (line (seq #\a #\b)) (seq #\c #\d))
                 (line (seq #\a #\b #\c #\d)))
   (check-equal? (seq-with-last (line (pivot (line #\a) (line #\b)))
                                (seq #\c #\d))
                 (line (pivot (line #\a) (line (seq #\b #\c #\d)))))
   (check-equal? (seq-with-last (line (seq #\a #\b)) (pivot (line #\c) (line #\d)))
                 (line (pivot (line (seq #\a #\b #\c))
                              (line #\d))))))

(define/provide-test-suite test-seq-with-first
  (test-case
   "Test seq-with-first"
   (check-equal? (seq-with-first #\a (line #\b))
                 (line (seq #\a #\b)))
   (check-equal? (seq-with-first #\a (line #\b #\c))
                 (line (seq #\a #\b) #\c))
   (check-equal? (seq-with-first 3 (line 4 #\a))
                 (line 7 #\a))
   (check-equal? (seq-with-first (pivot (line #\a) (line #\b)) (line #\c))
                 (line (pivot (line #\a) (line (seq #\b #\c)))))
   (check-equal? (seq-with-first #\a (line (pivot (line #\b) (line #\c))))
                 (line (pivot (line (seq #\a #\b)) (line #\c))))
   (check-equal? (seq-with-first (pivot (line #\a) (line #\b))
                                 (line (pivot (line #\c) (line #\d))))
                 (line (pivot (line #\a)
                              (line (seq #\b #\c))
                              (line #\d))))
   (check-equal? (seq-with-first #\a (line (seq #\b #\c)))
                 (line (seq #\a #\b #\c)))
   (check-equal? (seq-with-first (seq #\a #\b) (line #\c))
                 (line (seq #\a #\b #\c)))
   (check-equal? (seq-with-first (seq #\a #\b) (line (seq #\c #\d)))
                 (line (seq #\a #\b #\c #\d)))
   (check-equal? (seq-with-first (seq #\a #\b)
                                 (line (pivot (line #\c) (line #\d))))
                 (line (pivot (line (seq #\a #\b #\c)) (line #\d))))
   (check-equal? (seq-with-first (pivot (line #\a) (line #\b)) (line (seq #\c #\d)))
                 (line (pivot (line #\a)
                              (line (seq #\b #\c #\d)))))))

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

(define/provide-test-suite test-pivot-first
  (test-case
   "Test pivot-first"
   (check-equal? (pivot-first (pivot (line #\a) (line #\b))) (line #\a))
   (check-equal? (pivot-first (pivot (line #\a) (line #\b) (line #\c))) (line #\a))))

(define/provide-test-suite test-pivot-tser
  (test-case
   "Test pivot-tser"
   (check-equal? (pivot-tser (pivot (line #\a) (line #\b))) (line #\b))
   (check-equal? (pivot-tser (pivot (line #\a) (line #\b) (line #\c))) (pivot (line #\b) (line #\c)))))

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
