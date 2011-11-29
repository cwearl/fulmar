#lang racket

(require rackunit)
(require "../fulmar-core.rkt")
(require "../chunk-core.rkt")

;unit tests for writer.rkt

(define/provide-test-suite test-combine-lengths
  (test-case
   "Test combine-lengths"
   (check-eq? (combine-lengths "asdf") 4)
   (check-eq? (combine-lengths 'asdf) 4)
   (check-eq? (combine-lengths 4) 4)
   (check-eq? (combine-lengths "asdf" "jkl") 7)
   (check-eq? (combine-lengths "asdf" 'jkl) 7)
   (check-eq? (combine-lengths "asdf" 3) 7)
   (check-eq? (combine-lengths 'asdf "jkl") 7)
   (check-eq? (combine-lengths 'asdf 'jkl) 7)
   (check-eq? (combine-lengths 'asdf 3) 7)
   (check-eq? (combine-lengths 4 "jkl") 7)
   (check-eq? (combine-lengths 4 'jkl) 7)
   (check-eq? (combine-lengths 4 3) 7)))

(define/provide-test-suite test-combine-strings
  (test-case
   "Test combine-strings"
   (check-equal? (combine-strings "asdf") "asdf")
   (check-equal? (combine-strings 'asdf) "asdf")
   (check-equal? (combine-strings "asdf" "jkl") "asdfjkl")
   (check-equal? (combine-strings "asdf" 'jkl) "asdfjkl")
   (check-equal? (combine-strings 'asdf "jkl") "asdfjkl")
   (check-equal? (combine-strings 'asdf 'jkl) "asdfjkl")))

(define/provide-test-suite test-empty-chunk
  (test-case
   "Test empty-chunk"
   (check-equal? (empty-chunk (construct-context 80)) (nekot 'empty null (construct-context 80)))
   (check-equal? (empty-chunk (construct-context 3)) (nekot 'empty null (construct-context 3)))))

(define/provide-test-suite test-literal-chunk
  (test-case
   "Test literal-chunk"
   (define test-context (construct-context 80))
   (check-equal? ((literal-chunk "asdf") test-context) (nekot 'literal "asdf" test-context))
   (check-equal? ((literal-chunk 'asdf) test-context) (nekot 'literal "asdf" test-context))
   (check-equal? ((literal-chunk "asdf" "jkl") test-context) (nekot 'literal "asdfjkl" test-context))
   (check-equal? ((literal-chunk "asdf" 'jkl) test-context) (nekot 'literal "asdfjkl" test-context))
   (check-equal? ((literal-chunk 'asdf "jkl") test-context) (nekot 'literal "asdfjkl" test-context))
   (check-equal? ((literal-chunk 'asdf 'jkl) test-context) (nekot 'literal "asdfjkl" test-context))))

(define/provide-test-suite test-spaces-chunk
  (test-case
   "Test spaces-chunk"
   (define test-context (construct-context 80))
   (check-equal? ((spaces-chunk "asdf") test-context) (nekot 'spaces 4 test-context))
   (check-equal? ((spaces-chunk 'asdf) test-context) (nekot 'spaces 4 test-context))
   (check-equal? ((spaces-chunk 4) test-context) (nekot 'spaces 4 test-context))
   (check-equal? ((spaces-chunk "asdf" "jkl") test-context) (nekot 'spaces 7 test-context))
   (check-equal? ((spaces-chunk "asdf" 'jkl) test-context) (nekot 'spaces 7 test-context))
   (check-equal? ((spaces-chunk "asdf" 3) test-context) (nekot 'spaces 7 test-context))
   (check-equal? ((spaces-chunk 'asdf "jkl") test-context) (nekot 'spaces 7 test-context))
   (check-equal? ((spaces-chunk 'asdf 'jkl) test-context) (nekot 'spaces 7 test-context))
   (check-equal? ((spaces-chunk 'asdf 3) test-context) (nekot 'spaces 7 test-context))
   (check-equal? ((spaces-chunk 4 "jkl") test-context) (nekot 'spaces 7 test-context))
   (check-equal? ((spaces-chunk 4 'jkl) test-context) (nekot 'spaces 7 test-context))
   (check-equal? ((spaces-chunk 4 3) test-context) (nekot 'spaces 7 test-context))))

(define/provide-test-suite test-new-line-chunk
  (test-case
   "Test new-line-chunk"
   (check-equal? (new-line-chunk (construct-context 80)) (nekot 'new-line null (construct-context 80)))
   (check-equal? (new-line-chunk (construct-context 3)) (nekot 'new-line null (construct-context 3)))))

(define/provide-test-suite test-pp-directive-chunk
  (test-case
   "Test pp-directive-chunk"
   (check-equal? (pp-directive-chunk (construct-context 80)) (nekot 'pp-directive null (construct-context 80)))
   (check-equal? (pp-directive-chunk (construct-context 3)) (nekot 'pp-directive null (construct-context 3)))))

(define/provide-test-suite test-concat-chunk
  (test-case
   "Test concat-chunk"
   (check-equal? ((concat-chunk (literal-chunk 'asdf) (literal-chunk "jkl")) (construct-context 80))
                 (nekot 'concat (list (nekot 'literal "asdf" (construct-context 80))
                                      (nekot 'literal "jkl" (construct-context 80)))
                        (construct-context 80)))
   (check-equal? ((concat-chunk (literal-chunk 'asdf) (spaces-chunk 4) (literal-chunk "jkl")) (construct-context 80))
                 (nekot 'concat (list (nekot 'literal "asdf" (construct-context 80))
                                      (nekot 'spaces 4 (construct-context 80))
                                      (nekot 'literal "jkl" (construct-context 80)))
                        (construct-context 80)))))
