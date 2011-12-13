#lang racket

(require rackunit)
(require "../fulmar-core.rkt")
(require "../chunk-core.rkt")

;unit tests for chunk-core.rkt

;helper functions

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
   (check-eq? (combine-lengths 4 3) 7)
   (check-eq? (combine-lengths '(4 3)) 7)))

(define/provide-test-suite test-combine-strings
  (test-case
   "Test combine-strings"
   (check-equal? (combine-strings "asdf") "asdf")
   (check-equal? (combine-strings 'asdf) "asdf")
   (check-equal? (combine-strings "asdf" "jkl") "asdfjkl")
   (check-equal? (combine-strings "asdf" 'jkl) "asdfjkl")
   (check-equal? (combine-strings 'asdf "jkl") "asdfjkl")
   (check-equal? (combine-strings 'asdf 'jkl) "asdfjkl")
   (check-equal? (combine-strings '(asdf jkl)) "asdfjkl")))

(define/provide-test-suite test-length-equals-one
  (test-case
   "Test length-equals-one"
   (check-true (length-equals-one (list "")))
   (check-true (length-equals-one (list "  ")))
   (check-true (length-equals-one (list "abcd")))
   (check-false (length-equals-one (list "" "")))
   (check-false (length-equals-one (list "" "  ")))
   (check-false (length-equals-one (list "  " "")))
   (check-false (length-equals-one (list "  " "  ")))
   (check-false (length-equals-one (list "abcd" "")))
   (check-false (length-equals-one (list "" "abcd")))
   (check-false (length-equals-one (list "abcd" "abcd")))))

;nekot-building chunks

(define/provide-test-suite test-literal-chunk
  (test-case
   "Test literal-chunk"
   (define test-context (construct-context 80))
   (check-equal? ((literal-chunk "asdf") test-context) (nekot 'literal "asdf" test-context))
   (check-equal? ((literal-chunk 'asdf) test-context) (nekot 'literal "asdf" test-context))
   (check-equal? ((literal-chunk "asdf" "jkl") test-context) (nekot 'literal "asdfjkl" test-context))
   (check-equal? ((literal-chunk "asdf" 'jkl) test-context) (nekot 'literal "asdfjkl" test-context))
   (check-equal? ((literal-chunk 'asdf "jkl") test-context) (nekot 'literal "asdfjkl" test-context))
   (check-equal? ((literal-chunk 'asdf 'jkl) test-context) (nekot 'literal "asdfjkl" test-context))
   (check-equal? ((literal-chunk '(asdf jkl)) test-context) (nekot 'literal "asdfjkl" test-context))))

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
   (check-equal? ((spaces-chunk 4 3) test-context) (nekot 'spaces 7 test-context))
   (check-equal? ((spaces-chunk '(4 3)) test-context) (nekot 'spaces 7 test-context))))

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

;meta-nekot-building chunks

(define/provide-test-suite test-empty-chunk
  (test-case
   "Test empty-chunk"
   (check-equal? (empty-chunk (construct-context 80)) (nekot 'empty null (construct-context 80)))
   (check-equal? (empty-chunk (construct-context 3)) (nekot 'empty null (construct-context 3)))))

(define/provide-test-suite test-concat-chunk
  (test-case
   "Test concat-chunk"
   (check-equal? ((concat-chunk (literal-chunk 'asdf) (literal-chunk "jkl")) (construct-context 80))
                 (nekot 'concat (list (nekot 'literal "asdf" (construct-context 80))
                                      (nekot 'literal "jkl" (construct-context 80)))
                        (construct-context 80)))
   (check-equal? ((concat-chunk (list (literal-chunk 'asdf) (literal-chunk "jkl"))) (construct-context 80))
                 (nekot 'concat (list (nekot 'literal "asdf" (construct-context 80))
                                      (nekot 'literal "jkl" (construct-context 80)))
                        (construct-context 80)))
   (check-equal? ((concat-chunk (literal-chunk 'asdf) (spaces-chunk 4) (literal-chunk "jkl")) (construct-context 80))
                 (nekot 'concat (list (nekot 'literal "asdf" (construct-context 80))
                                      (nekot 'spaces 4 (construct-context 80))
                                      (nekot 'literal "jkl" (construct-context 80)))
                        (construct-context 80)))))

(define/provide-test-suite test-immediate-chunk
  (test-case
   "Test immediate-chunk"
   (define test-context (construct-context 80))
   (check-equal? ((immediate-chunk (literal-chunk 'asdf)) test-context)
                 (nekot 'immediate
                        (nekot 'literal "asdf" test-context)
                        test-context))
   (check-equal? ((immediate-chunk (spaces-chunk 4)) test-context)
                 (nekot 'immediate
                        (nekot 'spaces 4 test-context)
                        test-context))))

(define/provide-test-suite test-speculative-chunk
  (test-case
   "Test speculative-chunk"
   (define test-context (construct-context 80))
   (define test-success? (λ (any) #false))
   (define check-length (λ (lst) (= 1 (length lst))))
   (check-equal? ((speculative-chunk (literal-chunk 'asdf)
                                     test-success?
                                     (literal-chunk 'jkl)) test-context)
                 (nekot 'speculative
                        (list (nekot 'literal "asdf" test-context)
                              test-success?
                              (nekot 'literal "jkl" test-context))
                        test-context))
   (check-equal? ((speculative-chunk new-line-chunk
                                     check-length
                                     (spaces-chunk 4)) test-context)
                 (nekot 'speculative
                        (list (nekot 'new-line null test-context)
                              check-length
                              (nekot 'spaces 4 test-context))
                        test-context))))

(define/provide-test-suite test-position-indent-chunk
  (test-case
   "Test position-indent-chunk"
   (define test-context (construct-context 80))
   (define test-chunk (literal-chunk 'asdf))
   (define test-chunk-2 (literal-chunk 'jkl))
   (check-equal? ((position-indent-chunk test-chunk)
                  test-context)
                 (nekot 'position-indent
                        test-chunk
                        test-context))
   (check-equal? ((position-indent-chunk test-chunk-2)
                  test-context)
                 (nekot 'position-indent
                        test-chunk-2
                        test-context))))

;context-aware chunks

(define/provide-test-suite test-indent-chunk
  (test-case
   "Test indent-chunk"
   (define test-context (construct-context 80))
   (define test-context-2 (context 2 80 #false))
   (define test-context-3 (context 3 80 #false))
   (define test-context-4 (context 5 80 #false))
   (check-equal? ((indent-chunk 0 empty-chunk) test-context)
                 (nekot 'empty null test-context))
   (check-equal? ((indent-chunk 2 empty-chunk) test-context)
                 (nekot 'empty null test-context-2))
   (check-equal? ((indent-chunk 3 empty-chunk) test-context)
                 (nekot 'empty null test-context-3))
   (check-equal? ((indent-chunk 5 empty-chunk) test-context)
                 (nekot 'empty null test-context-4))
   (check-equal? ((indent-chunk (list 0 1 0 1) empty-chunk) test-context)
                 (nekot 'empty null test-context-2))
   (check-equal? ((indent-chunk (list 1 2) empty-chunk) test-context)
                 (nekot 'empty null test-context-3))
   (check-equal? ((indent-chunk (list 2 1 2) empty-chunk) test-context)
                 (nekot 'empty null test-context-4))
   (check-equal? ((indent-chunk 2 (indent-chunk 1 empty-chunk)) test-context)
                 (nekot 'empty null test-context-3))
   (check-equal? ((indent-chunk 3 (indent-chunk 2 empty-chunk)) test-context)
                 (nekot 'empty null test-context-4))))

(define/provide-test-suite test-comment-env-chunk
  (test-case
   "Test comment-env-chunk"
   (define test-context (construct-context 80))
   (define com-context (context 0 80 (comment-env 0)))
   (define test-context-2 (context 2 80 #false))
   (define com-context-2 (context 2 80 (comment-env 2)))
   (define com-context-3 (context 2 80 (comment-env 0)))
   (check-equal? ((comment-env-chunk empty-chunk) test-context)
                 (nekot 'empty null com-context))
   (check-equal? ((comment-env-chunk empty-chunk) test-context-2)
                 (nekot 'empty null com-context-2))
   (check-equal? ((comment-env-chunk (indent-chunk 2 (comment-env-chunk empty-chunk))) test-context)
                 (nekot 'empty null com-context-3))))

(define/provide-test-suite test-comment-line-chunk
  (test-case
   "Test comment-line-chunk - empty environment"
   (define test-context (construct-context 80))
   (define com-context (context 0 80 (comment-env 0)))
   (check-equal? ((comment-line-chunk (literal-chunk "asdf")) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "//" test-context)
                                     (nekot 'literal "asdf" test-context))
                               test-context)
                        test-context))
   (check-equal? ((comment-line-chunk (literal-chunk 'asdf)) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "//" test-context)
                                     (nekot 'literal "asdf" test-context))
                               test-context)
                        test-context))
   (check-equal? ((comment-line-chunk (literal-chunk 'as) (literal-chunk 'df)) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "//" test-context)
                                     (nekot 'literal "as" test-context)
                                     (nekot 'literal "df" test-context))
                               test-context)
                        test-context))
   (check-equal? ((comment-line-chunk (literal-chunk "as") (literal-chunk "df")) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "//" test-context)
                                     (nekot 'literal "as" test-context)
                                     (nekot 'literal "df" test-context))
                               test-context)
                        test-context))
   (check-equal? ((comment-line-chunk (literal-chunk 'as) (literal-chunk "df")) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "//" test-context)
                                     (nekot 'literal "as" test-context)
                                     (nekot 'literal "df" test-context))
                               test-context)
                        test-context))
   (check-equal? ((comment-line-chunk (literal-chunk "as") (literal-chunk 'df)) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "//" test-context)
                                     (nekot 'literal "as" test-context)
                                     (nekot 'literal "df" test-context))
                               test-context)
                        test-context)))
  (test-case
   "Test comment-line-chunk - comment environment"
   (define test-context (context 0 80 (comment-env 0)))
   (check-equal? ((comment-line-chunk (literal-chunk "asdf")) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "//" test-context)
                                     (nekot 'literal "asdf" test-context))
                               test-context)
                        test-context)))
  (test-case
   "Test comment-line-chunk - macro environment"
   (define test-context (context 0 80 macro-env))
   (check-equal? ((comment-line-chunk (literal-chunk "asdf")) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "/*" test-context)
                                     (nekot 'literal "asdf" test-context)
                                     (nekot 'literal "*/" test-context))
                               test-context)
                        test-context)))
  (test-case
   "Test comment-line-chunk - comment macro environment"
   (define test-context (context 0 80 (comment-macro-env 0)))
   (check-equal? ((comment-line-chunk (literal-chunk "asdf")) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "//" test-context)
                                     (nekot 'literal "asdf" test-context))
                               test-context)
                        test-context)))
  (test-case
   "Test comment-line-chunk - macro comment environment"
   (define test-context (context 0 80 (macro-comment-env 0)))
   (check-equal? ((comment-line-chunk (literal-chunk "asdf")) test-context)
                 (nekot 'immediate
                        (nekot 'concat
                               (list (nekot 'literal "//" test-context)
                                     (nekot 'literal "asdf" test-context))
                               test-context)
                        test-context))))

(define/provide-test-suite test-macro-env-chunk
  (test-case
   "Test macro-env-chunk"
   (define test-context (construct-context 80))
   (define mac-context (context 0 80 macro-env))
   (check-equal? ((macro-env-chunk (literal-chunk "asdf")) test-context)
                 (nekot 'literal "asdf" mac-context))
   (check-exn exn:fail? (λ () ((macro-env-chunk (literal-chunk "asdf")) mac-context)))))
