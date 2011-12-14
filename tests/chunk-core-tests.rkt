#lang racket

(require rackunit)
(require "../src/fulmar-core.rkt")
(require "../src/chunk-core.rkt")
(require "../src/writer.rkt")

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
   (check-equal? (write-nekot ((literal-chunk "asdf") test-context))
                 '("asdf"))
   (check-equal? (write-nekot ((literal-chunk 'asdf) test-context))
                 '("asdf"))
   (check-equal? (write-nekot ((literal-chunk "asdf" "jkl") test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot ((literal-chunk "asdf" 'jkl) test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot ((literal-chunk 'asdf "jkl") test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot ((literal-chunk 'asdf 'jkl) test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot ((literal-chunk '(asdf jkl)) test-context))
                 '("asdfjkl"))))

(define/provide-test-suite test-spaces-chunk
  (test-case
   "Test spaces-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((spaces-chunk "asdf") test-context))
                 '("    "))
   (check-equal? (write-nekot ((spaces-chunk 'asdf) test-context))
                 '("    "))
   (check-equal? (write-nekot ((spaces-chunk 4) test-context))
                 '("    "))
   (check-equal? (write-nekot ((spaces-chunk "asdf" "jkl") test-context))
                 '("       "))
   (check-equal? (write-nekot ((spaces-chunk "asdf" 'jkl) test-context))
                 '("       "))
   (check-equal? (write-nekot ((spaces-chunk "asdf" 3) test-context))
                 '("       "))
   (check-equal? (write-nekot ((spaces-chunk 'asdf "jkl") test-context))
                 '("       "))
   (check-equal? (write-nekot ((spaces-chunk 'asdf 'jkl) test-context))
                 '("       "))
   (check-equal? (write-nekot ((spaces-chunk 'asdf 3) test-context))
                 '("       "))
   (check-equal? (write-nekot ((spaces-chunk 4 "jkl") test-context))
                 '("       "))
   (check-equal? (write-nekot ((spaces-chunk 4 'jkl) test-context))
                 '("       "))
   (check-equal? (write-nekot ((spaces-chunk 4 3) test-context))
                 '("       "))
   (check-equal? (write-nekot ((spaces-chunk '(4 3)) test-context))
                 '("       "))))

(define/provide-test-suite test-new-line-chunk
  (test-case
   "Test new-line-chunk"
   (check-equal? (write-nekot (new-line-chunk (construct-context 80)))
                 '("" ""))
   (check-equal? (write-nekot (new-line-chunk (construct-context 3)))
                 '("" ""))))

(define/provide-test-suite test-pp-directive-chunk
  (test-case
   "Test pp-directive-chunk"
   (check-equal? (write-nekot (pp-directive-chunk (construct-context 80)))
                 '("#"))
   (check-equal? (write-nekot 'normal
                              (pp-directive-chunk (construct-context 3))
                              "")
                 '("#"))
   (check-equal? (write-nekot 'normal
                              (pp-directive-chunk (construct-context 3))
                              "   ")
                 '("#  "))
   (check-equal? (write-nekot 'normal
                              (pp-directive-chunk (construct-context 3))
                              "   /* ")
                 '("   /*#"))))

;meta-nekot-building chunks

(define/provide-test-suite test-empty-chunk
  (test-case
   "Test empty-chunk"
   (check-equal? (write-nekot (empty-chunk (construct-context 80)))
                 '(""))
   (check-equal? (write-nekot 'normal
                              (empty-chunk (construct-context 3))
                              "asdf")
                 '("asdf"))))

(define/provide-test-suite test-concat-chunk
  (test-case
   "Test concat-chunk"
   (check-equal? (write-nekot ((concat-chunk (literal-chunk 'asdf)
                                             (literal-chunk "jkl"))
                               (construct-context 80)))
                 '("asdfjkl"))
   (check-equal? (write-nekot ((concat-chunk (list (literal-chunk 'asdf)
                                                   (literal-chunk "jkl")))
                               (construct-context 80)))
                 '("asdfjkl"))
   (check-equal? (write-nekot ((concat-chunk (literal-chunk 'asdf)
                                             (spaces-chunk 4)
                                             (literal-chunk "jkl"))
                               (construct-context 80)))
                 '("asdf    jkl"))))

(define/provide-test-suite test-immediate-chunk
  (test-case
   "Test immediate-chunk"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 4))
   (check-equal? (write-nekot ((immediate-chunk (literal-chunk 'asdf)) test-context))
                 '("asdf"))
   (check-equal? (write-nekot ((immediate-chunk (spaces-chunk 4)) test-context))
                 '("    "))
   (check-equal? (write-nekot ((immediate-chunk (concat-chunk (spaces-chunk 4)
                                                              (literal-chunk 'asdf))) test-context-2))
                 '("    asdf"))))

(define/provide-test-suite test-speculative-chunk
  (test-case
   "Test speculative-chunk"
   (define test-context (construct-context 80))
   (define test-success? (λ (any) #false))
   (define check-length (λ (lst) (= 1 (length lst))))
   (check-equal? (write-nekot ((speculative-chunk (literal-chunk 'asdf)
                                                  test-success?
                                                  (literal-chunk 'jkl))
                               test-context))
                 '("jkl"))
   (check-equal? (write-nekot ((speculative-chunk new-line-chunk
                                                  check-length
                                                  (spaces-chunk 4))
                               test-context))
                 '("    "))))

(define/provide-test-suite test-position-indent-chunk
  (test-case
   "Test position-indent-chunk"
   (define test-context (construct-context 80))
   (define test-chunk (literal-chunk 'asdf))
   (define test-chunk-2 (literal-chunk 'jkl))
   (check-equal? (write-nekot ((position-indent-chunk (literal-chunk 'asdf)) test-context))
                 '("asdf"))
   (check-equal? (write-nekot ((concat-chunk (literal-chunk 'asdf)
                                             (position-indent-chunk (concat-chunk new-line-chunk
                                                                                  (literal-chunk 'jkl))))
                               test-context))
                 '("    jkl"
                   "asdf"))))

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
   (define test-context (construct-context 20))
   (define com-context (context 0 80 (comment-env 0)))
   (check-equal? (write-nekot ((comment-env-chunk (literal-chunk 'asdf) #\*) test-context))
                 '("/**asdf*/"))
   (check-equal? (write-nekot ((comment-env-chunk (literal-chunk 'asdf) #\ ) test-context))
                 '("/* asdf*/"))
   (check-equal? (write-nekot ((comment-env-chunk (literal-chunk 'asdf)) test-context))
                 '("/* asdf*/"))
   (check-equal? (write-nekot ((comment-env-chunk (literal-chunk 'asdf)) com-context))
                 '("/* // asdf"))
   (check-equal? (write-nekot ((comment-env-chunk (indent-chunk 2 (comment-env-chunk (literal-chunk 'asdf)))) test-context))
                 '("/* // asdf*/"))
   (check-equal? (write-nekot ((comment-env-chunk (concat-chunk new-line-chunk
                                                                (indent-chunk 2 (comment-env-chunk (literal-chunk 'asdf)))))
                                                  test-context))
                 '("/*   // asdf*/"
                   ""))
   (check-equal? (write-nekot ((comment-env-chunk (concat-chunk (literal-chunk 'asdf)
                                                                new-line-chunk
                                                                (literal-chunk 'jkl)))
                                                  test-context))
                 '("/* jkl*/"
                   "/* asdf           */"))))

(define/provide-test-suite test-comment-line-chunk
  (test-case
   "Test comment-line-chunk - empty environment"
   (define test-context (construct-context 80))
   (define com-context (context 0 80 (comment-env 0)))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk "asdf")) test-context))
                 '("//asdf"))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk 'asdf)) test-context))
                 '("//asdf"))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk 'as) (literal-chunk 'df)) test-context))
                 '("//asdf"))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk "as") (literal-chunk "df")) test-context))
                 '("//asdf"))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk 'as) (literal-chunk "df")) test-context))
                 '("//asdf"))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk "as") (literal-chunk 'df)) test-context))
                 '("//asdf")))
  (test-case
   "Test comment-line-chunk - comment environment"
   (define test-context (context 0 80 (comment-env 0)))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk "asdf")) test-context))
                 '("/* //asdf")))
  (test-case
   "Test comment-line-chunk - macro environment"
   (define test-context (context 0 80 macro-env))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk "asdf")) test-context))
                 '("/*asdf*/")))
  (test-case
   "Test comment-line-chunk - comment macro environment"
   (define test-context (context 0 80 (comment-macro-env 0)))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk "asdf")) test-context))
                 '("/* //asdf")))
  (test-case
   "Test comment-line-chunk - macro comment environment"
   (define test-context (context 0 80 (macro-comment-env 0)))
   (check-equal? (write-nekot ((comment-line-chunk (literal-chunk "asdf")) test-context))
                 '("/* //asdf"))))

(define/provide-test-suite test-macro-env-chunk
  (test-case
   "Test macro-env-chunk"
   (define test-context (construct-context 8))
   (define mac-context (context 0 80 macro-env))
   (check-equal? (write-nekot ((macro-env-chunk (concat-chunk (literal-chunk "asdf")
                                                              new-line-chunk
                                                              (literal-chunk 'jkl))) test-context))
                 '("jkl"
                   "asdf   \\"))
   (check-exn exn:fail? (λ () ((macro-env-chunk (literal-chunk "asdf")) mac-context)))))
