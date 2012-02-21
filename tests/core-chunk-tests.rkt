#lang racket

(require rackunit)
(require "../src/fulmar-core.rkt")
(require "../src/core-chunk.rkt")
(require "../src/writer.rkt")

;unit tests for core-chunk.rkt

;helper functions

(define/provide-test-suite test-combine-lengths
  (test-case
   "Test combine-lengths"
   (check-eq? (combine-lengths "asdf") 4)
   (check-eq? (combine-lengths 'asdf) 4)
   (check-eq? (combine-lengths 4) 4)
   (check-eq? (combine-lengths "asdf" "jkl") 7)
   (check-eq? (combine-lengths '("asdf" "jkl")) 7)
   (check-eq? (combine-lengths '(("asdf") "jkl")) 7)
   (check-eq? (combine-lengths '("asdf" ("jkl"))) 7)
   (check-eq? (combine-lengths '(("asdf") ("jkl"))) 7)
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
   (check-equal? (combine-strings '("asdf" "jkl")) "asdfjkl")
   (check-equal? (combine-strings '(("asdf") "jkl")) "asdfjkl")
   (check-equal? (combine-strings '("asdf" ("jkl"))) "asdfjkl")
   (check-equal? (combine-strings '(("asdf") ("jkl"))) "asdfjkl")
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

(define/provide-test-suite test-chunk-transform
  (test-case
   "Test chunk-transform - tests only implicit chunks (not structured chunks)"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform "asdf" test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform 'asdf test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform 0 test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform 4 test-context))
                 '("    "))))

;nekot-building chunks

(define/provide-test-suite test-literal-chunk
  (test-case
   "Test literal-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (literal-chunk "asdf") test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (literal-chunk 'asdf) test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (literal-chunk "asdf" "jkl") test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (literal-chunk "asdf" 'jkl) test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (literal-chunk 'asdf "jkl") test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (literal-chunk 'asdf 'jkl) test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (literal-chunk '(asdf jkl)) test-context))
                 '("asdfjkl"))))

(define/provide-test-suite test-spaces-chunk
  (test-case
   "Test spaces-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk "asdf") test-context))
                 '("    "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk 'asdf) test-context))
                 '("    "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk 4) test-context))
                 '("    "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk "asdf" "jkl") test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk "asdf" 'jkl) test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk "asdf" 3) test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk 'asdf "jkl") test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk 'asdf 'jkl) test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk 'asdf 3) test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk 4 "jkl") test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk 4 'jkl) test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk 4 3) test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces-chunk '(4 3)) test-context))
                 '("       "))))

(define/provide-test-suite test-new-line-chunk
  (test-case
   "Test new-line-chunk"
   (check-equal? (write-nekot (chunk-transform new-line-chunk (construct-context 80)))
                 '("" ""))
   (check-equal? (write-nekot (chunk-transform new-line-chunk (construct-context 3)))
                 '("" ""))))

(define/provide-test-suite test-pp-directive-chunk
  (test-case
   "Test pp-directive-chunk"
   (check-equal? (write-nekot (chunk-transform pp-directive-chunk (construct-context 80)))
                 '("#"))
   (check-equal? (write-nekot 'normal
                              (chunk-transform pp-directive-chunk (construct-context 3))
                              "")
                 '("#"))
   (check-equal? (write-nekot 'normal
                              (chunk-transform pp-directive-chunk (construct-context 3))
                              "   ")
                 '("#  "))
   (check-equal? (write-nekot 'normal
                              (chunk-transform pp-directive-chunk (construct-context 3))
                              "   /* ")
                 '("   /*#"))))

;meta-nekot-building chunks

(define/provide-test-suite test-empty-chunk
  (test-case
   "Test empty-chunk"
   (check-equal? (write-nekot (chunk-transform empty-chunk (construct-context 80)))
                 '(""))
   (check-equal? (write-nekot 'normal
                              (chunk-transform empty-chunk (construct-context 3))
                              "asdf")
                 '("asdf"))))

(define/provide-test-suite test-concat-chunk
  (test-case
   "Test concat-chunk"
   (check-equal? (write-nekot (chunk-transform (concat-chunk 'asdf
                                                             'jkl)
                                               (construct-context 80)))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (list 'asdf
                                                                   'jkl))
                                               (construct-context 80)))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk 'asdf
                                                             4
                                                             'jkl)
                                               (construct-context 80)))
                 '("asdf    jkl"))))

(define/provide-test-suite test-immediate-chunk
  (test-case
   "Test immediate-chunk"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 4))
   (check-equal? (write-nekot (chunk-transform (immediate-chunk 'asdf) test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (immediate-chunk 4) test-context))
                 '("    "))
   (check-equal? (write-nekot (chunk-transform (immediate-chunk (concat-chunk 4
                                                                              'asdf)) test-context-2))
                 '("    asdf"))))

(define/provide-test-suite test-speculative-chunk
  (test-case
   "Test speculative-chunk"
   (define test-context (construct-context 80))
   (define test-success? (λ (any) #false))
   (define check-length (λ (lst) (= 1 (length lst))))
   (check-equal? (write-nekot (chunk-transform (speculative-chunk 'asdf
                                                  test-success?
                                                  'jkl)
                               test-context))
                 '("jkl"))
   (check-equal? (write-nekot (chunk-transform (speculative-chunk new-line-chunk
                                                  check-length
                                                  4)
                               test-context))
                 '("    "))))

(define/provide-test-suite test-position-indent-chunk
  (test-case
   "Test position-indent-chunk"
   (define test-context (construct-context 80))
   (define test-chunk 'asdf)
   (define test-chunk-2 'jkl)
   (check-equal? (write-nekot (chunk-transform (position-indent-chunk 'asdf) test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk 'asdf
                                                             (position-indent-chunk (concat-chunk new-line-chunk
                                                                                                  'jkl)))
                                               test-context))
                 '("    jkl"
                   "asdf"))))

(define/provide-test-suite test-modify-context-chunk
  (test-case
   "Test modify-context-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (modify-context-chunk 'asdf
                                                                     enter-comment-env) test-context))
                 '("/* asdf"))))

;context-aware chunks

(define/provide-test-suite test-indent-chunk
  (test-case
   "Test indent-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 0 (concat-chunk 'asdf
                                                             new-line-chunk
                                                             'jkl))
                                               test-context))
                 '("jkl"
                   "asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 2 (concat-chunk 'asdf
                                                             new-line-chunk
                                                             'jkl))
                               test-context))
                 '("  jkl"
                   "  asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 3 (concat-chunk 'asdf
                                                             new-line-chunk
                                                             'jkl))
                               test-context))
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 5 (concat-chunk 'asdf
                                                             new-line-chunk
                                                             'jkl))
                               test-context))
                 '("     jkl"
                   "     asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk (list 0 1 0 1) (concat-chunk 'asdf
                                                                          new-line-chunk
                                                                          'jkl))
                               test-context))
                 '("  jkl"
                   "  asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk (list 1 2) (concat-chunk 'asdf
                                                                      new-line-chunk
                                                                      'jkl))
                               test-context))
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk (list 2 1 2) (concat-chunk 'asdf
                                                                        new-line-chunk
                                                                        'jkl))
                               test-context))
                 '("     jkl"
                   "     asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 2 (indent-chunk 1 (concat-chunk 'asdf
                                                                             new-line-chunk
                                                                             'jkl)))
                               test-context))
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 3 (indent-chunk 2 (concat-chunk 'asdf
                                                                             new-line-chunk
                                                                             'jkl)))
                               test-context))
                 '("     jkl"
                   "     asdf")))
  (test-case
   "Test indent-chunk - interaction with position-indent-chunk tests"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 3 (position-indent-chunk (concat-chunk 'asdf
                                                                                    new-line-chunk
                                                                                    'jkl)))
                               test-context))
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 3 (concat-chunk 'asdf
                                                             (position-indent-chunk (concat-chunk new-line-chunk
                                                                                                  'jkl))))
                               test-context))
                 '("       jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 3 (concat-chunk 'asdf
                                                                             (position-indent-chunk (concat-chunk new-line-chunk
                                                                                                                  'jkl
                                                                                                                  new-line-chunk
                                                                                                                  "123"))))
                                               test-context))
                 '("       123"
                   "       jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 3 (concat-chunk 'asdf
                                                                             (position-indent-chunk (concat-chunk new-line-chunk
                                                                                                                  'jkl
                                                                                                                  new-line-chunk
                                                                                                                  (indent-chunk 1 "123")))))
                                               test-context))
                 '("        123"
                   "       jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent-chunk 3 (concat-chunk 'asdf
                                                                             (position-indent-chunk (concat-chunk new-line-chunk
                                                                                                                  (indent-chunk 1 'jkl)
                                                                                                                  new-line-chunk
                                                                                                                  "123"))))
                                               test-context))
                 '("       123"
                   "        jkl"
                   "   asdf"))))

(define/provide-test-suite test-comment-env-chunk
  (test-case
   "Test comment-env-chunk"
   (define test-context (construct-context 20))
   (define com-context (context 0 80 (comment-env 0)))
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk 'asdf #\*) test-context))
                 '("/**asdf */"))
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk 'asdf #\ ) test-context))
                 '("/* asdf */"))
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk 'asdf) test-context))
                 '("/* asdf */"))
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk 'asdf) com-context))
                 '("/* // asdf"))
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk (indent-chunk 2 (comment-env-chunk 'asdf))) test-context))
                 '("/* // asdf */"))
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk (concat-chunk new-line-chunk
                                                                (indent-chunk 2 (comment-env-chunk 'asdf))))
                                                  test-context))
                 '("/*   // asdf */"
                   ""))
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk (concat-chunk 'asdf
                                                                new-line-chunk
                                                                'jkl))
                                                  test-context))
                 '("/* jkl */"
                   "/* asdf */"))))

(define/provide-test-suite test-macro-env-chunk
  (test-case
   "Test macro-env-chunk"
   (define test-context (construct-context 8))
   (define mac-context (context 0 80 macro-env))
   (check-equal? (write-nekot (chunk-transform (macro-env-chunk (concat-chunk 'asdf
                                                              new-line-chunk
                                                              'jkl)) test-context))
                 '("jkl"
                   "asdf   \\"))
   (check-exn exn:fail? (λ () (chunk-transform (macro-env-chunk 'asdf) mac-context)))))
