#lang racket

(require rackunit)
(require "../private/fulmar-core.rkt")
(require "../private/core-chunk.rkt")
(require "../private/writer.rkt")

;unit tests for core-chunk.rkt

;helper functions

(define/provide-test-suite test-combine-lengths
  (test-case
   "Test combine-lengths"
   (check-eq? (combine-lengths 4) 4)
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

(define/provide-test-suite test-literal
  (test-case
   "Test literal"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (literal "asdf") test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (literal 'asdf) test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (literal "asdf" "jkl") test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (literal "asdf" 'jkl) test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (literal 'asdf "jkl") test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (literal 'asdf 'jkl) test-context))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (literal '(asdf jkl)) test-context))
                 '("asdfjkl"))))

(define/provide-test-suite test-spaces
  (test-case
   "Test spaces"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (spaces 4) test-context))
                 '("    "))
   (check-equal? (write-nekot (chunk-transform (spaces 4 3) test-context))
                 '("       "))
   (check-equal? (write-nekot (chunk-transform (spaces '(4 3)) test-context))
                 '("       "))))

(define/provide-test-suite test-new-line
  (test-case
   "Test new-line"
   (check-equal? (write-nekot (chunk-transform new-line (construct-context 80)))
                 '("" ""))
   (check-equal? (write-nekot (chunk-transform new-line (construct-context 3)))
                 '("" ""))))

(define/provide-test-suite test-pp-directive
  (test-case
   "Test pp-directive"
   (check-equal? (write-nekot (chunk-transform pp-directive (construct-context 80)))
                 '("#"))
   (check-equal? (write-nekot 'normal
                              (chunk-transform pp-directive (construct-context 3))
                              "")
                 '("#"))
   (check-equal? (write-nekot 'normal
                              (chunk-transform pp-directive (construct-context 3))
                              "   ")
                 '("#  "))
   (check-equal? (write-nekot 'normal
                              (chunk-transform pp-directive (construct-context 3))
                              "   /* ")
                 '("   /*#"))))

;meta-nekot-building chunks

(define/provide-test-suite test-empty
  (test-case
   "Test empty"
   (check-equal? (write-nekot (chunk-transform empty (construct-context 80)))
                 '(""))
   (check-equal? (write-nekot 'normal
                              (chunk-transform empty (construct-context 3))
                              "asdf")
                 '("asdf"))))

(define/provide-test-suite test-concat
  (test-case
   "Test concat"
   (check-equal? (write-nekot (chunk-transform (concat 'asdf 'jkl)
                                               (construct-context 80)))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (concat (list 'asdf 'jkl))
                                               (construct-context 80)))
                 '("asdfjkl"))
   (check-equal? (write-nekot (chunk-transform (concat 'asdf 4 'jkl)
                                               (construct-context 80)))
                 '("asdf    jkl"))))

(define/provide-test-suite test-immediate
  (test-case
   "Test immediate"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 4))
   (check-equal? (write-nekot (chunk-transform (immediate 'asdf)
                                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (immediate 4)
                                               test-context))
                 '("    "))
   (check-equal? (write-nekot (chunk-transform (immediate (concat 4 'asdf))
                                               test-context-2))
                 '("    asdf"))))

(define/provide-test-suite test-speculative
  (test-case
   "Test speculative"
   (define test-context (construct-context 80))
   (define test-success? (λ (any) #false))
   (define check-length (λ (lst) (= 1 (length lst))))
   (check-equal? (write-nekot (chunk-transform (speculative 'asdf test-success? 'jkl)
                                               test-context))
                 '("jkl"))
   (check-equal? (write-nekot (chunk-transform (speculative new-line check-length 4)
                                               test-context))
                 '("    "))))

(define/provide-test-suite test-position-indent
  (test-case
   "Test position-indent"
   (define test-context (construct-context 80))
   (define test 'asdf)
   (define test-2 'jkl)
   (check-equal? (write-nekot (chunk-transform (position-indent 'asdf)
                                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (concat 'asdf (position-indent (concat new-line 'jkl)))
                                               test-context))
                 '("    jkl"
                   "asdf"))))

(define/provide-test-suite test-modify-context
  (test-case
   "Test modify-context"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (modify-context 'asdf enter-comment-env)
                                               test-context))
                 '("/* asdf"))))

;context-aware chunks

(define/provide-test-suite test-indent
  (test-case
   "Test indent"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (indent 0
                                                       (concat 'asdf new-line 'jkl))
                                               test-context))
                 '("jkl"
                   "asdf"))
   (check-equal? (write-nekot (chunk-transform (indent 2 (concat 'asdf new-line 'jkl))
                                               test-context))
                 '("  jkl"
                   "  asdf"))
   (check-equal? (write-nekot (chunk-transform (indent 3 (concat 'asdf new-line 'jkl))
                                               test-context))
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent 5 (concat 'asdf new-line 'jkl))
                                               test-context))
                 '("     jkl"
                   "     asdf"))
   (check-equal? (write-nekot (chunk-transform (indent (list 0 1 0 1)
                                                       (concat 'asdf new-line 'jkl))
                                               test-context))
                 '("  jkl"
                   "  asdf"))
   (check-equal? (write-nekot (chunk-transform (indent (list 1 2)
                                                       (concat 'asdf new-line 'jkl))
                                               test-context))
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent (list 2 1 2)
                                                       (concat 'asdf new-line 'jkl))
                                               test-context))
                 '("     jkl"
                   "     asdf"))
   (check-equal? (write-nekot (chunk-transform (indent 2 (indent 1 (concat 'asdf new-line 'jkl)))
                                               test-context))
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent 3 (indent 2 (concat 'asdf new-line 'jkl)))
                                               test-context))
                 '("     jkl"
                   "     asdf")))
  (test-case
   "Test indent - interaction with position-indent tests"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (indent 3 (position-indent (concat 'asdf new-line 'jkl)))
                                               test-context))
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent 3 (concat 'asdf (position-indent (concat new-line 'jkl))))
                                               test-context))
                 '("       jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent 3 (concat 'asdf (position-indent (concat new-line 'jkl new-line "123"))))
                                               test-context))
                 '("       123"
                   "       jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent 3 (concat 'asdf (position-indent (concat new-line 'jkl new-line (indent 1 "123")))))
                                               test-context))
                 '("        123"
                   "       jkl"
                   "   asdf"))
   (check-equal? (write-nekot (chunk-transform (indent 3 (concat 'asdf (position-indent (concat new-line
                                                                                                (indent 1 'jkl)
                                                                                                new-line
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
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk (indent 2 (comment-env-chunk 'asdf))) test-context))
                 '("/* // asdf */"))
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk (concat new-line (indent 2 (comment-env-chunk 'asdf))))
                                               test-context))
                 '("/*   // asdf */"
                   ""))
   (check-equal? (write-nekot (chunk-transform (comment-env-chunk (concat 'asdf new-line 'jkl))
                                               test-context))
                 '("/* jkl */"
                   "/* asdf */"))))

(define/provide-test-suite test-macro-env-chunk
  (test-case
   "Test macro-env-chunk"
   (define test-context (construct-context 8))
   (define mac-context (context 0 80 macro-env))
   (check-equal? (write-nekot (chunk-transform (macro-env-chunk (concat 'asdf new-line 'jkl))
                                               test-context))
                 '("jkl"
                   "asdf   \\"))
   (check-exn exn:fail? (λ () (chunk-transform (macro-env-chunk 'asdf) mac-context)))))
