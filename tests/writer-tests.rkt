#lang racket

(require rackunit)
(require "../fulmar-core.rkt")
(require "../writer.rkt")

;unit tests for writer.rkt

;helper tests

(define/provide-test-suite test-is-whitespace?
  (test-case
   "Test is-whitespace? (checks if string is all whitespace)"
   (check-true (is-whitespace? ""))
   (check-true (is-whitespace? " "))
   (check-true (is-whitespace? "  "))
   (check-true (is-whitespace? "                  "))
   (check-false (is-whitespace? "a"))
   (check-false (is-whitespace? " a "))
   (check-false (is-whitespace? "a    "))
   (check-false (is-whitespace? "    a"))
   (check-false (is-whitespace? "asdf"))))

(define/provide-test-suite test-make-whitespace
  (test-case
   "Test make-whitespace (make string of spaces)"
   (check-equal? (make-whitespace 0) "")
   (check-equal? (make-whitespace 1) " ")
   (check-equal? (make-whitespace 2) "  ")
   (check-equal? (make-whitespace 3) "   ")))

(define/provide-test-suite test-remove-whitespace
  (test-case
   "Test remove-whitespace (from end of line)"
   (check-equal? (remove-whitespace "") "")
   (check-equal? (remove-whitespace " ") "")
   (check-equal? (remove-whitespace "  ") "")
   (check-equal? (remove-whitespace "a") "a")
   (check-equal? (remove-whitespace " a") " a")
   (check-equal? (remove-whitespace "  a") "  a")
   (check-equal? (remove-whitespace "a ") "a")
   (check-equal? (remove-whitespace " a ") " a")
   (check-equal? (remove-whitespace " a      ") " a")
   (check-equal? (remove-whitespace " a b ") " a b")
   (check-equal? (remove-whitespace " a  b  c  ") " a  b  c")))

(define/provide-test-suite test-build-indentation
  (test-case
   "Test build-indentation (begins a new line correctly) - empty environment"
   (check-equal? (build-indentation (context 0 80 #false)) "")
   (check-equal? (build-indentation (context 4 80 #false)) "    ")
   (check-equal? (build-indentation (context 4 2 #false)) "    "))
  (test-case
   "Test build-indentation (begins a new line correctly) - comment environment"
   (check-equal? (build-indentation (context 0 80 (comment-env 0))) "/* ")
   (check-equal? (build-indentation (context 4 80 (comment-env 4))) "    /* ")
   (check-equal? (build-indentation (context 6 80 (comment-env 4))) "    /*   ")
   (check-equal? (build-indentation (context 2 80 (comment-env 4))) "    /* "))
  (test-case
   "Test build-indentation (begins a new line correctly) - macro environment"
   (check-equal? (build-indentation (context 3 80 macro-env)) "   "))
  (test-case
   "Test build-indentation (begins a new line correctly) - comment-macro environment"
   (check-equal? (build-indentation (context 4 80 (comment-macro-env 4))) "    /* ")
   (check-equal? (build-indentation (context 0 80 (comment-macro-env 0))) "/* ")
   (check-equal? (build-indentation (context 6 80 (comment-macro-env 4))) "    /*   ")
   (check-equal? (build-indentation (context 2 80 (comment-macro-env 4))) "    /* "))
  (test-case
   "Test build-indentation (begins a new line correctly) - macro-comment environment"
   (check-equal? (build-indentation (context 4 80 (macro-comment-env 4))) "    /* ")
   (check-equal? (build-indentation (context 0 80 (macro-comment-env 0))) "/* ")
   (check-equal? (build-indentation (context 6 80 (macro-comment-env 4))) "    /*   ")
   (check-equal? (build-indentation (context 2 80 (macro-comment-env 4))) "    /* ")))

(define/provide-test-suite test-finish-line
  (test-case
   "Test finish-line (ends a line correctly) - empty environment"
   (define test-context (context 0 6 #false))
   (check-equal? (finish-line "" test-context) "")
   (check-equal? (finish-line "  " test-context) "")
   (check-equal? (finish-line " 234" test-context) " 234")
   (check-equal? (finish-line " 234 " test-context) " 234")
   (check-equal? (finish-line " 234567"  test-context) " 234567")
   (check-equal? (finish-line " 234567 " test-context) " 234567")
   (check-equal? (finish-line " 234567  " test-context) " 234567"))
  (test-case
   "Test finish-line (ends a line correctly) - comment environment"
   (define test-context (context 0 6 (comment-env 0)))
   (check-equal? (finish-line "" test-context) "")
   (check-equal? (finish-line "  " test-context) "")
   (check-equal? (finish-line " 234" test-context) " 234  */")
   (check-equal? (finish-line " 234 " test-context) " 234  */")
   (check-equal? (finish-line " 234567" test-context) " 234567 */")
   (check-equal? (finish-line " 234567 " test-context) " 234567 */")
   (check-equal? (finish-line " 234567  " test-context) " 234567 */"))
  (test-case
   "Test finish-line (ends a line correctly) - macro environment"
   (define test-context (context 0 6 macro-env))
   (check-equal? (finish-line "" test-context) "      \\")
   (check-equal? (finish-line "  " test-context) "      \\")
   (check-equal? (finish-line " 234" test-context) " 234  \\")
   (check-equal? (finish-line " 234 " test-context) " 234  \\")
   (check-equal? (finish-line " 234567" test-context) " 234567 \\")
   (check-equal? (finish-line " 234567 " test-context) " 234567 \\")
   (check-equal? (finish-line " 234567  " test-context) " 234567 \\"))
  (test-case
   "Test finish-line (ends a line correctly) - comment-macro environment"
   (define test-context (context 0 6 (comment-macro-env 0)))
   (check-equal? (finish-line "" test-context) "")
   (check-equal? (finish-line "  " test-context) "")
   (check-equal? (finish-line " 234" test-context) " 234  \\ */")
   (check-equal? (finish-line " 234 " test-context) " 234  \\ */")
   (check-equal? (finish-line " 234567" test-context) " 234567 \\ */")
   (check-equal? (finish-line " 234567 " test-context) " 234567 \\ */")
   (check-equal? (finish-line " 234567  " test-context) " 234567 \\ */"))
  (test-case
   "Test finish-line (ends a line correctly) - macro-comment environment"
   (define test-context (context 0 6 (macro-comment-env 0)))
   (check-equal? (finish-line "" test-context) "      \\")
   (check-equal? (finish-line "  " test-context) "      \\")
   (check-equal? (finish-line " 234" test-context) " 234  */ \\")
   (check-equal? (finish-line " 234567" test-context) " 234567 */ \\")
   (check-equal? (finish-line " 234567 " test-context) " 234567 */ \\")
   (check-equal? (finish-line " 234567  " test-context) " 234567 */ \\")))

(define/provide-test-suite test-check-speculative-line-length
  (test-case
   "Test check-speculative-line-length (check against context's line length - empty string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "" test-context))
   (check-true (check-speculative-line-length 1 "" test-context))
   (check-true (check-speculative-line-length 3 "" test-context))
   (check-true (check-speculative-line-length 5 "" test-context))
   (check-true (check-speculative-line-length 6 "" test-context))
   (check-false (check-speculative-line-length 7 "" test-context))
   (check-false (check-speculative-line-length 10 "" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - single-element string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "1" test-context))
   (check-true (check-speculative-line-length 1 "1" test-context))
   (check-true (check-speculative-line-length 3 "1" test-context))
   (check-true (check-speculative-line-length 5 "1" test-context))
   (check-false (check-speculative-line-length 6 "1" test-context))
   (check-false (check-speculative-line-length 7 "1" test-context))
   (check-false (check-speculative-line-length 10 "1" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - short string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "123" test-context))
   (check-true (check-speculative-line-length 1 "123" test-context))
   (check-true (check-speculative-line-length 3 "123" test-context))
   (check-false (check-speculative-line-length 5 "123" test-context))
   (check-false (check-speculative-line-length 6 "123" test-context))
   (check-false (check-speculative-line-length 7 "123" test-context))
   (check-false (check-speculative-line-length 10 "123" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - long string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "12345" test-context))
   (check-true (check-speculative-line-length 1 "12345" test-context))
   (check-false (check-speculative-line-length 3 "12345" test-context))
   (check-false (check-speculative-line-length 5 "12345" test-context))
   (check-false (check-speculative-line-length 6 "12345" test-context))
   (check-false (check-speculative-line-length 7 "12345" test-context))
   (check-false (check-speculative-line-length 10 "12345" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - max-length string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "123456" test-context))
   (check-false (check-speculative-line-length 1 "123456" test-context))
   (check-false (check-speculative-line-length 3 "123456" test-context))
   (check-false (check-speculative-line-length 5 "123456" test-context))
   (check-false (check-speculative-line-length 6 "123456" test-context))
   (check-false (check-speculative-line-length 7 "123456" test-context))
   (check-false (check-speculative-line-length 10 "123456" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - slightly too long string"
   (define test-context (context 0 6 #false))
   (check-false (check-speculative-line-length 0 "1234567" test-context))
   (check-false (check-speculative-line-length 1 "1234567" test-context))
   (check-false (check-speculative-line-length 3 "1234567" test-context))
   (check-false (check-speculative-line-length 5 "1234567" test-context))
   (check-false (check-speculative-line-length 6 "1234567" test-context))
   (check-false (check-speculative-line-length 7 "1234567" test-context))
   (check-false (check-speculative-line-length 10 "1234567" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - too long string"
   (define test-context (context 0 6 #false))
   (check-false (check-speculative-line-length 0 "1234567890" test-context))
   (check-false (check-speculative-line-length 1 "1234567890" test-context))
   (check-false (check-speculative-line-length 3 "1234567890" test-context))
   (check-false (check-speculative-line-length 5 "1234567890" test-context))
   (check-false (check-speculative-line-length 6 "1234567890" test-context))
   (check-false (check-speculative-line-length 7 "1234567890" test-context))
   (check-false (check-speculative-line-length 10 "1234567890" test-context))))

(define/provide-test-suite test-add-hash-character
  (test-case
   "Test add-hash-character"
   (check-equal? (add-hash-character "") "#")
   (check-equal? (add-hash-character " ") "#")
   (check-equal? (add-hash-character "  ") "# ")
   (check-equal? (add-hash-character " a ") " a#")
   (check-equal? (add-hash-character " a") " #")
   (check-equal? (add-hash-character "a ") "a#")
   (check-equal? (add-hash-character "  a  ") "  a #")
   (check-equal? (add-hash-character "  a") "  #")
   (check-equal? (add-hash-character "a  ") "a #")))

;handler tests

(define/provide-test-suite test-add-empty
  (test-case
   "Test add-empty (writes empty nekot)"
   (define test-context (context 0 6 #false))
   (check-equal? (add-empty null test-context "") '(""))
   (check-equal? (add-empty null test-context "a") '("a"))
   (check-equal? (add-empty null test-context "asdf") '("asdf"))
   (check-equal? (add-empty null test-context "asdfjkl") '("asdfjkl"))))

(define/provide-test-suite test-add-literal
  (test-case
   "Test add-literal (writes literal nekot) - empty line - no indent"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal "" test-context "") '(""))
   (check-equal? (add-literal "abc" test-context "") '("abc"))
   (check-equal? (add-literal "abcd" test-context "") '("abcd"))
   (check-equal? (add-literal "abcdefg" test-context "") '("abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - single-element line - no indent"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal "" test-context "123") '("123"))
   (check-equal? (add-literal "abc" test-context "123") '("123abc"))
   (check-equal? (add-literal "abcd" test-context "123") '("abcd" "123"))
   (check-equal? (add-literal "abcdefg" test-context "123") '("abcdefg" "123")))
  (test-case
   "Test add-literal (writes literal nekot) - short line - no indent"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal "" test-context "1234") '("1234"))
   (check-equal? (add-literal "abc" test-context "1234") '("abc" "1234"))
   (check-equal? (add-literal "abcd" test-context "1234") '("abcd" "1234"))
   (check-equal? (add-literal "abcdefg" test-context "1234") '("abcdefg" "1234")))
  (test-case
   "Test add-literal (writes literal nekot) - long line - no indent"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal "" test-context "1234567") '("1234567"))
   (check-equal? (add-literal "abc" test-context "1234567") '("abc" "1234567"))
   (check-equal? (add-literal "abcd" test-context "1234567") '("abcd" "1234567"))
   (check-equal? (add-literal "abcdefg" test-context "1234567") '("abcdefg" "1234567")))
  (test-case
   "Test add-literal (writes literal nekot) - empty line - small indent"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal "" test-context "") '(""))
   (check-equal? (add-literal "abc" test-context "") '("abc"))
   (check-equal? (add-literal "abcd" test-context "") '("abcd"))
   (check-equal? (add-literal "abcdefg" test-context "") '("abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - single-element line - small indent"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal "" test-context "123") '("123"))
   (check-equal? (add-literal "abc" test-context "123") '("123abc"))
   (check-equal? (add-literal "abcd" test-context "123") '("123abcd"))
   (check-equal? (add-literal "abcdefg" test-context "123") '("123abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - short line - small indent"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal "" test-context "1234") '("1234"))
   (check-equal? (add-literal "abc" test-context "1234") '("   abc" "1234"))
   (check-equal? (add-literal "abcd" test-context "1234") '("   abcd" "1234"))
   (check-equal? (add-literal "abcdefg" test-context "1234") '("   abcdefg" "1234")))
  (test-case
   "Test add-literal (writes literal nekot) - long line - small indent"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal "" test-context "1234567") '("1234567"))
   (check-equal? (add-literal "abc" test-context "1234567") '("   abc" "1234567"))
   (check-equal? (add-literal "abcd" test-context "1234567") '("   abcd" "1234567"))
   (check-equal? (add-literal "abcdefg" test-context "1234567") '("   abcdefg" "1234567")))
  (test-case
   "Test add-literal (writes literal nekot) - empty line - long indent"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal "" test-context "") '(""))
   (check-equal? (add-literal "abc" test-context "") '("abc"))
   (check-equal? (add-literal "abcd" test-context "") '("abcd"))
   (check-equal? (add-literal "abcdefg" test-context "") '("abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - single-element line - long indent"
   (define test-context (context 6 6 #false))
   (check-equal? (add-literal "" test-context "123") '("123"))
   (check-equal? (add-literal "abc" test-context "123") '("123abc"))
   (check-equal? (add-literal "abcd" test-context "123") '("123abcd"))
   (check-equal? (add-literal "abcdefg" test-context "123") '("123abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - short line - long indent"
   (define test-context (context 6 6 #false))
   (check-equal? (add-literal "" test-context "1234") '("1234"))
   (check-equal? (add-literal "abc" test-context "1234") '("1234abc"))
   (check-equal? (add-literal "abcd" test-context "1234") '("1234abcd"))
   (check-equal? (add-literal "abcdefg" test-context "1234") '("1234abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - long line - long indent"
   (define test-context (context 6 6 #false))
   (check-equal? (add-literal "" test-context "1234567") '("1234567"))
   (check-equal? (add-literal "abc" test-context "1234567") '("      abc" "1234567"))
   (check-equal? (add-literal "abcd" test-context "1234567") '("      abcd" "1234567"))
   (check-equal? (add-literal "abcdefg" test-context "1234567") '("      abcdefg" "1234567"))))

(define/provide-test-suite test-add-spaces
  (test-case
   "Test add-spaces (writes spaces nekot) - empty line"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 0 test-context "") '(""))
   (check-equal? (add-spaces 3 test-context "") '("   "))
   (check-equal? (add-spaces 4 test-context "") '("    "))
   (check-equal? (add-spaces 7 test-context "") '("" "")))
  (test-case
   "Test add-spaces (writes spaces nekot) - single-element line"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 0 test-context "123") '("123"))
   (check-equal? (add-spaces 3 test-context "123") '("123   "))
   (check-equal? (add-spaces 4 test-context "123") '("" "123"))
   (check-equal? (add-spaces 7 test-context "123") '("" "123")))
  (test-case
   "Test add-spaces (writes spaces nekot) - short line"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 0 test-context "1234") '("1234"))
   (check-equal? (add-spaces 3 test-context "1234") '("" "1234"))
   (check-equal? (add-spaces 4 test-context "1234") '("" "1234"))
   (check-equal? (add-spaces 7 test-context "1234") '("" "1234")))
  (test-case
   "Test add-spaces (writes spaces nekot) - long line"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 0 test-context "1234567") '("1234567"))
   (check-equal? (add-spaces 3 test-context "1234567") '("" "1234567"))
   (check-equal? (add-spaces 4 test-context "1234567") '("" "1234567"))
   (check-equal? (add-spaces 7 test-context "1234567") '("" "1234567"))))

(define/provide-test-suite test-add-new-line
  (test-case
   "Test add-new-line (starts a new line)"
   (check-equal? (add-new-line null (construct-context 6) "") '("" ""))
   (check-equal? (add-new-line null (construct-context 6) "1") '("" "1"))
   (check-equal? (add-new-line null (construct-context 6) "123") '("" "123"))
   (check-equal? (add-new-line null (construct-context 6) "1234567") '("" "1234567"))))

(define/provide-test-suite test-add-pp-directive
  (test-case
   "Test add-pp-directive (modifies line for preprocessor directive)"
   (check-equal? (add-pp-directive null (construct-context 6) "") '("#"))
   (check-equal? (add-pp-directive null (construct-context 6) " ") '("#"))
   (check-equal? (add-pp-directive null (construct-context 6) "  ") '("# "))
   (check-equal? (add-pp-directive null (construct-context 6) "  /* ") '("  /*#"))))

(define/provide-test-suite test-add-concatenated
  (test-case
   "Test add-concatenated (writes sequence of nekots)"
   (define test-context (context 0 6 #false))
   (check-equal? (add-concatenated (list (nekot 'literal "asdf" test-context)
                                         (nekot 'new-line null test-context)
                                         (nekot 'spaces 2 test-context)
                                         (nekot 'literal "asdf" test-context))
                                   test-context
                                   "")
                 '("  asdf" "asdf"))
   (check-equal? (add-concatenated (list (nekot 'literal "asdf" test-context)
                                         (nekot 'new-line null test-context)
                                         (nekot 'spaces 2 test-context)
                                         (nekot 'pp-directive null test-context)
                                         (nekot 'literal "asdf" test-context))
                                   test-context
                                   "")
                 '("# asdf" "asdf"))))

(define/provide-test-suite test-unknown-nekot-type
  (test-case
   "Test unknown-nekot-type (writes unknown nekot - raises error)"
   (check-exn exn:fail? (λ () (unknown-nekot-type "FAIL" (construct-context 6) '(""))))))

(define/provide-test-suite test-write-nekot
  (test-case
   "Test write-nekot (writes any nekot)"
   (define test-context (context 0 6 #false))
   (check-equal? (write-nekot (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context)
                              "12")
                 '("  asdf" "12asdf"))
   (check-equal? (write-nekot (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'pp-directive null test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context)
                              "123")
                 '("# asdf" "asdf" "123"))
   (check-equal? (write-nekot (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context))
                 '("  asdf" "asdf"))
   (check-equal? (write-nekot (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'pp-directive null test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context))
                 '("# asdf" "asdf"))))

