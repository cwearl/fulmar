#lang racket

(require rackunit)
(require "../fulmar-core.rkt")
(require "../chunk-core.rkt")
(require "../chunk-standard.rkt")
(require "../writer.rkt")

;unit tests for chunk-standard.rkt

;list chunks

(define/provide-test-suite test-imm-list-chunk
  (test-case
   "Test imm-list-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((imm-list-chunk (literal-chunk "asdf")
                                               (spaces-chunk 4)
                                               (literal-chunk "jkl")) test-context))
                 '("asdf    jkl"))
   (check-equal? (write-nekot ((imm-list-chunk (spaces-chunk 4)
                                               (literal-chunk "asdf")
                                               new-line-chunk) test-context))
                 '("" "    asdf"))))

(define/provide-test-suite test-arg-list-chunk
  (test-case
   "Test arg-list-chunk"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot ((arg-list-chunk (literal-chunk "asdf")
                                               empty-chunk
                                               (literal-chunk "jkl")) test-context))
                 '("jkl" "" "asdf"))
   (check-equal? (write-nekot ((arg-list-chunk (literal-chunk "asdf")
                                               empty-chunk
                                               (literal-chunk "jkl")) test-context-2))
                 '("asdf  jkl"))))

(define/provide-test-suite test-smt-list-chunk
  (test-case
   "Test smt-list-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((smt-list-chunk (literal-chunk "asdf")
                                               new-line-chunk
                                               (literal-chunk "jkl")) test-context))
                 '("jkl" "" "" "asdf"))
   (check-equal? (write-nekot ((smt-list-chunk (spaces-chunk 4)
                                               (literal-chunk "asdf")
                                               empty-chunk) test-context))
                 '("" "asdf" ""))))

(define/provide-test-suite test-dfn-list-chunk
  (test-case
   "Test dfn-list-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((dfn-list-chunk (literal-chunk "asdf")
                                               new-line-chunk
                                               (literal-chunk "jkl")) test-context))
                 '("jkl" "" "" "" "" "asdf"))
   (check-equal? (write-nekot ((dfn-list-chunk (spaces-chunk 4)
                                               empty-chunk) test-context))
                 '("" "" ""))))

;character chunks

(define/provide-test-suite test-character-chunks
  (test-case
   "Test space-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (space-chunk test-context) "") '(" "))
   (check-equal? (write-nekot 'normal (space-chunk test-context) "1") '("1 "))
   (check-equal? (write-nekot 'normal (space-chunk test-context) "123456") '("" "123456")))
  (test-case
   "Test imm-space-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-space-chunk test-context) "") '(" "))
   (check-equal? (write-nekot 'normal (imm-space-chunk test-context) "1") '("1 "))
   (check-equal? (write-nekot 'normal (imm-space-chunk test-context) "123456") '("123456 ")))
  (test-case
   "Test blank-lines-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal ((blank-lines-chunk 1) test-context) "1") '("" "" "1"))
   (check-equal? (write-nekot 'normal ((blank-lines-chunk 3) test-context) "1") '("" "" "" "" "1")))
  (test-case
   "Test blank-line-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (blank-line-chunk test-context) "1") '("" "" "1"))
   (check-equal? (write-nekot 'normal (blank-line-chunk test-context) "123") '("" "" "123")))
  (test-case
   "Test open-paren-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (open-paren-chunk test-context) "") '("("))
   (check-equal? (write-nekot 'normal (open-paren-chunk test-context) "1") '("1("))
   (check-equal? (write-nekot 'normal (open-paren-chunk test-context) "123456") '("(" "123456")))
  (test-case
   "Test imm-open-paren-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-open-paren-chunk test-context) "") '("("))
   (check-equal? (write-nekot 'normal (imm-open-paren-chunk test-context) "1") '("1("))
   (check-equal? (write-nekot 'normal (imm-open-paren-chunk test-context) "123456") '("123456(")))
  (test-case
   "Test close-paren-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (close-paren-chunk test-context) "") '(")"))
   (check-equal? (write-nekot 'normal (close-paren-chunk test-context) "1") '("1)"))
   (check-equal? (write-nekot 'normal (close-paren-chunk test-context) "123456") '(")" "123456")))
  (test-case
   "Test imm-close-paren-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-close-paren-chunk test-context) "") '(")"))
   (check-equal? (write-nekot 'normal (imm-close-paren-chunk test-context) "1") '("1)"))
   (check-equal? (write-nekot 'normal (imm-close-paren-chunk test-context) "123456") '("123456)")))
  (test-case
   "Test open-crbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (open-crbr-chunk test-context) "") '("{"))
   (check-equal? (write-nekot 'normal (open-crbr-chunk test-context) "1") '("1{"))
   (check-equal? (write-nekot 'normal (open-crbr-chunk test-context) "123456") '("{" "123456")))
  (test-case
   "Test imm-open-crbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-open-crbr-chunk test-context) "") '("{"))
   (check-equal? (write-nekot 'normal (imm-open-crbr-chunk test-context) "1") '("1{"))
   (check-equal? (write-nekot 'normal (imm-open-crbr-chunk test-context) "123456") '("123456{")))(test-case
   "Test close-crbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (close-crbr-chunk test-context) "") '("}"))
   (check-equal? (write-nekot 'normal (close-crbr-chunk test-context) "1") '("1}"))
   (check-equal? (write-nekot 'normal (close-crbr-chunk test-context) "123456") '("}" "123456")))
  (test-case
   "Test imm-close-crbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-close-crbr-chunk test-context) "") '("}"))
   (check-equal? (write-nekot 'normal (imm-close-crbr-chunk test-context) "1") '("1}"))
   (check-equal? (write-nekot 'normal (imm-close-crbr-chunk test-context) "123456") '("123456}")))
  (test-case
   "Test open-anbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (open-anbr-chunk test-context) "") '("<"))
   (check-equal? (write-nekot 'normal (open-anbr-chunk test-context) "1") '("1<"))
   (check-equal? (write-nekot 'normal (open-anbr-chunk test-context) "123456") '("<" "123456")))
  (test-case
   "Test imm-open-anbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-open-anbr-chunk test-context) "") '("<"))
   (check-equal? (write-nekot 'normal (imm-open-anbr-chunk test-context) "1") '("1<"))
   (check-equal? (write-nekot 'normal (imm-open-anbr-chunk test-context) "123456") '("123456<")))
  (test-case
   "Test close-anbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (close-anbr-chunk test-context) "") '(">"))
   (check-equal? (write-nekot 'normal (close-anbr-chunk test-context) "1") '("1>"))
   (check-equal? (write-nekot 'normal (close-anbr-chunk test-context) "123456") '(">" "123456")))
  (test-case
   "Test imm-close-anbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-close-anbr-chunk test-context) "") '(">"))
   (check-equal? (write-nekot 'normal (imm-close-anbr-chunk test-context) "1") '("1>"))
   (check-equal? (write-nekot 'normal (imm-close-anbr-chunk test-context) "123456") '("123456>")))
  (test-case
   "Test comma-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (comma-chunk test-context) "") '(","))
   (check-equal? (write-nekot 'normal (comma-chunk test-context) "1") '("1,"))
   (check-equal? (write-nekot 'normal (comma-chunk test-context) "123456") '("," "123456")))
  (test-case
   "Test imm-comma-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-comma-chunk test-context) "") '(","))
   (check-equal? (write-nekot 'normal (imm-comma-chunk test-context) "1") '("1,"))
   (check-equal? (write-nekot 'normal (imm-comma-chunk test-context) "123456") '("123456,"))))

;preprocessor chunks

(define/provide-test-suite test-pp-define-chunk
  (test-case
   "Test pp-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot 'normal ((pp-define-chunk "name") test-context) "") '("#define name"))
   (check-equal? (write-nekot 'normal ((pp-define-chunk 'name) test-context) "") '("#define name"))
   (check-equal? (write-nekot 'normal ((concat-chunk (spaces-chunk 3) (pp-define-chunk 'name)) test-context) "") '("#  define name"))
   (check-equal? (write-nekot 'normal ((pp-define-chunk 'name) test-context) "/* ") '("/*#define name"))))

(define/provide-test-suite test-pp-ifndef-chunk
  (test-case
   "Test pp-ifndef-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot 'normal ((pp-ifndef-chunk "condition") test-context) "") '("#ifndef condition"))
   (check-equal? (write-nekot 'normal ((pp-ifndef-chunk 'condition) test-context) "") '("#ifndef condition"))
   (check-equal? (write-nekot 'normal ((concat-chunk (spaces-chunk 3) (pp-ifndef-chunk 'condition)) test-context) "") '("#  ifndef condition"))
   (check-equal? (write-nekot 'normal ((pp-ifndef-chunk 'condition) test-context) "/* ") '("/*#ifndef condition"))))

(define/provide-test-suite test-pp-endif-chunk
  (test-case
   "Test pp-endif-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot 'normal ((pp-endif-chunk "condition") test-context) "") '("#endif //condition"))
   (check-equal? (write-nekot 'normal ((pp-endif-chunk 'condition) test-context) "") '("#endif //condition"))
   (check-equal? (write-nekot 'normal ((concat-chunk (spaces-chunk 3) (pp-endif-chunk 'condition)) test-context) "") '("#  endif //condition"))
   (check-equal? (write-nekot 'normal ((pp-endif-chunk 'condition) test-context) "/* ") '("/*#endif //condition"))))

(define/provide-test-suite test-pp-header-file-chunk
  (test-case
   "Test pp-header-file-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot 'normal
                              ((pp-header-file-chunk "header_file"
                                                     (literal-chunk "asdf")
                                                     (literal-chunk "jkl")) test-context)
                              "")
                 '("#endif //header_file"
                   ""
                   "   jkl"
                   ""
                   "   asdf"
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))
   (check-equal? (write-nekot 'normal
                              ((pp-header-file-chunk "header_file"
                                                     (pp-header-file-chunk "silly thing"
                                                                           (literal-chunk "asdf")
                                                                           (literal-chunk "jkl"))) test-context)
                              "")
                 '("#endif //header_file"
                   ""
                   "#  endif //silly thing"
                   ""
                   "      jkl"
                   ""
                   "      asdf"
                   ""
                   "#     define silly thing"
                   "#  ifndef silly thing"
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))))

;macro chunks

(define/provide-test-suite test-macro-definition-chunk
  (test-case
   "Test macro-defintion-chunk"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 19))
   (check-equal? (write-nekot 'normal
                              ((macro-definition-chunk 'name null (literal-chunk 'asdf)) test-context)
                              "")
                 '("#define name asdf"))
   (check-equal? (write-nekot 'normal
                              ((macro-definition-chunk 'name
                                                       (list 'first)
                                                       (concat-chunk (literal-chunk 'asdf)
                                                                     space-chunk
                                                                     (literal-chunk 'first)))
                               test-context)
                              "")
                 '("#define name(first) asdf first"))
   (check-equal? (write-nekot 'normal
                              ((macro-definition-chunk 'name
                                                       (list 'first)
                                                       (concat-chunk (literal-chunk 'asdf)
                                                                     space-chunk
                                                                     (literal-chunk 'first)))
                               test-context-2)
                              "")
                 '("   asdf first" "#define name(first) \\"))
   (check-equal? (write-nekot 'normal
                              ((macro-definition-chunk 'name
                                                       (list 'first 'second)
                                                       (concat-chunk (literal-chunk 'asdf)
                                                                     space-chunk
                                                                     (literal-chunk 'first)
                                                                     space-chunk
                                                                     (literal-chunk 'second)))
                               test-context)
                              "")
                 '("#define name(first, second) asdf first second"))
   (check-equal? (write-nekot 'normal
                              ((macro-definition-chunk 'name
                                                       (list 'first 'second)
                                                       (concat-chunk (literal-chunk 'asdf)
                                                                     new-line-chunk
                                                                     (literal-chunk 'first)
                                                                     new-line-chunk
                                                                     (literal-chunk 'second)))
                               test-context-2)
                              "")
                 '("   second" "   first          \\" "   asdf           \\" "             second) \\" "#define name(first, \\"))
   (check-equal? (write-nekot 'normal
                              ((macro-definition-chunk 'name
                                                       (list (list "test1" 'first) 'second)
                                                       (concat-chunk (literal-chunk 'asdf)
                                                                     new-line-chunk
                                                                     (literal-chunk 'first)
                                                                     new-line-chunk
                                                                     (literal-chunk 'second)))
                               test-context-2)
                              "")
                 '("   second" "   first          \\" "   asdf           \\" "             second) \\" "             first, \\" "#define name(/*test1*/ \\"))
   (check-equal? (write-nekot 'normal
                              ((macro-definition-chunk 'name
                                                       (list 'first (list 'test2 'second))
                                                       (concat-chunk (literal-chunk 'asdf)
                                                                     new-line-chunk
                                                                     (literal-chunk 'first)
                                                                     new-line-chunk
                                                                     (literal-chunk 'second)))
                               test-context-2)
                              "")
                 '("   second" "   first          \\" "   asdf           \\" "             second) \\""             /*test2*/ \\" "#define name(first, \\"))
   (check-equal? (write-nekot 'normal
                              ((macro-definition-chunk 'name
                                                       (list (list "test1" 'first) (list 'test2 'second))
                                                       (concat-chunk (literal-chunk 'asdf)
                                                                     new-line-chunk
                                                                     (literal-chunk 'first)
                                                                     new-line-chunk
                                                                     (literal-chunk 'second)))
                               test-context-2)
                              "")
                 '("   second" "   first          \\" "   asdf           \\" "             second) \\""             /*test2*/ \\" "             first, \\" "#define name(/*test1*/ \\"))))
