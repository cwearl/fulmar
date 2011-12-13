#lang racket

(require rackunit)
(require "../fulmar-core.rkt")
(require "../chunk-core.rkt")
(require "../chunk-standard.rkt")
(require "../writer.rkt")

;unit tests for chunk-standard.rkt

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

;keyword chunks

(define/provide-test-suite test-keyword-chunks
  (test-case
   "Test define-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot 'normal (define-chunk test-context) "") '("define"))
   (check-equal? (write-nekot 'normal (define-chunk test-context) "1") '("1define"))
   (check-equal? (write-nekot 'normal (define-chunk test-context) "123456") '("define" "123456")))
  (test-case
   "Test imm-define-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-define-chunk test-context) "") '("define"))
   (check-equal? (write-nekot 'normal (imm-define-chunk test-context) "1") '("1define"))
   (check-equal? (write-nekot 'normal (imm-define-chunk test-context) "123456") '("123456define")))
  (test-case
   "Test ifndef-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot 'normal (ifndef-chunk test-context) "") '("ifndef"))
   (check-equal? (write-nekot 'normal (ifndef-chunk test-context) "1") '("1ifndef"))
   (check-equal? (write-nekot 'normal (ifndef-chunk test-context) "123456") '("ifndef" "123456")))
  (test-case
   "Test imm-ifndef-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-ifndef-chunk test-context) "") '("ifndef"))
   (check-equal? (write-nekot 'normal (imm-ifndef-chunk test-context) "1") '("1ifndef"))
   (check-equal? (write-nekot 'normal (imm-ifndef-chunk test-context) "123456") '("123456ifndef")))
  (test-case
   "Test endif-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (endif-chunk test-context) "") '("endif"))
   (check-equal? (write-nekot 'normal (endif-chunk test-context) "1") '("1endif"))
   (check-equal? (write-nekot 'normal (endif-chunk test-context) "123456") '("endif" "123456")))
  (test-case
   "Test imm-endif-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-endif-chunk test-context) "") '("endif"))
   (check-equal? (write-nekot 'normal (imm-endif-chunk test-context) "1") '("1endif"))
   (check-equal? (write-nekot 'normal (imm-endif-chunk test-context) "123456") '("123456endif")))
  (test-case
   "Test template-chunk"
   (define test-context (construct-context 9))
   (check-equal? (write-nekot 'normal (template-chunk test-context) "") '("template"))
   (check-equal? (write-nekot 'normal (template-chunk test-context) "1") '("1template"))
   (check-equal? (write-nekot 'normal (template-chunk test-context) "123456") '("template" "123456")))
  (test-case
   "Test imm-template-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-template-chunk test-context) "") '("template"))
   (check-equal? (write-nekot 'normal (imm-template-chunk test-context) "1") '("1template"))
   (check-equal? (write-nekot 'normal (imm-template-chunk test-context) "123456") '("123456template")))
  (test-case
   "Test typename-chunk"
   (define test-context (construct-context 9))
   (check-equal? (write-nekot 'normal (typename-chunk test-context) "") '("typename"))
   (check-equal? (write-nekot 'normal (typename-chunk test-context) "1") '("1typename"))
   (check-equal? (write-nekot 'normal (typename-chunk test-context) "123456") '("typename" "123456")))
  (test-case
   "Test imm-typename-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-typename-chunk test-context) "") '("typename"))
   (check-equal? (write-nekot 'normal (imm-typename-chunk test-context) "1") '("1typename"))
   (check-equal? (write-nekot 'normal (imm-typename-chunk test-context) "123456") '("123456typename")))
  (test-case
   "Test typedef-chunk"
   (define test-context (construct-context 8))
   (check-equal? (write-nekot 'normal (typedef-chunk test-context) "") '("typedef"))
   (check-equal? (write-nekot 'normal (typedef-chunk test-context) "1") '("1typedef"))
   (check-equal? (write-nekot 'normal (typedef-chunk test-context) "123456") '("typedef" "123456")))
  (test-case
   "Test imm-typedef-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-typedef-chunk test-context) "") '("typedef"))
   (check-equal? (write-nekot 'normal (imm-typedef-chunk test-context) "1") '("1typedef"))
   (check-equal? (write-nekot 'normal (imm-typedef-chunk test-context) "123456") '("123456typedef")))
  (test-case
   "Test void-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (void-chunk test-context) "") '("void"))
   (check-equal? (write-nekot 'normal (void-chunk test-context) "1") '("1void"))
   (check-equal? (write-nekot 'normal (void-chunk test-context) "123456") '("void" "123456")))
  (test-case
   "Test imm-void-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-void-chunk test-context) "") '("void"))
   (check-equal? (write-nekot 'normal (imm-void-chunk test-context) "1") '("1void"))
   (check-equal? (write-nekot 'normal (imm-void-chunk test-context) "123456") '("123456void")))
  (test-case
   "Test inline-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot 'normal (inline-chunk test-context) "") '("inline"))
   (check-equal? (write-nekot 'normal (inline-chunk test-context) "1") '("1inline"))
   (check-equal? (write-nekot 'normal (inline-chunk test-context) "123456") '("inline" "123456")))
  (test-case
   "Test imm-inline-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-inline-chunk test-context) "") '("inline"))
   (check-equal? (write-nekot 'normal (imm-inline-chunk test-context) "1") '("1inline"))
   (check-equal? (write-nekot 'normal (imm-inline-chunk test-context) "123456") '("123456inline")))
  (test-case
   "Test return-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot 'normal (return-chunk test-context) "") '("return"))
   (check-equal? (write-nekot 'normal (return-chunk test-context) "1") '("1return"))
   (check-equal? (write-nekot 'normal (return-chunk test-context) "123456") '("return" "123456")))
  (test-case
   "Test imm-return-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-return-chunk test-context) "") '("return"))
   (check-equal? (write-nekot 'normal (imm-return-chunk test-context) "1") '("1return"))
   (check-equal? (write-nekot 'normal (imm-return-chunk test-context) "123456") '("123456return")))
  (test-case
   "Test struct-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot 'normal (struct-chunk test-context) "") '("struct"))
   (check-equal? (write-nekot 'normal (struct-chunk test-context) "1") '("1struct"))
   (check-equal? (write-nekot 'normal (struct-chunk test-context) "123456") '("struct" "123456")))
  (test-case
   "Test imm-struct-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-struct-chunk test-context) "") '("struct"))
   (check-equal? (write-nekot 'normal (imm-struct-chunk test-context) "1") '("1struct"))
   (check-equal? (write-nekot 'normal (imm-struct-chunk test-context) "123456") '("123456struct")))
  (test-case
   "Test class-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (class-chunk test-context) "") '("class"))
   (check-equal? (write-nekot 'normal (class-chunk test-context) "1") '("1class"))
   (check-equal? (write-nekot 'normal (class-chunk test-context) "123456") '("class" "123456")))
  (test-case
   "Test imm-class-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot 'normal (imm-class-chunk test-context) "") '("class"))
   (check-equal? (write-nekot 'normal (imm-class-chunk test-context) "1") '("1class"))
   (check-equal? (write-nekot 'normal (imm-class-chunk test-context) "123456") '("123456class"))))

;list chunks

(define/provide-test-suite test-attach-list-separator
  (test-case
   "Test attach-list-separator"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((concat-chunk (attach-list-separator comma-chunk
                                                                    (literal-chunk "asdf")
                                                                    (literal-chunk "jkl")))
                               test-context))
                 '("asdf,jkl"))
   (check-equal? (write-nekot ((concat-chunk (attach-list-separator comma-chunk
                                                                   (literal-chunk "asdf")))
                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot ((concat-chunk (attach-list-separator comma-chunk
                                                                    (literal-chunk "asdf")
                                                                    (literal-chunk "jkl")
                                                                    (literal-chunk "12345")))
                               test-context))
                 '("asdf,jkl,12345"))
   (check-equal? (write-nekot ((concat-chunk (attach-list-separator comma-chunk
                                                                    (literal-chunk "asdf")
                                                                    (literal-chunk "123")
                                                                    (literal-chunk "12345")
                                                                    (literal-chunk "1")))
                               (construct-context 4)))
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-between-chunk
  (test-case
   "Test between-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((between-chunk space-chunk
                                              (literal-chunk "asdf")
                                              (literal-chunk "jkl"))
                               test-context))
                 '("asdf jkl"))
   (check-equal? (write-nekot ((between-chunk space-chunk
                                              (literal-chunk "asdf"))
                              test-context))
                 '("asdf"))
   (check-equal? (write-nekot ((between-chunk space-chunk
                                              (literal-chunk "asdf")
                                              (literal-chunk "jkl")
                                              (literal-chunk "12345"))
                              test-context))
                 '("asdf jkl 12345"))
   (check-equal? (write-nekot ((between-chunk new-line-chunk
                                              (literal-chunk "asdf")
                                              (literal-chunk "123")
                                              (literal-chunk "12345")
                                              (literal-chunk "1"))
                               test-context))
                 '("1" "12345" "123" "asdf"))))

(define/provide-test-suite test-between/attach-chunk
  (test-case
   "Test between/attach-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((between/attach-chunk comma-chunk
                                                     space-chunk
                                                     (literal-chunk "asdf")
                                                     (literal-chunk "jkl"))
                               test-context))
                 '("asdf, jkl"))
   (check-equal? (write-nekot ((between/attach-chunk comma-chunk
                                                     space-chunk
                                                     (literal-chunk "asdf"))
                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot ((between/attach-chunk comma-chunk
                                                     space-chunk
                                                     (literal-chunk "asdf")
                                                     (literal-chunk "jkl")
                                                     (literal-chunk "12345"))
                               test-context))
                 '("asdf, jkl, 12345"))
   (check-equal? (write-nekot ((between/attach-chunk comma-chunk
                                                     space-chunk
                                                     (literal-chunk "asdf")
                                                     (literal-chunk "123")
                                                     (literal-chunk "12345")
                                                     (literal-chunk "1"))
                               test-context))
                 '("asdf, 123, 12345, 1"))
   (check-equal? (write-nekot ((between/attach-chunk comma-chunk
                                                     new-line-chunk
                                                     (literal-chunk "asdf")
                                                     (literal-chunk "123")
                                                     (literal-chunk "12345")
                                                     (literal-chunk "1"))
                               test-context))
                 '("1" "12345," "123," "asdf,"))
   (check-equal? (write-nekot ((between/attach-chunk comma-chunk
                                                     space-chunk
                                                     (literal-chunk "asdf")
                                                     (literal-chunk "123")
                                                     (literal-chunk "12345")
                                                     (literal-chunk "1"))
                               (construct-context 4)))
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-arg-list-chunk
  (test-case
   "Test arg-list-chunk"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot ((arg-list-chunk (literal-chunk "(")
                                               (literal-chunk ",")
                                               (literal-chunk ")")
                                               (literal-chunk "asdf")
                                               empty-chunk
                                               (literal-chunk "jkl"))
                               test-context))
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-nekot ((arg-list-chunk (literal-chunk "(")
                                               (literal-chunk ",")
                                               (literal-chunk ")")
                                               (literal-chunk "asdf")
                                               empty-chunk
                                               (literal-chunk "jkl"))
                               test-context-2))
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-paren-list-chunk
  (test-case
   "Test paren-list-chunk"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot ((paren-list-chunk (literal-chunk "asdf")
                                                 empty-chunk
                                                 (literal-chunk "jkl"))
                               test-context))
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-nekot ((paren-list-chunk (literal-chunk "asdf")
                                                 empty-chunk
                                                 (literal-chunk "jkl"))
                               test-context-2))
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-template-list-chunk
  (test-case
   "Test template-list-chunk"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot ((template-list-chunk (literal-chunk "asdf")
                                                    empty-chunk
                                                    (literal-chunk "jkl"))
                               test-context))
                 '(" jkl>" " ," "<asdf,"))
   (check-equal? (write-nekot ((template-list-chunk (literal-chunk "asdf")
                                                    empty-chunk
                                                    (literal-chunk "jkl"))
                               test-context-2))
                 '("<asdf, , jkl>"))))

(define/provide-test-suite test-smt-list-chunk
  (test-case
   "Test smt-list-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((smt-list-chunk blank-line-chunk
                                               (literal-chunk "asdf")
                                               space-chunk
                                               (literal-chunk "jkl"))
                               test-context))
                 '("jkl;" "" " ;" "" "asdf;"))
   (check-equal? (write-nekot ((smt-list-chunk new-line-chunk
                                               (spaces-chunk 4)
                                               (literal-chunk "asdf")
                                               empty-chunk)
                               test-context))
                 '(";" "asdf;" "    ;"))))

(define/provide-test-suite test-body-chunk
  (test-case
   "Test body-chunk"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 6))
   (check-equal? (write-nekot ((body-chunk (literal-chunk "asdf")
                                           space-chunk
                                           (literal-chunk "jkl"))
                               test-context))
                 '("{ asdf;  ; jkl; }"))
   (check-equal? (write-nekot ((body-chunk (literal-chunk "asdf")
                                           space-chunk
                                           (literal-chunk "jkl"))
                               test-context-2))
                 '("}" "   jkl;" "" "    ;" "" "   asdf;" "{"))))

;preprocessor chunks

(define/provide-test-suite test-pp-define-chunk
  (test-case
   "Test pp-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((pp-define-chunk (literal-chunk "name")) test-context)) '("#define name"))
   (check-equal? (write-nekot ((pp-define-chunk (concat-chunk (literal-chunk "name") (literal-chunk "2"))) test-context)) '("#define name2"))
   (check-equal? (write-nekot ((concat-chunk (spaces-chunk 3) (pp-define-chunk (literal-chunk 'name))) test-context)) '("#  define name"))
   (check-equal? (write-nekot 'normal ((pp-define-chunk (literal-chunk 'name)) test-context) "/* ") '("/*#define name"))))

(define/provide-test-suite test-pp-ifndef-chunk
  (test-case
   "Test pp-ifndef-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((pp-ifndef-chunk (literal-chunk "condition")) test-context)) '("#ifndef condition"))
   (check-equal? (write-nekot ((pp-ifndef-chunk (concat-chunk (literal-chunk "condition") (literal-chunk "2"))) test-context)) '("#ifndef condition2"))
   (check-equal? (write-nekot ((concat-chunk (spaces-chunk 3) (pp-ifndef-chunk (literal-chunk "condition"))) test-context)) '("#  ifndef condition"))
   (check-equal? (write-nekot 'normal ((pp-ifndef-chunk (literal-chunk 'condition)) test-context) "/* ") '("/*#ifndef condition"))))

(define/provide-test-suite test-pp-endif-chunk
  (test-case
   "Test pp-endif-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((pp-endif-chunk (literal-chunk "condition")) test-context)) '("#endif //condition"))
   (check-equal? (write-nekot ((pp-endif-chunk (concat-chunk (literal-chunk "condition") (literal-chunk "2"))) test-context)) '("#endif //condition2"))
   (check-equal? (write-nekot ((concat-chunk (spaces-chunk 3) (pp-endif-chunk (literal-chunk "condition"))) test-context)) '("#  endif //condition"))
   (check-equal? (write-nekot 'normal ((pp-endif-chunk (literal-chunk 'condition)) test-context) "/* ") '("/*#endif //condition"))))

(define/provide-test-suite test-pp-header-file-chunk
  (test-case
   "Test pp-header-file-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((pp-header-file-chunk (literal-chunk "header_file")
                                                     (literal-chunk "asdf")
                                                     (literal-chunk "jkl"))
                               test-context))
                 '("#endif //header_file"
                   ""
                   "   jkl;"
                   ""
                   "   asdf;"
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))
   (check-equal? (write-nekot ((pp-header-file-chunk (literal-chunk "header_file")
                                                     (pp-header-file-chunk (literal-chunk "silly thing")
                                                                           (literal-chunk "asdf")
                                                                           (literal-chunk "jkl")))
                               test-context))
                 '("#endif //header_file"
                   ""
                   "#  endif //silly thing;"
                   ""
                   "      jkl;"
                   ""
                   "      asdf;"
                   ""
                   "#     define silly thing"
                   "#  ifndef silly thing"
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))))

(define/provide-test-suite test-macro-define-chunk
  (test-case
   "Test macro-defintion-chunk"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 19))
   (check-equal? (write-nekot ((macro-define-chunk (literal-chunk 'name)
                                                   null
                                                   (literal-chunk 'asdf))
                               test-context))
                 '("#define name asdf"))
   (check-equal? (write-nekot ((macro-define-chunk (literal-chunk 'name)
                                                   (list (literal-chunk 'first))
                                                   (concat-chunk (literal-chunk 'asdf)
                                                                 space-chunk
                                                                 (literal-chunk 'first)))
                               test-context))
                 '("#define name(first) asdf first"))
   (check-equal? (write-nekot ((macro-define-chunk (literal-chunk 'name)
                                                   (list (literal-chunk 'first))
                                                   (concat-chunk (literal-chunk 'asdf)
                                                                 space-chunk
                                                                 (literal-chunk 'first)))
                               test-context-2))
                 '("   asdf first" "#define name(first) \\"))
   (check-equal? (write-nekot ((macro-define-chunk (literal-chunk 'name)
                                                   (list (literal-chunk 'first)
                                                         (literal-chunk 'second))
                                                   (concat-chunk (literal-chunk 'asdf)
                                                                 space-chunk
                                                                 (literal-chunk 'first)
                                                                 space-chunk
                                                                 (literal-chunk 'second)))
                               test-context))
                 '("#define name(first, second) asdf first second"))
   (check-equal? (write-nekot ((macro-define-chunk (literal-chunk 'name)
                                                   (list (literal-chunk 'first)
                                                         (literal-chunk 'second))
                                                   (concat-chunk (literal-chunk 'asdf)
                                                                 new-line-chunk
                                                                 (literal-chunk 'first)
                                                                 new-line-chunk
                                                                 (literal-chunk 'second)))
                               test-context-2))
                 '("   second"
                   "   first          \\"
                   "   asdf           \\"
                   "             second) \\"
                   "#define name(first, \\"))
   (check-equal? (write-nekot ((macro-define-chunk (literal-chunk 'name)
                                                   (list (concat-chunk (comment-line-chunk (literal-chunk "test1"))
                                                                       new-line-chunk
                                                                       (literal-chunk 'first))
                                                         (literal-chunk 'second))
                                                   (concat-chunk (literal-chunk 'asdf)
                                                                 new-line-chunk
                                                                 (literal-chunk 'first)
                                                                 new-line-chunk
                                                                 (literal-chunk 'second)))
                               test-context-2))
                 '("   second"
                   "   first          \\"
                   "   asdf           \\"
                   "             second) \\"
                   "             first, \\"
                   "#define name(/*test1*/ \\"))
   (check-equal? (write-nekot ((macro-define-chunk (literal-chunk 'name)
                                                   (list (literal-chunk 'first)
                                                         (concat-chunk (comment-line-chunk (literal-chunk "test2"))
                                                                       new-line-chunk
                                                                       (literal-chunk 'second)))
                                                   (concat-chunk (literal-chunk 'asdf)
                                                                 new-line-chunk
                                                                 (literal-chunk 'first)
                                                                 new-line-chunk
                                                                 (literal-chunk 'second)))
                               test-context-2))
                 '("   second"
                   "   first          \\"
                   "   asdf           \\"
                   "             second) \\"
                   "             /*test2*/ \\"
                   "#define name(first, \\"))
   (check-equal? (write-nekot ((macro-define-chunk (literal-chunk 'name)
                                                   (list (concat-chunk (comment-line-chunk (literal-chunk "test1"))
                                                                       new-line-chunk
                                                                       (literal-chunk 'first))
                                                         (concat-chunk (comment-line-chunk (literal-chunk "test2"))
                                                                       new-line-chunk
                                                                       (literal-chunk 'second)))
                                                   (concat-chunk (literal-chunk 'asdf)
                                                                 new-line-chunk
                                                                 (literal-chunk 'first)
                                                                 new-line-chunk
                                                                 (literal-chunk 'second)))
                               test-context-2))
                 '("   second"
                   "   first          \\"
                   "   asdf           \\"
                   "             second) \\"
                   "             /*test2*/ \\"
                   "             first, \\"
                   "#define name(/*test1*/ \\"))))

;template chunks

(define/provide-test-suite test-template-define-chunk
  (test-case
   "Test template-defintion-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((template-define-chunk null
                                                      (literal-chunk 'asdf))
                               test-context))
                 '("asdf" "template<>"))
   (check-equal? (write-nekot ((template-define-chunk (list (literal-chunk 'first))
                                                      (literal-chunk 'asdf))
                               test-context))
                 '("asdf" "template<first>"))
   (check-equal? (write-nekot ((template-define-chunk (list (literal-chunk 'first)
                                                            (literal-chunk 'second))
                                                      (literal-chunk 'asdf))
                               test-context))
                 '("asdf" "template<first, second>"))
   (check-equal? (write-nekot ((template-define-chunk (list (concat-chunk (comment-line-chunk (literal-chunk "test"))
                                                                          new-line-chunk
                                                                          (literal-chunk 'first))
                                                            (literal-chunk 'second))
                                                      (literal-chunk 'asdf))
                               test-context))
                 '("asdf" "         second>" "         first," "template<//test"))))

(define/provide-test-suite test-template-use-chunk
  (test-case
   "Test template-defintion-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((template-use-chunk (literal-chunk 'name)
                                                   null)
                               test-context))
                 '("name<>"))
   (check-equal? (write-nekot ((template-use-chunk (literal-chunk 'name)
                                                   (list (literal-chunk 'first)))
                               test-context))
                 '("name<first>"))
   (check-equal? (write-nekot ((template-use-chunk (literal-chunk 'name)
                                                   (list (literal-chunk 'first)
                                                         (literal-chunk 'second)))
                               test-context))
                 '("name<first, second>"))
   (check-equal? (write-nekot ((template-use-chunk (literal-chunk 'name)
                                                   (list (literal-chunk 'first)
                                                         (literal-chunk 'second)))
                               (construct-context 6)))
                 '("     second>" "name<first,"))))

;function chunks

(define/provide-test-suite test-function-declare-chunk
  (test-case
   "Test function-declare-chunk - with return type qualifier"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((function-declare-chunk (literal-chunk 'name)
                                                       (literal-chunk 'return-type)
                                                       (literal-chunk 'qualifier)
                                                       null)
                               test-context))
                 '("inline return-type qualifier name (void)"))
   (check-equal? (write-nekot ((function-declare-chunk (literal-chunk 'name)
                                                       (literal-chunk 'return-type)
                                                       (literal-chunk 'qualifier)
                                                       (list (literal-chunk 'first)))
                               test-context))
                 '("inline return-type qualifier name (first)"))
   (check-equal? (write-nekot ((function-declare-chunk (literal-chunk 'name)
                                                       (literal-chunk 'return-type)
                                                       (literal-chunk 'qualifier)
                                                       (list (literal-chunk 'first)
                                                             (literal-chunk 'second)))
                               test-context))
                 '("inline return-type qualifier name (first, second)"))
   (check-equal? (write-nekot ((function-declare-chunk (literal-chunk 'name)
                                                       (literal-chunk 'return-type)
                                                       (literal-chunk 'qualifier)
                                                       (list (literal-chunk 'first)
                                                             (literal-chunk 'second)))
                               (construct-context 20)))
                 '("                                   second)"
                   "inline return-type qualifier name (first,")))
  (test-case
   "Test function-declare-chunk - without return type qualifier"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((function-declare-chunk (literal-chunk 'name)
                                                       (literal-chunk 'return-type)
                                                       null)
                               test-context))
                 '("inline return-type name (void)"))
   (check-equal? (write-nekot ((function-declare-chunk (literal-chunk 'name)
                                                       (literal-chunk 'return-type)
                                                       (list (literal-chunk 'first)))
                               test-context))
                 '("inline return-type name (first)"))
   (check-equal? (write-nekot ((function-declare-chunk (literal-chunk 'name)
                                                       (literal-chunk 'return-type)
                                                       (list (literal-chunk 'first)
                                                             (literal-chunk 'second)))
                               test-context))
                 '("inline return-type name (first, second)"))
   (check-equal? (write-nekot ((function-declare-chunk (literal-chunk 'name)
                                                       (literal-chunk 'return-type)
                                                       (list (literal-chunk 'first)
                                                             (literal-chunk 'second)))
                               (construct-context 20)))
                 '("                         second)"
                   "inline return-type name (first,"))))

(define/provide-test-suite test-void-function-declare-chunk
  (test-case
   "Test void-function-declare-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((void-function-declare-chunk (literal-chunk 'name)
                                                            null)
                               test-context))
                 '("inline void name (void)"))
   (check-equal? (write-nekot ((void-function-declare-chunk (literal-chunk 'name)
                                                       (list (literal-chunk 'first)))
                               test-context))
                 '("inline void name (first)"))
   (check-equal? (write-nekot ((void-function-declare-chunk (literal-chunk 'name)
                                                            (list (literal-chunk 'first)
                                                             (literal-chunk 'second)))
                               test-context))
                 '("inline void name (first, second)"))
   (check-equal? (write-nekot ((void-function-declare-chunk (literal-chunk 'name)
                                                            (list (literal-chunk 'first)
                                                             (literal-chunk 'second)))
                               (construct-context 20)))
                 '("                  second)"
                   "inline void name (first,"))))

(define/provide-test-suite test-function-define-chunk
  (test-case
   "Test function-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((function-define-chunk (literal-chunk 'signature)
                                                      null)
                               test-context))
                 '("signature {}"))
   (check-equal? (write-nekot ((function-define-chunk (literal-chunk 'signature)
                                                      (list (literal-chunk 'first)))
                               test-context))
                 '("signature { first; }"))
   (check-equal? (write-nekot ((function-define-chunk (literal-chunk 'signature)
                                                      (list (literal-chunk 'first)
                                                            (literal-chunk 'second)))
                               test-context))
                 '("signature { first; second; }"))
   (check-equal? (write-nekot ((function-define-chunk (literal-chunk 'signature)
                                                      (list (literal-chunk 'first)
                                                            (literal-chunk 'second)))
                               (construct-context 20)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   "signature {"))))

(define/provide-test-suite test-void-function-define-chunk
  (test-case
   "Test void-function-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((void-function-define-chunk (literal-chunk 'name)
                                                           null
                                                           null)
                               test-context))
                 '("inline void name (void) {}"))
   (check-equal? (write-nekot ((void-function-define-chunk (literal-chunk 'name)
                                                      (list (literal-chunk 'first))
                                                      (list (literal-chunk 'first)))
                               test-context))
                 '("inline void name (first) { first; }"))
   (check-equal? (write-nekot ((void-function-define-chunk (literal-chunk 'name)
                                                      (list (literal-chunk 'first)
                                                            (literal-chunk 'second))
                                                      (list (literal-chunk 'first)
                                                            (literal-chunk 'second)))
                               test-context))
                 '("inline void name (first, second) { first; second; }"))
   (check-equal? (write-nekot ((void-function-define-chunk (literal-chunk 'name)
                                                      (list (literal-chunk 'first)
                                                            (literal-chunk 'second))
                                                      (list (literal-chunk 'first)
                                                            (literal-chunk 'second)))
                               (construct-context 10)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   "                  second) {"
                   "inline void name (first,"))))

(define/provide-test-suite test-returning-function-define-chunk
  (test-case
   "Test returning-function-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((returning-function-define-chunk (function-declare-chunk (literal-chunk 'name)
                                                                                        (literal-chunk 'return-type)
                                                                                        null)
                                                                null
                                                                (literal-chunk "expr"))
                               test-context))
                 '("inline return-type name (void) { return expr; }"))
   (check-equal? (write-nekot ((returning-function-define-chunk (function-declare-chunk (literal-chunk 'name)
                                                                                        (literal-chunk 'return-type)
                                                                                        (list (literal-chunk 'first)))
                                                                (list (literal-chunk 'first))
                                                                (literal-chunk "expr"))

                               test-context))
                 '("inline return-type name (first) { first; return expr; }"))
   (check-equal? (write-nekot ((returning-function-define-chunk (function-declare-chunk (literal-chunk 'name)
                                                                                        (literal-chunk 'return-type)
                                                                                        (list (literal-chunk 'first)
                                                                                              (literal-chunk 'second)))
                                                                (list (literal-chunk 'first)
                                                                      (literal-chunk 'second))
                                                                (literal-chunk "expr"))
                               test-context))
                 '("inline return-type name (first, second) { first; second; return expr; }"))
   (check-equal? (write-nekot ((returning-function-define-chunk (function-declare-chunk (literal-chunk 'name)
                                                                                        (literal-chunk 'return-type)
                                                                                        (list (literal-chunk 'first)
                                                                                              (literal-chunk 'second)))
                                                                (list (literal-chunk 'first)
                                                                      (literal-chunk 'second))
                                                                (literal-chunk "expr"))
                               (construct-context 15)))
                 '("}"
                   "   return expr;"
                   ""
                   "   second;"
                   ""
                   "   first;"
                   "                         second) {"
                   "inline return-type name (first,"))))

;class/struct chunks

(define/provide-test-suite test-struct-declare-chunk
  (test-case
   "Test struct-declare-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((struct-declare-chunk (literal-chunk 'name))
                               test-context))
                 '("struct name"))))

(define/provide-test-suite test-template-struct-declare-chunk
  (test-case
   "Test template-struct-declare-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((template-struct-declare-chunk (literal-chunk 'name)
                                                              null
                                                              null)
                               test-context))
                 '("struct name<>"
                   "template<>"))
   (check-equal? (write-nekot ((template-struct-declare-chunk (literal-chunk 'name)
                                                              (list (literal-chunk 'first))
                                                              (list (literal-chunk 'first)))
                               test-context))
                 '("struct name<first>"
                   "template<first>"))
   (check-equal? (write-nekot ((template-struct-declare-chunk (literal-chunk 'name)
                                                              (list (literal-chunk 'first)
                                                                    (literal-chunk 'second))
                                                              (list (literal-chunk 'first)
                                                                    (literal-chunk 'second)))
                               test-context))
                 '("struct name<first, second>"
                   "template<first, second>"))
   (check-equal? (write-nekot ((template-struct-declare-chunk (literal-chunk 'name)
                                                              (list (literal-chunk 'first))
                                                              (list (literal-chunk 'first)
                                                                    (literal-chunk 'second)))
                               test-context))
                 '("struct name<first, second>"
                   "template<first>"))))

(define/provide-test-suite test-section-define-chunk
  (test-case
   "Test section-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((section-define-chunk (literal-chunk 'name)
                                                     (literal-chunk 'first)
                                                     (literal-chunk 'second))
                               test-context))
                 '(" second;"
                   " first;"
                   "name:"))))

(define/provide-test-suite test-struct-define-chunk
  (test-case
   "Test struct-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot ((struct-define-chunk (literal-chunk 'signature)
                                                    (list (literal-chunk 'first)
                                                          (literal-chunk 'second)))
                               test-context))
                 '("signature { first; second; }"))
   (check-equal? (write-nekot ((struct-define-chunk (literal-chunk 'signature)
                                                    (list (literal-chunk 'first)
                                                          (literal-chunk 'second)))
                               (construct-context 8)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   "signature {"))))

