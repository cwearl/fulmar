#lang racket

(require rackunit)
(require "../private/fulmar-core.rkt")
(require "../private/core-chunk.rkt")
(require "../standard-chunk.rkt")
(require "../private/writer.rkt")

;unit tests for standard-chunk.rkt

;basic chunks

(define/provide-test-suite test-basic-chunks
  (test-case
   "Test blank-lines"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (concat "1" (blank-lines 1))
                                               test-context))
                 '("" "" "1"))
   (check-equal? (write-nekot (chunk-transform (concat "1" (blank-lines 3))
                                               test-context))
                 '("" "" "" "" "1")))
  (test-case
   "Test blank-line"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (concat "1" blank-line)
                                               test-context))
                 '("" "" "1"))
   (check-equal? (write-nekot (chunk-transform (concat "123" blank-line)
                                               test-context))
                 '("" "" "123"))))

;list chunks

(define/provide-test-suite test-attach-list-separator
  (test-case
   "Test attach-list-separator"
   (define test-context (construct-context 80))
   (check-equal? (attach-list-separator ",")
                 null)
   (check-equal? (write-nekot (chunk-transform (concat (attach-list-separator "," 'asdf 'jkl))
                                               test-context))
                 '("asdf,jkl"))
   (check-equal? (write-nekot (chunk-transform (concat (attach-list-separator "," 'asdf))
                                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (concat (attach-list-separator "," 'asdf 'jkl "12345"))
                                               test-context))
                 '("asdf,jkl,12345"))
   (check-equal? (write-nekot (chunk-transform (concat (attach-list-separator "," 'asdf "123" "12345" "1"))
                                               (construct-context 4)))
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-between
  (test-case
   "Test between"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (between 1)
                                               test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform (between 1 'asdf 'jkl)
                                               test-context))
                 '("asdf jkl"))
   (check-equal? (write-nekot (chunk-transform (between 1 'asdf)
                                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (between 1 'asdf 'jkl "12345")
                                               test-context))
                 '("asdf jkl 12345"))
   (check-equal? (write-nekot (chunk-transform (between new-line 'asdf "123" "12345" "1")
                                               test-context))
                 '("1" "12345" "123" "asdf"))))

(define/provide-test-suite test-between/attach
  (test-case
   "Test between/attach"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (between/attach "," 1)
                                               test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform (between/attach "," 1 'asdf 'jkl)
                                               test-context))
                 '("asdf, jkl"))
   (check-equal? (write-nekot (chunk-transform (between/attach "," 1 'asdf)
                                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (between/attach "," 1 'asdf 'jkl "12345")
                                               test-context))
                 '("asdf, jkl, 12345"))
   (check-equal? (write-nekot (chunk-transform (between/attach "," 1 'asdf "123" "12345" "1")
                                               test-context))
                 '("asdf, 123, 12345, 1"))
   (check-equal? (write-nekot (chunk-transform (between/attach "," new-line 'asdf "123" "12345" "1")
                                               test-context))
                 '("1" "12345," "123," "asdf,"))
   (check-equal? (write-nekot (chunk-transform (between/attach "," 1 'asdf "123" "12345" "1")
                                               (construct-context 4)))
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-arg-list
  (test-case
   "Test arg-list"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (arg-list sur-paren "," 'asdf)
                                               test-context))
                 '("(asdf)"))
   (check-equal? (write-nekot (chunk-transform (arg-list sur-paren "," (concat 'asdf 'jkl))
                                               test-context))
                 '(" jkl)"
                   "(asdf"))
   (check-equal? (write-nekot (chunk-transform (arg-list sur-paren "," 'asdf empty 'jkl)
                                               test-context))
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-nekot (chunk-transform (arg-list sur-paren "," 'asdf empty 'jkl)
                                               test-context-2))
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-paren-list
  (test-case
   "Test paren-list"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (paren-list 'asdf empty 'jkl)
                                               test-context))
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-nekot (chunk-transform (paren-list 'asdf empty 'jkl)
                                               test-context-2))
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-template-list
  (test-case
   "Test template-list"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-list 'asdf empty 'jkl)
                                               test-context))
                 '(" jkl>" " ," "<asdf,"))
   (check-equal? (write-nekot (chunk-transform (template-list 'asdf empty 'jkl)
                                               test-context-2))
                 '("<asdf, , jkl>"))))

(define/provide-test-suite test-smt-list
  (test-case
   "Test smt-list"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (smt-list blank-line 'asdf 1 'jkl)
                                               test-context))
                 '("jkl;" "" " ;" "" "asdf;"))
   (check-equal? (write-nekot (chunk-transform (smt-list new-line
                                                         (spaces 4)
                                                         'asdf
                                                         empty)
                                               test-context))
                 '(";" "asdf;" "    ;"))))


(define/provide-test-suite test-constructor-assignment-list
  (test-case
   "Test constructor-assignment-list"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list)
                                               test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list 'asdf)
                                               test-context))
                 '("  : asdf"))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list 'asdf 'jkl)
                                               test-context))
                 '("  : asdf, jkl"))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list 'asdf 'jkl)
                                               test-context-2))
                 '("    jkl"
                   "  : asdf,"))))

(define/provide-test-suite test-body
  (test-case
   "Test body"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (body 'asdf 1 'jkl)
                                               test-context))
                 '("{ asdf;  ; jkl; }"))
   (check-equal? (write-nekot (chunk-transform (body "123456")
                                               test-context-2))
                 '("}"
                   "   123456;"
                   "{"))
   (check-equal? (write-nekot (chunk-transform (body 'asdf 1 'jkl)
                                               test-context-2))
                 '("}"
                   "   jkl;"
                   ""
                   "    ;"
                   ""
                   "   asdf;"
                   "{"))))

;preprocessor chunks

(define/provide-test-suite test-pp-define
  (test-case
   "Test pp-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-define 'name) test-context)) '("#define name"))
   (check-equal? (write-nekot (chunk-transform (pp-define (concat 'name "2")) test-context)) '("#define name2"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-define 'name)) test-context)) '("#  define name"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-define 'name)) test-context)) '("/*#define name"))))

(define/provide-test-suite test-pp-include
  (test-case
   "Test pp-include"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-include 'name) test-context)) '("#include <name>"))
   (check-equal? (write-nekot (chunk-transform (pp-include (concat 'name "2")) test-context)) '("#include <name2>"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-include 'name)) test-context)) '("#  include <name>"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-include 'name)) test-context)) '("/*#include <name>"))))

(define/provide-test-suite test-pp-includes
  (test-case
   "Test pp-includes"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-includes 'name) test-context)) '("#include <name>"))
   (check-equal? (write-nekot (chunk-transform (pp-includes 'name 'name2)
                                               test-context))
                 '("#include <name2>"
                   "#include <name>"))))

(define/provide-test-suite test-pp-ifdef
  (test-case
   "Test pp-ifdef"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-ifdef 'condition) test-context)) '("#ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-ifdef (concat 'condition "2")) test-context)) '("#ifdef condition2"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-ifdef 'condition)) test-context)) '("#  ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-ifdef 'condition)) test-context)) '("/*#ifdef condition"))))

(define/provide-test-suite test-pp-ifndef
  (test-case
   "Test pp-ifndef"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-ifndef 'condition) test-context)) '("#ifndef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-ifndef (concat 'condition "2")) test-context)) '("#ifndef condition2"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-ifndef 'condition)) test-context)) '("#  ifndef condition"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-ifndef 'condition)) test-context)) '("/*#ifndef condition"))))

(define/provide-test-suite test-pp-else
  (test-case
   "Test pp-else"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform pp-else test-context)) '("#else"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) pp-else) test-context)) '("#  else"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " pp-else) test-context)) '("/*#else"))))

(define/provide-test-suite test-pp-endif
  (test-case
   "Test pp-endif"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-endif 'condition) test-context)) '("/* condition */" "#endif"))
   (check-equal? (write-nekot (chunk-transform (pp-endif (concat 'condition "2")) test-context)) '("/* condition2 */" "#endif"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-endif 'condition)) test-context)) '("/* condition */" "#  endif"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-endif 'condition)) test-context)) '("/* condition */" "/*#endif"))))

(define/provide-test-suite test-pp-conditional
  (test-case
   "Test pp-conditional"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-conditional 'ifdef 'condition 'then)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   then"
                   "#ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-conditional 'ifndef 'condition 'then 'else2)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifndef condition"))))

(define/provide-test-suite test-pp-conditional-ifdef
  (test-case
   "Test pp-conditional-ifdef"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifdef 'condition 'then)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   then"
                   "#ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifdef 'condition 'then 'else2)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifdef condition"))))

(define/provide-test-suite test-pp-conditional-ifndef
  (test-case
   "Test pp-conditional-ifndef"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifndef 'condition 'then)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   then"
                   "#ifndef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifndef 'condition 'then 'else2)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifndef condition"))))

(define/provide-test-suite test-pp-header-file
  (test-case
   "Test pp-header-file"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-header-file 'header_file empty 'asdf 'jkl)
                                               test-context))
                 '("/* header_file */"
                   "#endif"
                   ""
                   "   jkl"
                   ""
                   "   asdf"
                   ""
                   ""
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))
   (check-equal? (write-nekot (chunk-transform (pp-header-file 'header_file
                                                               (concat (pp-include 'iostream)
                                                                       new-line
                                                                       (pp-include 'algorithm))
                                                               'asdf
                                                               'jkl)
                                               test-context))
                 '("/* header_file */"
                   "#endif"
                   ""
                   "   jkl"
                   ""
                   "   asdf"
                   ""
                   "#  include <algorithm>"
                   "#  include <iostream>"
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))))

(define/provide-test-suite test-macro-define
  (test-case
   "Test macro-defintion"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 20))
   (check-equal? (write-nekot (chunk-transform (macro-define 'name null 'asdf)
                                               test-context))
                 '("#define name asdf"))))

;general chunks

(define/provide-test-suite test-namespace-define
  (test-case
   "Test namespace-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (namespace-define 'name 'asdf)
                                               test-context))
                 '("namespace name { asdf; } /* name */"))
   (check-equal? (write-nekot (chunk-transform (namespace-define 'name 'asdf)
                                               (construct-context 12)))
                 '("} /* name */"
                   "   asdf;"
                   "namespace name {"))
   (check-equal? (write-nekot (chunk-transform (namespace-define 'name 'asdf 'jkl)
                                               (construct-context 12)))
                 '("} /* name */"
                   "   jkl;"
                   ""
                   "   asdf;"
                   "namespace name {"))))

(define/provide-test-suite test-described-smts
  (test-case
   "Test described-smts"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (described-smts 'name 'asdf)
                                               test-context))
                 '("asdf"
                   "/* name */"))
   (check-equal? (write-nekot (chunk-transform (described-smts 'name 'asdf 'jkl)
                                               test-context))
                 '("jkl"
                   "asdf;"
                   "/* name */"))
   (check-equal? (write-nekot (chunk-transform (described-smts 'name 'asdf 'jkl "1234")
                                               test-context))
                 '("1234"
                   "jkl;"
                   "asdf;"
                   "/* name */"))))

(define/provide-test-suite test-constize
  (test-case
   "Test constize"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (constize 'asdf)
                                               test-context))
                 '("asdf const"))
   (check-equal? (write-nekot (chunk-transform (constize 'asdf)
                                               (construct-context 4)))
                 '("asdf const"))))

;template chunks

(define/provide-test-suite test-template-define
  (test-case
   "Test template-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-define null 'asdf)
                                               test-context))
                 '(" asdf"
                   "template<>"))
   (check-equal? (write-nekot (chunk-transform (template-define (list 'first) 
                                                                'asdf)
                                               test-context))
                 '(" asdf"
                   "template<first>"))
   (check-equal? (write-nekot (chunk-transform (template-define (list 'first 'second)
                                                                'asdf)
                                               test-context))
                 '(" asdf"
                   "template<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-define (list (concat (comment-env-chunk 'test)
                                                                              new-line
                                                                              'first)
                                                                      'second)
                                                                'asdf)
                                               test-context))
                 '(" asdf"
                   "         second>"
                   "         first,"
                   "template</* test */"))))

(define/provide-test-suite test-template-use
  (test-case
   "Test template-defintion"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-use 'name null)
                                               test-context))
                 '("name"))
   (check-equal? (write-nekot (chunk-transform (template-use 'name (list 'first))
                                               test-context))
                 '("name<first>"))
   (check-equal? (write-nekot (chunk-transform (template-use 'name (list 'first 'second))
                                               test-context))
                 '("name<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-use 'name (list 'first 'second))
                                               (construct-context 6)))
                 '("     second>"
                   "name<first,"))))

;function chunks

(define/provide-test-suite test-function-declare
  (test-case
   "Test function-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (function-declare 'name 'return-type null)
                                               test-context))
                 '("inline return-type name(void)"))
   (check-equal? (write-nekot (chunk-transform (function-declare 'name 'return-type (list 'first))
                                               test-context))
                 '("inline return-type name(first)"))
   (check-equal? (write-nekot (chunk-transform (function-declare 'name 'return-type (list 'first 'second))
                                               test-context))
                 '("inline return-type name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (function-declare 'name 'return-type (list 'first 'second))
                                               (construct-context 8)))
                 '("     second)"
                   "name(first,"
                   "return-type"
                   "inline"))))

(define/provide-test-suite test-static-function-declare
  (test-case
   "Test static-function-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (static-function-declare 'name 'return-type null)
                                               test-context))
                 '("static inline return-type name(void)"))
   (check-equal? (write-nekot (chunk-transform (static-function-declare 'name 'return-type (list 'first))
                                               test-context))
                 '("static inline return-type name(first)"))
   (check-equal? (write-nekot (chunk-transform (static-function-declare 'name 'return-type (list 'first 'second))
                                               test-context))
                 '("static inline return-type name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (static-function-declare 'name 'return-type (list 'first 'second))
                                               (construct-context 20)))
                 '("                 second)"
                   "return-type name(first,"
                   "static inline"))))

(define/provide-test-suite test-void-function-declare
  (test-case
   "Test void-function-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (void-function-declare 'name null)
                                               test-context))
                 '("inline void name(void)"))
   (check-equal? (write-nekot (chunk-transform (void-function-declare 'name (list 'first))
                                               test-context))
                 '("inline void name(first)"))
   (check-equal? (write-nekot (chunk-transform (void-function-declare 'name (list 'first 'second))
                                               test-context))
                 '("inline void name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (void-function-declare 'name (list 'first 'second))
                                               (construct-context 20)))
                 '("                 second)"
                   "inline void name(first,"))))

(define/provide-test-suite test-function-define
  (test-case
   "Test function-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (function-define 'signature)
                                               test-context))
                 '("signature {}"))
   (check-equal? (write-nekot (chunk-transform (function-define 'signature 'first)
                                               test-context))
                 '("signature { first; }"))
   (check-equal? (write-nekot (chunk-transform (function-define 'signature 'first 'second)
                                               test-context))
                 '("signature { first; second; }"))
   (check-equal? (write-nekot (chunk-transform (function-define 'signature 'first 'second)
                                               (construct-context 20)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   "signature {"))))

(define/provide-test-suite test-void-function-define
  (test-case
   "Test void-function-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (void-function-define 'name null null)
                                               test-context))
                 '("inline void name(void) {}"))
   (check-equal? (write-nekot (chunk-transform (void-function-define 'name
                                                                     (list 'first)
                                                                     (list 'first))
                                               test-context))
                 '("inline void name(first) { first; }"))
   (check-equal? (write-nekot (chunk-transform (void-function-define 'name
                                                                     (list 'first 'second)
                                                                     (list 'first 'second))
                                               test-context))
                 '("inline void name(first, second) { first; second; }"))
   (check-equal? (write-nekot (chunk-transform (void-function-define 'name
                                                                     (list 'first 'second)
                                                                     (list 'first 'second))
                                               (construct-context 10)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   "          second) {"
                   "void name(first,"
                   "inline"))))

(define/provide-test-suite test-returning-function-define
  (test-case
   "Test returning-function-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type null)
                                                                          null
                                                                          'expr)
                                               test-context))
                 '("inline return-type name(void) { return expr; }"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type (list 'first))
                                                                          (list 'first)
                                                                          'expr)
                                               test-context))
                 '("inline return-type name(first) { first; return expr; }"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type (list 'first 'second))
                                                                          (list 'first 'second)
                                                                          'expr)
                                               test-context))
                 '("inline return-type name(first, second) { first; second; return expr; }"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type (list 'first 'second))
                                                                          (list 'first 'second)
                                                                          'expr)
                                               (construct-context 20)))
                 '("}"
                   "   return expr;"
                   ""
                   "   second;"
                   ""
                   "   first;"
                   "name(first, second) {"
                   "inline return-type"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type (list 'first 'second))
                                                                          (list 'first 'second)
                                                                          'expr)
                                               (construct-context 10)))
                 '("}"
                   "   return expr;"
                   ""
                   "   second;"
                   ""
                   "   first;"
                   "     second) {"
                   "name(first,"
                   "return-type"
                   "inline"))))

(define/provide-test-suite test-constructor-assignment
  (test-case
   "Test constructor-assignment"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment 'first 'second)
                                               test-context))
                 '("first(second)"))))

(define/provide-test-suite test-constructor
  (test-case
   "Test constructor"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (constructor 'name null null)
                                               test-context))
                 '("name() {}"))
   (check-equal? (write-nekot (chunk-transform (constructor 'name
                                                            (list 'first)
                                                            (list 'assign)
                                                            'asdf)
                                               test-context))
                 '("{ asdf; }"
                   "  : assign"
                   "name(first)"))
   (check-equal? (write-nekot (chunk-transform (constructor 'name
                                                            (list 'first 'second)
                                                            (list 'assign1 'assign2)
                                                            'asdf
                                                            'jkl)
                                               test-context))
                 '("{ asdf; jkl; }"
                   "  : assign1, assign2"
                   "name(first, second)"))))

;class/struct chunks

(define/provide-test-suite test-struct-declare
  (test-case
   "Test struct-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (struct-declare 'name)
                                               test-context))
                 '("struct name"))))

(define/provide-test-suite test-template-struct-declare
  (test-case
   "Test template-struct-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare 'name null null)
                                               test-context))
                 '(" struct name"
                   "template<>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare 'name
                                                                        (list 'first)
                                                                        (list 'first))
                                               test-context))
                 '(" struct name<first>"
                   "template<first>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare 'name
                                                                        (list 'first 'second)
                                                                        (list 'first 'second))
                                               test-context))
                 '(" struct name<first, second>"
                   "template<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare 'name
                                                                        (list 'first)
                                                                        (list 'first 'second))
                                               test-context))
                 '(" struct name<first, second>"
                   "template<first>"))))

(define/provide-test-suite test-section-define
  (test-case
   "Test section-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (section-define 'name 'first 'second)
                                               test-context))
                 '(" second"
                   ""
                   " first"
                   "name:"))))

(define/provide-test-suite test-struct-define
  (test-case
   "Test struct-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (struct-define 'signature 'first 'second)
                                               test-context))
                 '("signature { first second }"))
   (check-equal? (write-nekot (chunk-transform (struct-define 'signature 'first 'second)
                                               (construct-context 8)))
                 '("}"
                   "   second"
                   ""
                   "   first"
                   "signature {"))))

(define/provide-test-suite test-template-struct-define
  (test-case
   "Test template-struct-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-struct-define 'name
                                                                       (list 'first 'second)
                                                                       null
                                                                       'first
                                                                       'second)
                                               test-context))
                 '(" struct name { first second }"
                   "template<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-define 'name
                                                                       (list 'first 'second)
                                                                       null
                                                                       'first
                                                                       'second)
                                               (construct-context 12)))
                 '("}"
                   "   second"
                   ""
                   "   first"
                   " struct name {"
                   "         second>"
                   "template<first,"))))

(define/provide-test-suite test-scope-resolution-operator
  (test-case
   "Test scope-resolution-operator"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (scope-resolution-operator 'first 'second)
                                               test-context))
                 '("first::second"))))

;statement chunks

(define/provide-test-suite test-typedef-smt
  (test-case
   "Test typedef-smt"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (typedef-smt 'lhs 'rhs)
                                               test-context))
                 '("lhs typedef rhs"))))

(define/provide-test-suite test-function-call
  (test-case
   "Test function-call"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (function-call 'name)
                                               test-context))
                 '("name()"))
   (check-equal? (write-nekot (chunk-transform (function-call 'name 'first)
                                               test-context))
                 '("name(first)"))
   (check-equal? (write-nekot (chunk-transform (function-call 'name 'first 'second)
                                               test-context))
                 '("name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (function-call 'name 'first 'second)
                                               (construct-context 4)))
                 '("     second)"
                   "name(first,"))))

(define/provide-test-suite test-member-function-call
  (test-case
   "Test member-function-call"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (member-function-call 'obj 'name)
                                               test-context))
                 '("obj.name()"))
   (check-equal? (write-nekot (chunk-transform (member-function-call 'obj 'name 'first)
                                               test-context))
                 '("obj.name(first)"))
   (check-equal? (write-nekot (chunk-transform (member-function-call 'obj 'name 'first 'second)
                                               test-context))
                 '("obj.name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (member-function-call 'object 'name 'first 'second)
                                               (construct-context 4)))
                 '("            second)"
                   "object.name(first,"))))
