#lang racket

(require "private/core-chunk.rkt")

;fulmar standard chunks

;;;;;;;;;;;;;;;;;;;;;;
;core chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(provide flatten*)
(provide literal)
(provide spaces)
(provide new-line)
(provide empty)
(provide concat)
(provide immediate)
(provide speculative)
(provide position-indent)
(provide indent)
(provide comment-env-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;basic chunks;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;if empty
; returns then, if given is null (or a list that flattens to null)
; returns else, otherwise
(define (if-empty given then else)
  (if (empty? (flatten given))
      then
      else))
(provide if-empty)

;surround/before and after chunk
; adds surround before and after chunk
(define (surround surround chunk)
  (concat surround chunk surround))
(provide surround)

;blank lines chunk
; adds n blank lines
(define (blank-lines . lengths)
  (concat (make-list (combine-lengths 1 lengths)
                     new-line)))
(provide blank-lines)

;blank line chunk
; adds a blank line
(define blank-line (blank-lines 1))
(provide blank-line)

;surround parenthesis chunk
(define (sur-paren . chunks)
  (concat (immediate "(")
          chunks
          (immediate ")")))
(provide sur-paren)

;surround curly bracket chunk
(define (sur-crbr . chunks)
  (concat (immediate "{")
          chunks
          (immediate "}")))
(provide sur-crbr)

;surround angle bracket chunk
(define (sur-anbr . chunks)
  (concat (immediate "<")
          chunks
          (immediate ">")))
(provide sur-anbr)

;;;;;;;;;;;;;;;;;;;;;;
;list chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;attach a chunk to other chunks
; adds to-add immediately after each of the given chunks
; except: to-add is NOT added to the final chunk
(define (attach-list-separator to-attach . chunk-lists)
  (define chunks (flatten chunk-lists))
  (if (empty? chunks) ; can't use if-empty here - take will complain
      null
      (flatten* (map (λ (chunk) (concat chunk (immediate to-attach)))
                     (take chunks (- (length chunks) 1)))
                (last chunks))))
(provide attach-list-separator)

;insert a chunk between other chunks
; concatenates given chunks with add-between between given chunks
(define (between add-between-chunk . chunks)
  (concat (add-between (flatten chunks)
                       add-between-chunk)))
(provide between)

; combine between and attach functionality
;  adds to-add after each of the given chunks
;    and then adds add-between between new chunks
(define (between/attach to-attach add-between . chunks)
  (between add-between (attach-list-separator to-attach chunks)))
(provide between/attach)

;argument list chunk
; attempts to put chunks on a single line with a space between each chunk
; if that fails, puts chunks on their own lines
(define (arg-list sur attach . chunks)
  (define (build spacing)
    (between/attach attach spacing chunks))
  (sur (if-empty chunks
                 empty
                 (speculative (build 1)
                              length-equals-one
                              (position-indent (build new-line))))))
(provide arg-list)

;parenthesis argument list chunk
(define (paren-list . chunks)
  (arg-list sur-paren "," chunks))
(provide paren-list)

;template argument list chunk
(define (template-list . chunks)
  (arg-list sur-anbr "," chunks))
(provide template-list)

;body list chunk
(define (body-list attach . chunks)
  (arg-list sur-crbr attach chunks))
(provide body-list)

;list of chunks
; blank line added between each chunk
(define (top-list . chunks)
  (between blank-line chunks))
(provide top-list)

;list of statement chunks without final semi-colon
; adds spacing added between each chunk
;  and attaches a semi-colon to the end of each chunk (except last)
(define (internal-smt-list spacing . chunks)
  (between/attach ";" spacing chunks))
(provide internal-smt-list)

;list of statement chunks
; adds spacing added between each chunk
;  and attaches a semi-colon to the end of each chunk
(define (smt-list spacing . chunks)
  (if-empty chunks
            empty
            (concat (internal-smt-list spacing chunks)
                    (immediate ";"))))
(provide smt-list)

;constructor assignment list chunk
; each assignment is separated by a comma
; - first line is indented 2 spaces and begun with a colon
(define (constructor-assignment-list . chunks)
  (define (build spacing)
    (indent 2 (concat ":"
                      (immediate 1)
                      (position-indent (between/attach "," spacing chunks)))))
  (if-empty chunks
            empty
            (speculative (build 1)
                         length-equals-one
                         (build new-line))))
(provide constructor-assignment-list)

;general body chunk
; surrounds chunks with curly brackets
; - adds a semi-colon after each chunk, if use-semi-colons is true
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define (general-body use-semi-colons . chunks)
  (define (build start/end-spacing spacing)
    (surround start/end-spacing
              (if use-semi-colons
                  (smt-list spacing chunks)
                  (between spacing chunks))))
  (sur-crbr (if-empty chunks
                      empty
                      (speculative (build 1 1)
                                   length-equals-one
                                   (indent 3 (build new-line blank-line))))))
(provide general-body)

;body chunk
; surrounds chunks with curly brackets
; - adds a semi-colon after each chunk
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define (body . chunks)
  (general-body #true chunks))
(provide body)

;class body chunk
; surrounds chunks with curly brackets
; - does NOT add semi-colons after each chunk
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define (class-body . chunks)
  (general-body #false chunks))
(provide class-body)

;;;;;;;;;;;;;;;;;;;;;;
;preprocessor chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;preprocessor define chunk
; #define chunk
(define (pp-define name)
  (concat pp-directive 'define 1 name))
(provide pp-define)

;preprocessor include chunk
; #include<...> chunk
(define (pp-include included)
  (concat pp-directive 'include 1 (template-list included)))
(provide pp-include)

;alternate preprocessor include chunk
; #include<...> chunk
(define (pp-alt-include included)
  (concat pp-directive 'include 1 "\"" included "\""))
(provide pp-alt-include)

;multiple includes
(define (pp-includes . chunks)
  (between new-line (map pp-include (flatten chunks))))
(provide pp-includes)

;preprocessor if-not-defined chunk
(define (pp-ifdef condition)
  (concat pp-directive 'ifdef 1 condition))
(provide pp-ifdef)

;preprocessor if-not-defined chunk
(define (pp-ifndef condition)
  (concat pp-directive 'ifndef 1 condition))
(provide pp-ifndef)

;preprocessor if-not-defined chunk
(define pp-else (concat pp-directive 'else))
(provide pp-else)

;preprocessor endif chunk
(define (pp-endif condition)
  (concat pp-directive 'endif new-line (comment-env-chunk condition)))
(provide pp-endif)

;preprocessor conditional chunk
(define (pp-conditional directive condition then [else #false])
  (concat pp-directive
          directive
          1
          condition
          new-line
          (indent 3 then)
          new-line
          (if else
              (concat pp-else
                      new-line
                      (indent 3 else)
                      new-line)
              empty)
          (pp-endif condition)))
(provide pp-conditional)

;preprocessor conditional ifdef chunk
(define (pp-conditional-ifdef condition then [else #false])
  (pp-conditional 'ifdef condition then else))
(provide pp-conditional-ifdef)

;preprocessor conditional ifndef chunk
(define (pp-conditional-ifndef condition then [else #false])
  (pp-conditional 'ifndef condition then else))
(provide pp-conditional-ifndef)

;preprocessor h file wrapper chunk
(define (pp-header-file file-name . chunks)
  (pp-conditional-ifndef file-name
                         (concat (pp-define file-name)
                                 blank-line
                                 (top-list chunks)
                                 new-line)))
(provide pp-header-file)

;macro defintion chunk
; a macro definition
(define (macro-define name params chunk)
  (define macro-signature (concat (pp-define name)
                                  (if-empty params
                                            empty
                                            (paren-list params))))
  (speculative (concat macro-signature 1 chunk)
               length-equals-one
               (macro-env-chunk (concat macro-signature new-line (indent 3 chunk)))))
(provide macro-define)

;;;;;;;;;;;;;;;;;;;;;;
;general chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;namespace define chunk
(define (namespace-define name . chunks)
  (define chunk (concat 'namespace
                        (immediate 1)
                        (immediate name)
                        (immediate 1)
                        (body chunks)))
  (speculative chunk
               length-equals-one
               (concat chunk 1 (comment-env-chunk name))))
(provide namespace-define)

;described statements chunk
(define (described-smts comment . chunks)
  (concat (comment-env-chunk comment)
          new-line
          (between/attach ";" new-line chunks)))
(provide described-smts)

;make constant
(define (constize chunk)
  (concat chunk
          (immediate 1)
          (immediate 'const)))
(provide constize)

;;;;;;;;;;;;;;;;;;;;;;
;template chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;make given chunk a template with given template parameters
(define (template-define params chunk)
  (concat 'template
          (template-list params)
          new-line
          (indent 1 chunk)))
(provide template-define)

;make a use of a template
(define (template-use name . args)
  (concat name (if-empty args empty (template-list args))))
(provide template-use)

;;;;;;;;;;;;;;;;;;;;;;
;function chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;general function declaration
(define (general-function-declare name return-type . params)
  (concat return-type 1 name (paren-list (if-empty params
                                                   'void
                                                   params))))
(provide general-function-declare)

;function declaration
(define (function-declare name return-type . params)
  (concat 'inline 1 (general-function-declare name return-type params)))
(provide function-declare)

;static function declaration
(define (static-function-declare name return-type . params)
  (concat 'static 1 (function-declare name return-type params)))
(provide static-function-declare)

;void function declaration
(define (void-function-declare name params)
  (function-declare name 'void params))
(provide void-function-declare)

;function defintion
(define (function-define signature . chunks)
  (concat signature
          (immediate 1)
          (body chunks)))
(provide function-define)

;void function defintion
(define (void-function-define name params . body)
  (function-define (void-function-declare name params)
                   body))
(provide void-function-define)

;returning function defintion
(define (returning-function-define signature body return-expr)
  (function-define signature (flatten* body (concat 'return
                                                    (immediate 1)
                                                    (position-indent return-expr)))))
(provide returning-function-define)

;constructor assignment
(define (constructor-assignment var val)
  (concat var (sur-paren (concat val))))
(provide constructor-assignment)

;constructor defintion
(define (constructor name params assigns . chunks)
  (concat name 
          (paren-list params)
          (if-empty assigns
                    (immediate 1)
                    (surround new-line (constructor-assignment-list assigns)))
          (body chunks)))
(provide constructor)

;;;;;;;;;;;;;;;;;;;;;;
;class/struct chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;struct declaration
(define (struct-declare name)
  (concat 'struct 1 name))
(provide struct-declare)

;template struct declaration
(define (template-struct-declare name params . args)
  (template-define params (template-use (struct-declare name)
                                        args)))
(provide template-struct-declare)

;struct section
(define (section-define type . chunks)
  (if-empty chunks
            empty
            (concat type ":" new-line (indent 1 (between blank-line chunks)))))
(provide section-define)

;public section
(define (public-section . chunks)
  (section-define 'public chunks))
(provide public-section)

;private section
(define (private-section . chunks)
  (section-define 'private chunks))
(provide private-section)

;protected section
(define (protected-section . chunks)
  (section-define 'protected chunks))
(provide protected-section)

;struct definition
(define (struct-define signature . body)
  (concat signature
          (immediate 1)
          (class-body body)))
(provide struct-define)

;template struct definition
(define (template-struct-define name params args . body)
  (struct-define (template-struct-declare name params args)
                 body))
(provide template-struct-define)

;scope resolution operator
(define (scope-resolution-operator scope variable)
  (concat scope
          (immediate ":")
          (immediate ":")
          variable))
(provide scope-resolution-operator)

;;;;;;;;;;;;;;;;;;;;;;
;statement chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;typedef statement chunk
(define (typedef-smt lhs rhs)
  (concat lhs 1 'typedef 1 rhs))
(provide typedef-smt)

;function call
(define (function-call fcn . args)
  (concat fcn (paren-list args)))
(provide function-call)

;member function call
(define (member-function-call obj fcn . args)
  (concat obj
          (immediate ".")
          (position-indent (function-call fcn args))))
(provide member-function-call)
