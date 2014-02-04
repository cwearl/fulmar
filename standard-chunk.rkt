#lang racket

(require "private/core-chunk.rkt")
 
; from core-chunk
(provide
 flatten*
 literal 
 spaces  
 new-line
 empty
 concat
 immediate
 speculative
 position-indent
 indent
 comment-env-chunk)

(provide 
 (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;
;basic chunks;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;if empty
; returns then, if given is null (or a list that flattens to null)
; returns else, otherwise
(define-syntax if-empty
  (syntax-rules (flatten)
    [(if-empty given then else)
     (if (empty? (flatten given))
         then
         else)]))

;surround/before and after chunk
; adds surround before and after chunk
(define (surround surround chunk)
  (concat surround chunk surround))

;blank lines chunk
; adds n blank lines
(define (blank-lines . lengths)
  (concat (make-list (combine-lengths 1 lengths)
                     new-line)))

;blank line chunk
; adds a blank line
(define blank-line (blank-lines 1))

;surround parenthesis chunk
(define (sur-paren . chunks)
  (concat (immediate "(")
          chunks
          (immediate ")")))

;surround curly bracket chunk
(define (sur-crbr . chunks)
  (concat (immediate "{")
          chunks
          (immediate "}")))

;surround angle bracket chunk
(define (sur-anbr . chunks)
  (concat (immediate "<")
          chunks
          (immediate ">")))

;;;;;;;;;;;;;;;;;;;;;;
;list chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;attach a chunk to other chunks
; adds to-add immediately after each of the given chunks
; except: to-add is NOT added to the final chunk
(define (attach-list-separator to-attach . chunk-lists)
  (define chunks (flatten chunk-lists))
  (if-empty chunks
            null
            (flatten* (map (λ (chunk) (concat chunk (immediate to-attach)))
                           (take chunks (- (length chunks) 1)))
                      (last chunks))))

;insert a chunk between other chunks
; concatenates given chunks with add-between between given chunks
(define (between add-between-chunk . chunks)
  (concat (add-between (flatten chunks)
                       add-between-chunk)))

; combine between and attach functionality
;  adds to-add after each of the given chunks
;    and then adds add-between between new chunks
(define (between/attach to-attach add-between . chunks)
  (between add-between (attach-list-separator to-attach chunks)))

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

;parenthesis argument list chunk
(define (paren-list . chunks)
  (arg-list sur-paren "," chunks))

;template argument list chunk
(define (template-list . chunks)
  (arg-list sur-anbr "," chunks))

;body list chunk
(define (body-list attach . chunks)
  (arg-list sur-crbr attach chunks))

;list of chunks
; blank line added between each chunk
(define (top-list . chunks)
  (between blank-line chunks))

;list of statement chunks without final semi-colon
; adds spacing added between each chunk
;  and attaches a semi-colon to the end of each chunk (except last)
(define (internal-smt-list spacing . chunks)
  (between/attach ";" spacing chunks))

;list of statement chunks
; adds spacing added between each chunk
;  and attaches a semi-colon to the end of each chunk
(define (smt-list spacing . chunks)
  (if-empty chunks
            empty
            (concat (internal-smt-list spacing chunks)
                    (immediate ";"))))

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
                      (if (= 1 (length chunks))
                          (speculative 
                           (build 1 1)
                           length-equals-one
                           (indent 3 (build new-line blank-line)))
                          (indent 3 (build new-line blank-line))))))

;body chunk
; surrounds chunks with curly brackets
; - adds a semi-colon after each chunk
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define (body . chunks)
  (general-body #true chunks))

;class body chunk
; surrounds chunks with curly brackets
; - does NOT add semi-colons after each chunk
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define (class-body . chunks)
  (general-body #false chunks))

;;;;;;;;;;;;;;;;;;;;;;
;preprocessor chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;preprocessor define chunk
; #define chunk
(define (pp-define name)
  (concat pp-directive 'define 1 name))

;preprocessor include chunk
; #include<...> chunk
(define (pp-include included)
  (concat pp-directive 'include 1 (template-list included)))

;alternate preprocessor include chunk
; #include<...> chunk
(define (pp-alt-include included)
  (concat pp-directive 'include 1 "\"" included "\""))

;multiple includes
(define (pp-includes . chunks)
  (between new-line (map pp-include (flatten chunks))))

;preprocessor if-not-defined chunk
(define (pp-ifdef condition)
  (concat pp-directive 'ifdef 1 condition))

;preprocessor if-not-defined chunk
(define (pp-ifndef condition)
  (concat pp-directive 'ifndef 1 condition))

;preprocessor if-not-defined chunk
(define pp-else (concat pp-directive 'else))

;preprocessor endif chunk
(define (pp-endif condition)
  (concat pp-directive 'endif new-line (comment-env-chunk condition)))

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

;preprocessor conditional ifdef chunk
(define (pp-conditional-ifdef condition then [else #false])
  (pp-conditional 'ifdef condition then else))

;preprocessor conditional ifndef chunk
(define (pp-conditional-ifndef condition then [else #false])
  (pp-conditional 'ifndef condition then else))

;preprocessor h file wrapper chunk
(define (pp-header-file file-name . chunks)
  (pp-conditional-ifndef file-name
                         (concat (pp-define file-name)
                                 blank-line
                                 (top-list chunks)
                                 new-line)))

;macro defintion chunk
; a macro definition
(define (macro-define name params chunk)
  (immediate (concat (pp-define name) 1 chunk)))

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
  
  (concat chunk 1 (comment-env-chunk name)))

;described statements chunk
(define (described-smts comment . chunks)
  (concat (comment-env-chunk comment)
          new-line
          (between/attach ";" new-line chunks)))

;make constant
(define (constize chunk)
  (concat chunk
          (immediate 1)
          (immediate 'const)))

;;;;;;;;;;;;;;;;;;;;;;
;template chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;make given chunk a template with given template parameters
(define (template-define params chunk)
  (concat 'template
          (template-list params)
          new-line
          (indent 1 chunk)))

;make a use of a template
(define (template-use name . args)
  (concat name (if-empty args empty (template-list args))))

;;;;;;;;;;;;;;;;;;;;;;
;function chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;general function declaration
(define (general-function-declare name return-type . params)
  (concat return-type 1 name (paren-list (if-empty params
                                                   'void
                                                   params))))

;function declaration
(define (function-declare name return-type . params)
  (concat 'inline 1 (general-function-declare name return-type params)))

;static function declaration
(define (static-function-declare name return-type . params)
  (concat 'static 1 (function-declare name return-type params)))

;void function declaration
(define (void-function-declare name params)
  (function-declare name 'void params))

;function defintion
(define (function-define signature . chunks)
  (concat signature
          (immediate 1)
          (body chunks)))

;void function defintion
(define (void-function-define name params . body)
  (function-define (void-function-declare name params)
                   body))

;returning function defintion
(define (returning-function-define signature body return-expr)
  (function-define signature (flatten* body (concat 'return
                                                    (immediate 1)
                                                    (position-indent return-expr)))))

;constructor assignment
(define (constructor-assignment var val)
  (concat var (sur-paren (concat val))))

;constructor defintion
(define (constructor name params assigns . chunks)
  (concat name 
          (paren-list params)
          (if-empty assigns
                    (immediate 1)
                    (surround new-line (constructor-assignment-list assigns)))
          (body chunks)))

;;;;;;;;;;;;;;;;;;;;;;
;class/struct chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;struct declaration
(define (struct-declare name)
  (concat 'struct 1 name))

;template struct declaration
(define (template-struct-declare name params . args)
  (template-define params (template-use (struct-declare name)
                                        args)))

;struct section
(define (section-define type . chunks)
  (if-empty chunks
            empty
            (concat type ":" new-line (indent 1 (between blank-line chunks)))))

;public section
(define (public-section . chunks)
  (section-define 'public chunks))

;private section
(define (private-section . chunks)
  (section-define 'private chunks))

;protected section
(define (protected-section . chunks)
  (section-define 'protected chunks))

;struct definition
(define (struct-define signature . body)
  (concat signature
          (immediate 1)
          (class-body body)))

;template struct definition
(define (template-struct-define name params args . body)
  (struct-define (template-struct-declare name params args)
                 body))

;scope resolution operator
(define (scope-resolution-operator scope variable)
  (concat scope
          (immediate ":")
          (immediate ":")
          variable))

;;;;;;;;;;;;;;;;;;;;;;
;statement chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;typedef statement chunk
(define (typedef-smt lhs rhs)
  (concat lhs 1 'typedef 1 rhs))

;function call
(define (function-call fcn . args)
  (concat fcn (paren-list args)))

;member function call
(define (member-function-call obj fcn . args)
  (concat obj
          (immediate ".")
          (position-indent (function-call fcn args))))
