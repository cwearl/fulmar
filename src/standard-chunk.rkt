#lang racket

(require "core-chunk.rkt")

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
;character chunks;;;;;
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

;space chunk
; adds a space
(define space (spaces 1))
(provide space)

;immediate space chunk
; adds a space immediately
(define imm-space (immediate space))
(provide imm-space)

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

;open parenthesis chunk
; adds "("
(define open-paren "(")
(provide open-paren)

;immediate open parenthesis chunk
; adds "(" immediately
(define imm-open-paren (immediate open-paren))
(provide imm-open-paren)

;close parenthesis chunk
; adds ")"
(define close-paren ")")
(provide close-paren)

;immediate close parenthesis chunk
; adds ")" immediately
(define imm-close-paren (immediate close-paren))
(provide imm-close-paren)

;surround parenthesis chunk
(define (sur-paren . chunks)
  (concat imm-open-paren chunks imm-close-paren))
(provide sur-paren)

;open curly bracket chunk
; adds "{"
(define open-crbr "{")
(provide open-crbr)

;immediate open curly bracket chunk
; adds "{" immediately
(define imm-open-crbr (immediate open-crbr))
(provide imm-open-crbr)

;close curly bracket chunk
; adds "}"
(define close-crbr "}")
(provide close-crbr)

;immediate close curly bracket chunk
; adds "}" immediately
(define imm-close-crbr (immediate close-crbr))
(provide imm-close-crbr)

;surround curly bracket chunk
(define (sur-crbr . chunks)
  (concat imm-open-crbr chunks imm-close-crbr))
(provide sur-crbr)

;open angle-bracket chunk
; adds "<"
(define open-anbr "<")
(provide open-anbr)

;immediate open angle-bracket chunk
; adds "<" immediately
(define imm-open-anbr (immediate open-anbr))
(provide imm-open-anbr)

;close angle-bracket chunk
; adds ">"
(define close-anbr ">")
(provide close-anbr)

;immediate close angle-bracket chunk
; adds ">" immediately
(define imm-close-anbr (immediate close-anbr))
(provide imm-close-anbr)

;surround angle bracket chunk
(define (sur-anbr . chunks)
  (concat imm-open-anbr chunks imm-close-anbr))
(provide sur-anbr)

;comma chunk
; adds ","
(define comma ",")
(provide comma)

;immediate comma chunk
; adds "," immediately
(define imm-comma (immediate comma))
(provide imm-comma)

;period chunk
; adds "."
(define period ".")
(provide period)

;immediate period chunk
; adds "," immediately
(define imm-period (immediate period))
(provide imm-period)

;colon chunk
; adds ":"
(define colon ":")
(provide colon)

;immediate colon chunk
; adds "," immediately
(define imm-colon (immediate colon))
(provide imm-colon)

;semi-colon chunk
; adds ";"
(define semi-colon ";")
(provide semi-colon)

;immediate semi-colon chunk
; adds ";" immediately
(define imm-semi-colon (immediate semi-colon))
(provide imm-semi-colon)

;;;;;;;;;;;;;;;;;;;;;;
;keyword chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;define chunk
(define define-chunk 'define)
(provide define-chunk)

;immediate define chunk
; adds "define" immediately
(define imm-define (immediate define-chunk))
(provide imm-define)

;include chunk
(define include 'include)
(provide include)

;immediate include chunk
; adds "include" immediately
(define imm-include (immediate include))
(provide imm-include)

;ifdef chunk
(define ifdef 'ifdef)
(provide ifdef)

;immediate ifdef chunk
; adds "ifdef" immediately
(define imm-ifdef (immediate ifdef))
(provide imm-ifdef)

;ifndef chunk
(define ifndef 'ifndef)
(provide ifndef)

;immediate ifndef chunk
; adds "ifndef" immediately
(define imm-ifndef (immediate ifndef))
(provide imm-ifndef)

;else chunk
(define else 'else)
(provide else)

;immediate else chunk
; adds "else" immediately
(define imm-else (immediate else))
(provide imm-else)

;endif chunk
(define endif 'endif)
(provide endif)

;immediate endif chunk
; adds "endif" immediately
(define imm-endif (immediate endif))
(provide imm-endif)

;template chunk
(define template 'template)
(provide template)

;immediate template chunk
; adds "template" immediately
(define imm-template (immediate template))
(provide imm-template)

;typename chunk
(define typename 'typename)
(provide typename)

;immediate typename chunk
; adds "typename" immediately
(define imm-typename (immediate typename))
(provide imm-typename)

;typedef chunk
(define typedef 'typedef)
(provide typedef)

;immediate typedef chunk
; adds "typedef" immediately
(define imm-typedef (immediate typedef))
(provide imm-typedef)

;void chunk
(define void 'void)
(provide void)

;immediate void chunk
; adds "void" immediately
(define imm-void (immediate void))
(provide imm-void)

;inline chunk
(define inline 'inline)
(provide inline)

;immediate inline chunk
; adds "inline" immediately
(define imm-inline (immediate inline))
(provide imm-inline)

;static chunk
(define static 'static)
(provide static)

;immediate static chunk
; adds "static" immediately
(define imm-static (immediate static))
(provide imm-static)

;return chunk
(define return 'return)
(provide return)

;immediate return chunk
; adds "return" immediately
(define imm-return (immediate return))
(provide imm-return)

;const chunk
(define const 'const)
(provide const)

;immediate const chunk
; adds "const" immediately
(define imm-const (immediate const))
(provide imm-const)

;struct chunk
(define struct 'struct)
(provide struct)

;immediate struct chunk
; adds "struct" immediately
(define imm-struct (immediate struct))
(provide imm-struct)

;class chunk
(define class 'class)
(provide class)

;immediate class chunk
; adds "class" immediately
(define imm-class (immediate class))
(provide imm-class)

;public chunk
(define public 'public)
(provide public)

;immediate public chunk
; adds "public" immediately
(define imm-public (immediate public))
(provide imm-public)

;protected chunk
(define protected 'protected)
(provide protected)

;immediate protected chunk
; adds "protected" immediately
(define imm-protected (immediate protected))
(provide imm-protected)

;private chunk
(define private 'private)
(provide private)

;immediate private chunk
; adds "private" immediately
(define imm-private (immediate private))
(provide imm-private)

;namespace chunk
(define namespace 'namespace)
(provide namespace)

;immediate namespace chunk
; adds "namespace" immediately
(define imm-namespace (immediate namespace))
(provide imm-namespace)

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
                 (speculative (build space)
                              length-equals-one
                              (position-indent (build new-line))))))
(provide arg-list)

;parenthesis argument list chunk
(define (paren-list . chunks)
  (arg-list sur-paren comma chunks))
(provide paren-list)

;template argument list chunk
(define (template-list . chunks)
  (arg-list sur-anbr comma chunks))
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
  (between/attach semi-colon spacing chunks))
(provide internal-smt-list)

;list of statement chunks
; adds spacing added between each chunk
;  and attaches a semi-colon to the end of each chunk
(define (smt-list spacing . chunks)
  (if-empty chunks
            empty
            (concat (internal-smt-list spacing chunks)
                    imm-semi-colon)))
(provide smt-list)

;constructor assignment list chunk
; each assignment is separated by a comma
; - first line is indented 2 spaces and begun with a colon
(define (constructor-assignment-list . chunks)
  (define (build spacing)
    (indent 2 (concat colon imm-space (position-indent (between/attach comma spacing chunks)))))
  (if-empty chunks
            empty
            (speculative (build space)
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
                      (speculative (build space space)
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
  (concat pp-directive define-chunk space name))
(provide pp-define)

;preprocessor include chunk
; #include<...> chunk
(define (pp-include included)
  (concat pp-directive include space (template-list included)))
(provide pp-include)

;alternate preprocessor include chunk
; #include<...> chunk
(define (pp-alt-include included)
  (concat pp-directive include space "\"" included "\""))
(provide pp-alt-include)

;multiple includes
(define (pp-includes . chunks)
  (between new-line (map pp-include (flatten chunks))))
(provide pp-includes)

;preprocessor if-not-defined chunk
(define (pp-ifdef condition)
  (concat pp-directive ifdef space condition))
(provide pp-ifdef)

;preprocessor if-not-defined chunk
(define (pp-ifndef condition)
  (concat pp-directive ifndef space condition))
(provide pp-ifndef)

;preprocessor if-not-defined chunk
(define pp-else (concat pp-directive else))
(provide pp-else)

;preprocessor endif chunk
(define (pp-endif condition)
  (concat pp-directive endif new-line (comment-env-chunk condition)))
(provide pp-endif)

;preprocessor conditional chunk
(define (pp-conditional directive condition then [else #false])
  (concat pp-directive
          directive
          space
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
  (pp-conditional ifdef condition then else))
(provide pp-conditional-ifdef)

;preprocessor conditional ifndef chunk
(define (pp-conditional-ifndef condition then [else #false])
  (pp-conditional ifndef condition then else))
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
  (speculative (concat macro-signature space chunk)
               length-equals-one
               (macro-env-chunk (concat macro-signature new-line (indent 3 chunk)))))
(provide macro-define)

;;;;;;;;;;;;;;;;;;;;;;
;general chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;namespace define chunk
(define (namespace-define name . chunks)
  (define chunk (concat imm-namespace
                        imm-space
                        (immediate name)
                        imm-space
                        (body chunks)))
  (speculative chunk
               length-equals-one
               (concat chunk space (comment-env-chunk name))))
(provide namespace-define)

;described statements chunk
(define (described-smts comment . chunks)
  (concat (comment-env-chunk comment)
          new-line
          (between/attach semi-colon new-line chunks)))
(provide described-smts)

;make constant
(define (constize chunk)
  (concat chunk imm-space imm-const))
(provide constize)

;;;;;;;;;;;;;;;;;;;;;;
;template chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;make given chunk a template with given template parameters
(define (template-define params chunk)
  (concat template
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
  (concat return-type space name (paren-list (if-empty params
                                                       imm-void
                                                       params))))
(provide general-function-declare)

;function declaration
(define (function-declare name return-type . params)
  (concat imm-inline space (general-function-declare name return-type params)))
(provide function-declare)

;static function declaration
(define (static-function-declare name return-type . params)
  (concat imm-static imm-space (function-declare name return-type params)))
(provide static-function-declare)

;void function declaration
(define (void-function-declare name params)
  (function-declare name void params))
(provide void-function-declare)

;function defintion
(define (function-define signature . chunks)
  (concat signature imm-space (body chunks)))
(provide function-define)

;void function defintion
(define (void-function-define name params . body)
  (function-define (void-function-declare name params)
                   body))
(provide void-function-define)

;returning function defintion
(define (returning-function-define signature body return-expr)
  (function-define signature (flatten* body (concat return imm-space (position-indent return-expr)))))
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
                    imm-space
                    (surround new-line (constructor-assignment-list assigns)))
          (body chunks)))
(provide constructor)

;;;;;;;;;;;;;;;;;;;;;;
;class/struct chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;struct declaration
(define (struct-declare name)
  (concat struct space name))
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
            (concat type colon new-line (indent 1 (between blank-line chunks)))))
(provide section-define)

;public section
(define (public-section . chunks)
  (section-define public chunks))
(provide public-section)

;private section
(define (private-section . chunks)
  (section-define private chunks))
(provide private-section)

;protected section
(define (protected-section . chunks)
  (section-define protected chunks))
(provide protected-section)

;struct definition
(define (struct-define signature . body)
  (concat signature imm-space (class-body body)))
(provide struct-define)

;template struct definition
(define (template-struct-define name params args . body)
  (struct-define (template-struct-declare name params args)
                 body))
(provide template-struct-define)

;scope resolution operator
(define (scope-resolution-operator scope variable)
  (concat scope imm-colon imm-colon variable))
(provide scope-resolution-operator)

;;;;;;;;;;;;;;;;;;;;;;
;statement chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;typedef statement chunk
(define (typedef-smt lhs rhs)
  (concat lhs space typedef space rhs))
(provide typedef-smt)

;function call
(define (function-call fcn . args)
  (concat fcn (paren-list args)))
(provide function-call)

;member function call
(define (member-function-call obj fcn . args)
  (concat obj imm-period (position-indent (function-call fcn args))))
(provide member-function-call)
