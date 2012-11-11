#lang racket

(require "debug.rkt")
(require "core-chunk.rkt")

;fulmar standard chunks

;;;;;;;;;;;;;;;;;;;;;;
;core chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(provide flatten*)
(provide literal-chunk)
(provide spaces-chunk)
(provide new-line-chunk)
(provide empty-chunk)
(provide concat-chunk)
(provide immediate-chunk)
(provide speculative-chunk)
(provide position-indent-chunk)
(provide indent-chunk)
(provide comment-env-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;character chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;space chunk
; adds a space
(define/debug space-chunk
  chunk/c
  (spaces-chunk 1))
(provide space-chunk)

;immediate space chunk
; adds a space immediately
(define/debug imm-space-chunk
  chunk/c
  (immediate-chunk space-chunk))
(provide imm-space-chunk)

;blank lines chunk
; adds n blank lines
(define/debug (blank-lines-chunk . lengths)
  (->* () #:rest (non-empty-listof exact-positive-integer?) chunk/c)
  (concat-chunk (make-list (combine-lengths 1
                                            lengths)
                           new-line-chunk)))
(provide blank-lines-chunk)

;blank line chunk
; adds a blank line
(define/debug blank-line-chunk
  chunk/c
  (blank-lines-chunk 1))
(provide blank-line-chunk)

;open parenthesis chunk
; adds "("
(define/debug open-paren-chunk
  chunk/c
  "(")
(provide open-paren-chunk)

;immediate open parenthesis chunk
; adds "(" immediately
(define/debug imm-open-paren-chunk
  chunk/c
  (immediate-chunk open-paren-chunk))
(provide imm-open-paren-chunk)

;close parenthesis chunk
; adds ")"
(define/debug close-paren-chunk
  chunk/c
  ")")
(provide close-paren-chunk)

;immediate close parenthesis chunk
; adds ")" immediately
(define/debug imm-close-paren-chunk
  chunk/c
  (immediate-chunk close-paren-chunk))
(provide imm-close-paren-chunk)

;open curly bracket chunk
; adds "{"
(define/debug open-crbr-chunk
  chunk/c
  "{")
(provide open-crbr-chunk)

;immediate open curly bracket chunk
; adds "{" immediately
(define/debug imm-open-crbr-chunk
  chunk/c
  (immediate-chunk open-crbr-chunk))
(provide imm-open-crbr-chunk)

;close curly bracket chunk
; adds "}"
(define/debug close-crbr-chunk
  chunk/c
  "}")
(provide close-crbr-chunk)

;immediate close curly bracket chunk
; adds "}" immediately
(define/debug imm-close-crbr-chunk
  chunk/c
  (immediate-chunk close-crbr-chunk))
(provide imm-close-crbr-chunk)

;open angle-bracket chunk
; adds "<"
(define/debug open-anbr-chunk
  chunk/c
  "<")
(provide open-anbr-chunk)

;immediate open angle-bracket chunk
; adds "<" immediately
(define/debug imm-open-anbr-chunk
  chunk/c
  (immediate-chunk open-anbr-chunk))
(provide imm-open-anbr-chunk)

;close angle-bracket chunk
; adds ">"
(define/debug close-anbr-chunk
  chunk/c
  ">")
(provide close-anbr-chunk)

;immediate close angle-bracket chunk
; adds ">" immediately
(define/debug imm-close-anbr-chunk
  chunk/c
  (immediate-chunk close-anbr-chunk))
(provide imm-close-anbr-chunk)

;comma chunk
; adds ","
(define/debug comma-chunk
  chunk/c
  ",")
(provide comma-chunk)

;immediate comma chunk
; adds "," immediately
(define/debug imm-comma-chunk
  chunk/c
  (immediate-chunk comma-chunk))
(provide imm-comma-chunk)

;period chunk
; adds "."
(define/debug period-chunk
  chunk/c
  ".")
(provide period-chunk)

;immediate period chunk
; adds "," immediately
(define/debug imm-period-chunk
  chunk/c
  (immediate-chunk period-chunk))
(provide imm-period-chunk)

;colon chunk
; adds ":"
(define/debug colon-chunk
  chunk/c
  ":")
(provide colon-chunk)

;immediate colon chunk
; adds "," immediately
(define/debug imm-colon-chunk
  chunk/c
  (immediate-chunk colon-chunk))
(provide imm-colon-chunk)

;semi-colon chunk
; adds ";"
(define/debug semi-colon-chunk
  chunk/c
  ";")
(provide semi-colon-chunk)

;immediate semi-colon chunk
; adds ";" immediately
(define/debug imm-semi-colon-chunk
  chunk/c
  (immediate-chunk semi-colon-chunk))
(provide imm-semi-colon-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;keyword chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;define chunk
(define/debug define-chunk
  chunk/c
  'define)
(provide define-chunk)

;immediate define chunk
; adds "define" immediately
(define/debug imm-define-chunk
  chunk/c
  (immediate-chunk define-chunk))
(provide imm-define-chunk)

;include chunk
(define/debug include-chunk
  chunk/c
  'include)
(provide include-chunk)

;immediate include chunk
; adds "include" immediately
(define/debug imm-include-chunk
  chunk/c
  (immediate-chunk include-chunk))
(provide imm-include-chunk)

;ifdef chunk
(define/debug ifdef-chunk
  chunk/c
  'ifdef)
(provide ifdef-chunk)

;immediate ifdef chunk
; adds "ifdef" immediately
(define/debug imm-ifdef-chunk
  chunk/c
  (immediate-chunk ifdef-chunk))
(provide imm-ifdef-chunk)

;ifndef chunk
(define/debug ifndef-chunk
  chunk/c
  'ifndef)
(provide ifndef-chunk)

;immediate ifndef chunk
; adds "ifndef" immediately
(define/debug imm-ifndef-chunk
  chunk/c
  (immediate-chunk ifndef-chunk))
(provide imm-ifndef-chunk)

;else chunk
(define/debug else-chunk
  chunk/c
  'else)
(provide else-chunk)

;immediate else chunk
; adds "else" immediately
(define/debug imm-else-chunk
  chunk/c
  (immediate-chunk else-chunk))
(provide imm-else-chunk)

;endif chunk
(define/debug endif-chunk
  chunk/c
  'endif)
(provide endif-chunk)

;immediate endif chunk
; adds "endif" immediately
(define/debug imm-endif-chunk
  chunk/c
  (immediate-chunk endif-chunk))
(provide imm-endif-chunk)

;template chunk
(define/debug template-chunk
  chunk/c
  'template)
(provide template-chunk)

;immediate template chunk
; adds "template" immediately
(define/debug imm-template-chunk
  chunk/c
  (immediate-chunk template-chunk))
(provide imm-template-chunk)

;typename chunk
(define/debug typename-chunk
  chunk/c
  'typename)
(provide typename-chunk)

;immediate typename chunk
; adds "typename" immediately
(define/debug imm-typename-chunk
  chunk/c
  (immediate-chunk typename-chunk))
(provide imm-typename-chunk)

;typedef chunk
(define/debug typedef-chunk
  chunk/c
  'typedef)
(provide typedef-chunk)

;immediate typedef chunk
; adds "typedef" immediately
(define/debug imm-typedef-chunk
  chunk/c
  (immediate-chunk typedef-chunk))
(provide imm-typedef-chunk)

;void chunk
(define/debug void-chunk
  chunk/c
  'void)
(provide void-chunk)

;immediate void chunk
; adds "void" immediately
(define/debug imm-void-chunk
  chunk/c
  (immediate-chunk void-chunk))
(provide imm-void-chunk)

;inline chunk
(define/debug inline-chunk
  chunk/c
  'inline)
(provide inline-chunk)

;immediate inline chunk
; adds "inline" immediately
(define/debug imm-inline-chunk
  chunk/c
  (immediate-chunk inline-chunk))
(provide imm-inline-chunk)

(define/debug static-chunk
  chunk/c
  'static)
(provide static-chunk)

;immediate static chunk
; adds "static" immediately
(define/debug imm-static-chunk
  chunk/c
  (immediate-chunk static-chunk))
(provide imm-static-chunk)

;return chunk
(define/debug return-chunk
  chunk/c
  'return)
(provide return-chunk)

;immediate return chunk
; adds "return" immediately
(define/debug imm-return-chunk
  chunk/c
  (immediate-chunk return-chunk))
(provide imm-return-chunk)

;const chunk
(define/debug const-chunk
  chunk/c
  'const)
(provide const-chunk)

;immediate const chunk
; adds "const" immediately
(define/debug imm-const-chunk
  chunk/c
  (immediate-chunk const-chunk))
(provide imm-const-chunk)

;struct chunk
(define/debug struct-chunk
  chunk/c
  'struct)
(provide struct-chunk)

;immediate struct chunk
; adds "struct" immediately
(define/debug imm-struct-chunk
  chunk/c
  (immediate-chunk struct-chunk))
(provide imm-struct-chunk)

;class chunk
(define/debug class-chunk
  chunk/c
  'class)
(provide class-chunk)

;immediate class chunk
; adds "class" immediately
(define/debug imm-class-chunk
  chunk/c
  (immediate-chunk class-chunk))
(provide imm-class-chunk)

;public chunk
(define/debug public-chunk
  chunk/c
  'public)
(provide public-chunk)

;immediate public chunk
; adds "public" immediately
(define/debug imm-public-chunk
  chunk/c
  (immediate-chunk public-chunk))
(provide imm-public-chunk)

;protected chunk
(define/debug protected-chunk
  chunk/c
  'protected)
(provide protected-chunk)

;immediate protected chunk
; adds "protected" immediately
(define/debug imm-protected-chunk
  chunk/c
  (immediate-chunk protected-chunk))
(provide imm-protected-chunk)

;private chunk
(define/debug private-chunk
  chunk/c
  'private)
(provide private-chunk)

;immediate private chunk
; adds "private" immediately
(define/debug imm-private-chunk
  chunk/c
  (immediate-chunk private-chunk))
(provide imm-private-chunk)

;namespace chunk
(define/debug namespace-chunk
  chunk/c
  'namespace)
(provide namespace-chunk)

;immediate namespace chunk
; adds "namespace" immediately
(define/debug imm-namespace-chunk
  chunk/c
  (immediate-chunk namespace-chunk))
(provide imm-namespace-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;list chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;attach a chunk to other chunks
; adds to-add-chunk immediately after each of the given chunks
; except: to-add-chunk is NOT added to the final chunk
(define/debug (attach-list-separator to-attach-chunk . chunk-lists)
  (->* (chunk/c) #:rest nullable-chunk-list/c nullable-chunk-list/c)
  (let ([chunks (flatten chunk-lists)])
    (if (empty? chunks)
        null
        (flatten* (map (λ (chunk) (concat-chunk chunk (immediate-chunk to-attach-chunk)))
                       (take chunks (- (length chunks) 1)))
                  (last chunks)))))
(provide attach-list-separator)

;insert a chunk between other chunks
; concatenates given chunks with add-between-chunk between given chunks
(define/debug (between-chunk add-between-chunk . chunk-lists)
  (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (let ([chunks (flatten chunk-lists)])
    (if (empty? chunks)
        empty-chunk
        (concat-chunk (add-between chunks
                                   add-between-chunk)))))
(provide between-chunk)

; combine between and attach functionality
;  adds to-add-chunk after each of the given chunks
;    and then adds add-between-chunk between new chunks
(define/debug (between/attach-chunk to-attach-chunk add-between-chunk . chunks)
  (->* (chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (between-chunk add-between-chunk
                 (attach-list-separator to-attach-chunk
                                        chunks)))
(provide between/attach-chunk)

;argument list chunk
; attempts to put chunks on a single line with a space between each chunk
; if that fails, puts chunks on their own lines
(define/debug (arg-list-chunk open-chunk attach-chunk close-chunk . chunk-lists)
  (->* (chunk/c chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (let ([chunks (flatten chunk-lists)])
    (concat-chunk (immediate-chunk open-chunk)
                  (cond [;no parameters
                         (empty? chunks)
                         empty-chunk]
                        [;one parameter - no commas
                         (= 1 (length chunks))
                         (position-indent-chunk (first chunks))]
                        [;more than one parameter - commas and possibly more than one line
                         else
                         (speculative-chunk (between/attach-chunk attach-chunk
                                                                  space-chunk
                                                                  chunks)
                                            length-equals-one
                                            (position-indent-chunk (between/attach-chunk attach-chunk
                                                                                         new-line-chunk
                                                                                         chunks)))])
                (immediate-chunk close-chunk))))
(provide arg-list-chunk)

;parenthesis argument list chunk
(define/debug (paren-list-chunk . chunks)
  (->* () #:rest nullable-chunk-list/c chunk/c)
  (arg-list-chunk open-paren-chunk
                  comma-chunk
                  close-paren-chunk
                  chunks))
(provide paren-list-chunk)

;template argument list chunk
(define/debug (template-list-chunk . chunks)
  (->* () #:rest nullable-chunk-list/c chunk/c)
  (arg-list-chunk open-anbr-chunk
                  comma-chunk
                  close-anbr-chunk
                  chunks))
(provide template-list-chunk)

;statement line list of chunks
; each chunk is expanded on its own line
; - each chunk put on it's own line
(define/debug (smt-list-chunk spacing-chunk . chunks)
  (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (between/attach-chunk semi-colon-chunk
                        spacing-chunk
                        chunks))
(provide smt-list-chunk)

;statement line list of chunks with last semi-colon
; each chunk is expanded on its own line
; - each chunk put on it's own line
(define/debug (top-smt-list-chunk spacing-chunk . chunks)
  (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (if (empty? (flatten chunks))
      empty-chunk
      (concat-chunk (smt-list-chunk spacing-chunk
                                    chunks)
                    imm-semi-colon-chunk)))
(provide top-smt-list-chunk)

;constructor assignment list chunk
; each assignment is separated by a comma
; - first line is indented 2 spaces and begun with a colon
(define/debug (constructor-assignment-list-chunk . assignment-lists)
  (->* () #:rest nullable-chunk-list/c chunk/c)
  (let* ([assigns (flatten assignment-lists)]
         [build (λ (spacing-chunk)
                  (indent-chunk 2
                                (concat-chunk colon-chunk
                                              imm-space-chunk
                                              (position-indent-chunk (between/attach-chunk comma-chunk
                                                                                           spacing-chunk
                                                                                           assigns)))))])
    (if (empty? assigns)
        empty-chunk
        (speculative-chunk (build space-chunk)
                           length-equals-one
                           (build new-line-chunk)))))
(provide constructor-assignment-list-chunk)

;body chunk
; surrounds chunks with curly brackets
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define/debug (body-chunk . chunks)
  (->* () #:rest nullable-chunk-list/c chunk/c)
  (let ([build (λ (spacing-chunk)
                 (top-smt-list-chunk spacing-chunk
                                     chunks))])
    (concat-chunk imm-open-crbr-chunk ; open body
                  (if (empty? (flatten chunks)) ; if nothing in body
                      empty-chunk ; have no body
                      (speculative-chunk (concat-chunk space-chunk          ; if body can fit on a single line, put it there
                                                       (build space-chunk)
                                                       space-chunk)
                                         length-equals-one
                                         (indent-chunk 3                    ; if not, increase indent
                                                       (concat-chunk new-line-chunk ; and start a new line
                                                                     (speculative-chunk (build blank-line-chunk)  ; if inner body fits on a single line, put it there
                                                                                        length-equals-one
                                                                                        (concat-chunk new-line-chunk ; if not, put a blank line between start of body and start of inner body
                                                                                                      (build blank-line-chunk)))
                                                                     new-line-chunk)))) ; start a new line after inner body has been printed
                  imm-close-crbr-chunk))) ; close body
(provide body-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;preprocessor chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;preprocessor define chunk
; #define chunk
(define/debug (pp-define-chunk name)
  (-> chunk/c chunk/c)
  (concat-chunk pp-directive-chunk
                define-chunk
                space-chunk
                name))
(provide pp-define-chunk)

;preprocessor include chunk
; #include<...> chunk
(define/debug (pp-include-chunk included)
  (-> chunk/c chunk/c)
  (concat-chunk pp-directive-chunk
                include-chunk
                space-chunk
                (template-list-chunk included)))
(provide pp-include-chunk)

;alternate preprocessor include chunk
; #include<...> chunk
(define/debug (pp-alt-include-chunk included)
  (-> chunk/c chunk/c)
  (concat-chunk pp-directive-chunk
                include-chunk
                space-chunk
                "\""
                included
                "\""))
(provide pp-alt-include-chunk)

;multiple includes
(define/debug (pp-includes-chunk . chunks)
  (->* () #:rest chunk-list/c chunk/c)
  (between-chunk new-line-chunk
                 (map pp-include-chunk
                      (flatten chunks))))
(provide pp-includes-chunk)

;preprocessor if-not-defined chunk
(define/debug (pp-ifdef-chunk condition)
  (-> chunk/c chunk/c)
  (concat-chunk pp-directive-chunk
                ifdef-chunk
                space-chunk
                condition))
(provide pp-ifdef-chunk)

;preprocessor if-not-defined chunk
(define/debug (pp-ifndef-chunk condition)
  (-> chunk/c chunk/c)
  (concat-chunk pp-directive-chunk
                ifndef-chunk
                space-chunk
                condition))
(provide pp-ifndef-chunk)

;preprocessor if-not-defined chunk
(define/debug pp-else-chunk
  chunk/c
  (concat-chunk pp-directive-chunk
                else-chunk))
(provide pp-else-chunk)

;preprocessor endif chunk
(define/debug (pp-endif-chunk condition)
  (-> chunk/c chunk/c)
  (concat-chunk pp-directive-chunk
                endif-chunk
                new-line-chunk
                (comment-env-chunk condition)))
(provide pp-endif-chunk)

;preprocessor conditional chunk
(define/debug (pp-conditional-chunk directive condition then [else #false])
  (->* (chunk/c chunk/c chunk/c) ((or/c chunk/c #false)) chunk/c)
  (concat-chunk pp-directive-chunk
                directive
                space-chunk
                condition
                new-line-chunk
                (indent-chunk 3
                              then)
                new-line-chunk
                (if else
                    (concat-chunk pp-else-chunk
                                  new-line-chunk
                                  (indent-chunk 3
                                                else)
                                  new-line-chunk)
                    empty-chunk)
                (pp-endif-chunk condition)))
(provide pp-conditional-chunk)

;preprocessor conditional ifdef chunk
(define/debug (pp-conditional-ifdef-chunk condition then [else #false])
  (->* (chunk/c chunk/c) ((or/c chunk/c #false)) chunk/c)
  (pp-conditional-chunk ifdef-chunk condition then else))
(provide pp-conditional-ifdef-chunk)

;preprocessor conditional ifndef chunk
(define/debug (pp-conditional-ifndef-chunk condition then [else #false])
  (->* (chunk/c chunk/c) ((or/c chunk/c #false)) chunk/c)
  (pp-conditional-chunk ifndef-chunk condition then else))
(provide pp-conditional-ifndef-chunk)

;preprocessor h file wrapper chunk
(define/debug (pp-header-file-chunk file-name file-setup . chunks)
  (->* (chunk/c chunk/c) #:rest chunk-list/c chunk/c)
  (pp-conditional-ifndef-chunk file-name
                               (concat-chunk (pp-define-chunk file-name)
                                             blank-line-chunk
                                             file-setup
                                             blank-line-chunk
                                             (top-smt-list-chunk blank-line-chunk
                                                                 chunks)
                                             new-line-chunk)))
(provide pp-header-file-chunk)

;macro defintion chunk
; a macro definition
(define/debug (macro-define-chunk name params chunk)
  (-> chunk/c nullable-chunk-list/c chunk/c chunk/c)
  (let ([macro-signature (concat-chunk (pp-define-chunk name)
                                       (if (empty? (flatten params))
                                           empty-chunk
                                           (paren-list-chunk params)))])
    (speculative-chunk (concat-chunk macro-signature
                                     space-chunk
                                     chunk)
                       length-equals-one
                       (macro-env-chunk (concat-chunk macro-signature
                                                      new-line-chunk
                                                      (indent-chunk 3 chunk))))))
(provide macro-define-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;general chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;namespace define chunk
(define/debug (namespace-define-chunk name . chunks)
  (->* (chunk/c) #:rest chunk-list/c chunk/c)
  (let ([chunk (concat-chunk imm-namespace-chunk
                             imm-space-chunk
                             (immediate-chunk name)
                             imm-space-chunk
                             (body-chunk chunks))])
    (speculative-chunk chunk
                       length-equals-one
                       (concat-chunk chunk
                                     space-chunk
                                     (comment-env-chunk name)))))
(provide namespace-define-chunk)

;described statements chunk
(define/debug (described-smts-chunk comment . chunks)
  (->* (chunk/c) #:rest chunk-list/c chunk/c)
  (concat-chunk (comment-env-chunk comment)
                new-line-chunk
                (between/attach-chunk semi-colon-chunk
                                      new-line-chunk
                                      chunks)))
(provide described-smts-chunk)

;make constant
(define/debug (constize-chunk chunk)
  (-> chunk/c chunk/c)
  (concat-chunk chunk
                imm-space-chunk
                imm-const-chunk))
(provide constize-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;template chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;make given chunk a template with given template parameters
(define/debug (template-define-chunk params chunk)
  (-> nullable-chunk-list/c chunk/c chunk/c)
  (concat-chunk template-chunk
                (template-list-chunk params)
                new-line-chunk
                (indent-chunk 1
                              chunk)))
(provide template-define-chunk)

;make a use of a template
(define/debug (template-use-chunk name . args)
  (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (concat-chunk name
                (if (empty? (flatten args))
                    empty-chunk
                    (template-list-chunk args))))
(provide template-use-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;function chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;function declaration
(define/debug (function-declare-chunk name return-type . params)
  (->* (chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (concat-chunk imm-inline-chunk
                space-chunk
                return-type
                space-chunk
                name
                (if (empty? (flatten params))
                    (concat-chunk imm-open-paren-chunk
                                  imm-void-chunk
                                  imm-close-paren-chunk)
                    (paren-list-chunk params))))
(provide function-declare-chunk)

;static function declaration
(define/debug (static-function-declare-chunk name return-type . params)
  (->* (chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (concat-chunk imm-static-chunk
                imm-space-chunk
                (function-declare-chunk name
                                        return-type
                                        params)))
(provide static-function-declare-chunk)

;void function declaration
(define/debug (void-function-declare-chunk name params)
  (-> chunk/c nullable-chunk-list/c chunk/c)
  (function-declare-chunk name void-chunk params))
(provide void-function-declare-chunk)

;function defintion
(define/debug (function-define-chunk signature . body)
  (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (concat-chunk signature
                imm-space-chunk
                (if (empty? (flatten body))
                    (body-chunk)
                    (body-chunk body))))
(provide function-define-chunk)

;void function defintion
(define/debug (void-function-define-chunk name params . body)
  (->* (chunk/c nullable-chunk-list/c) #:rest nullable-chunk-list/c chunk/c)
  (function-define-chunk (void-function-declare-chunk name
                                                      params)
                         body))
(provide void-function-define-chunk)

;returning function defintion
(define/debug (returning-function-define-chunk signature body return-expr)
  (-> chunk/c nullable-chunk-list/c chunk/c chunk/c)
  (function-define-chunk signature
                         (flatten* body
                                   (concat-chunk return-chunk
                                                 imm-space-chunk
                                                 (position-indent-chunk return-expr)))))
(provide returning-function-define-chunk)

;constructor assignment
(define/debug (constructor-assignment-chunk var val)
  (-> chunk/c chunk/c chunk/c)
  (concat-chunk var
                imm-open-paren-chunk
                val
                imm-close-paren-chunk))
(provide constructor-assignment-chunk)

;constructor defintion
(define/debug (constructor-chunk name params assigns . body)
  (->* (chunk/c nullable-chunk-list/c nullable-chunk-list/c) #:rest nullable-chunk-list/c chunk/c)
  (concat-chunk name
                (paren-list-chunk params)
                (if (empty? (flatten assigns))
                    (concat-chunk imm-space-chunk
                                  (body-chunk body))
                    (concat-chunk new-line-chunk
                                  (constructor-assignment-list-chunk assigns)
                                  new-line-chunk
                                  (body-chunk body)))))
(provide constructor-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;class/struct chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;struct declaration
(define/debug (struct-declare-chunk name)
  (-> chunk/c chunk/c)
  (concat-chunk struct-chunk
                space-chunk
                name))
(provide struct-declare-chunk)

;template struct declaration
(define/debug (template-struct-declare-chunk name params . args)
  (->* (chunk/c nullable-chunk-list/c) #:rest nullable-chunk-list/c chunk/c)
  (template-define-chunk params
                         (template-use-chunk (struct-declare-chunk name)
                                             args)))
(provide template-struct-declare-chunk)

;struct section
(define/debug (section-define-chunk type . chunks)
  (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (concat-chunk type
                colon-chunk
                new-line-chunk
                (indent-chunk 1 (smt-list-chunk new-line-chunk chunks))))
(provide section-define-chunk)

;struct definition
(define/debug (struct-define-chunk signature . body)
  (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (concat-chunk signature
                imm-space-chunk
                (body-chunk body)))
(provide struct-define-chunk)

;template struct definition
(define/debug (template-struct-define-chunk name params args . body)
  (->* (chunk/c nullable-chunk-list/c nullable-chunk-list/c) #:rest nullable-chunk-list/c chunk/c)
  (struct-define-chunk (template-struct-declare-chunk name params args)
                       body))
(provide template-struct-define-chunk)

;scope resolution operator
(define/debug (scope-resolution-operator-chunk scope variable)
  (-> chunk/c chunk/c chunk/c)
  (concat-chunk scope
                imm-colon-chunk
                imm-colon-chunk
                variable))
(provide scope-resolution-operator-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;statement chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;typedef statement chunk
(define/debug (typedef-smt-chunk lhs rhs)
  (-> chunk/c chunk/c chunk/c)
  (concat-chunk lhs
                space-chunk
                typedef-chunk
                space-chunk
                rhs))
(provide typedef-smt-chunk)

;function call
(define/debug (function-call-chunk fcn . args)
  (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (concat-chunk fcn
                (paren-list-chunk args)))
(provide function-call-chunk)

;member function call
(define/debug (member-function-call-chunk obj fcn . args)
  (->* (chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c)
  (concat-chunk obj
                imm-period-chunk
                (position-indent-chunk (function-call-chunk fcn args))))
(provide member-function-call-chunk)
