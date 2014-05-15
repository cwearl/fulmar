#lang typed/racket

;;; GET FULMAR CHUNK-MAKING FUNCTIONS ;;;

(require/typed fulmar/private/fulmar-core
               [#:struct s-chunk ([name : Symbol] [body : Chunk])])

; Almost certainly not correct!
; This type definition needs to be fixed when we type-ify more fulmar.
(define-type Chunk (Rec Ch (U s-chunk String Symbol (Listof Ch))))

(require/typed fulmar/standard-chunk
               [concat (Chunk * -> Chunk)]
               [between (Chunk Chunk * -> Chunk)]
               [between/attach (Chunk Chunk Chunk * -> Chunk)])

(: between-spaces ((Listof Chunk) -> Chunk))
(define (between-spaces chunks)
  (apply between " " chunks))

;;; GENERAL-PURPOSE STUFF ;;;
;;  should go somewhere else  ;;

;;; TYPE DEFINITIONS ;;;

(define-type NDBoolean (U Boolean 'unspecified))
(define-type C++-base-type (U C++-pointable-type Symbol))
(define-type C++-type-size (U Null 'long 'short 'longlong))
(define-type C++-type-signedness (U Null 'signed 'unsigned))
(define-type C++-type-qualifier (U 'const 'volatile))
(define-type C++-float-type (U 'float 'double 'longdouble))

;; Internal C++ type representation ;;

(struct: C++-type
  ([base : C++-base-type])
  #:transparent)

(struct: C++-qualified-type C++-type
  ([qualifiers : (Listof C++-type-qualifier)])
  #:transparent)

(struct: C++-pointable-type C++-qualified-type () #:transparent)
(struct: C++-reference-type C++-qualified-type () #:transparent)
(struct: C++-pointer-type C++-pointable-type () #:transparent)

(struct: C++-array-type C++-pointable-type
  ([length : Integer]) ; Should actually be someting like (U Integer Chunk)
  #:transparent)

; Primitive type stuff
(struct: C++-sizable-type C++-pointable-type
  ([size : C++-type-size])
  #:transparent)

(struct: C++-integer-type C++-sizable-type
  ([signedness : C++-type-signedness])
  #:transparent)

; Template type stuff
(struct: C++-templated-type C++-pointable-type
  ([parameters : (Listof C++-type)]) ; Should allow more kinds of parameters
  #:transparent)

;;; PUBLIC CONSTRUCTORS ;;;

(provide fmr-float
         fmr-double
         fmr-long-double
         fmr-int
         fmr-char
         fmr-pointer
         fmr-reference
         fmr-array
         fmr-template-type)

(: fmr-float (C++-type-qualifier * -> C++-pointable-type))
(define (fmr-float . qualifiers)
  (C++-pointable-type 'float qualifiers))

(: fmr-double (C++-type-qualifier * -> C++-pointable-type))
(define (fmr-double . qualifiers)
  (C++-pointable-type 'double qualifiers))

(: fmr-long-double (C++-type-qualifier * -> C++-sizable-type))
(define (fmr-long-double . qualifiers)
  (C++-sizable-type 'double qualifiers 'long))

(: fmr-int (C++-type-size
            C++-type-signedness
            C++-type-qualifier * -> C++-integer-type))
(define (fmr-int size signedness . qualifiers)
  (C++-integer-type 'int qualifiers size signedness))

(: fmr-char (C++-type-signedness C++-type-qualifier * -> C++-integer-type))
(define (fmr-char signedness . qualifiers)
  (C++-integer-type 'char qualifiers '() signedness))

(: fmr-pointer (C++-pointable-type C++-type-qualifier * -> C++-pointer-type))
(define (fmr-pointer base . qualifiers)
  (C++-pointer-type base qualifiers))

(: fmr-reference (C++-pointable-type C++-type-qualifier * -> C++-reference-type))
(define (fmr-reference base . qualifiers)
  (C++-reference-type base qualifiers))

(: fmr-array (C++-pointable-type
              Integer
              C++-type-qualifier * -> C++-array-type))
(define (fmr-array base length . qualifiers)
  (C++-array-type base qualifiers length))

(: fmr-template-type (C++-base-type
                      (U C++-type C++-type-qualifier) * -> C++-templated-type))
(define (fmr-template-type base . qualifiers-and-params)
  (let: ([sqa : (Pairof (Listof C++-type) (Listof C++-type-qualifier))
              (segregate (λ: ([q-or-p : (U C++-type C++-type-qualifier)])
                           (if (C++-type? q-or-p)
                               (cons (Just q-or-p) (Nothing))
                               (cons (Nothing) (Just q-or-p))))
                         qualifiers-and-params)])
    
    (let ([params (car sqa)]
          [qualifiers (cdr sqa)])
      (C++-templated-type base qualifiers params))))

;;; TYPE RENDERING ;;;

(provide fmr-variable-decl
         fmr-type-decl)

(: render-base-type (C++-base-type -> Chunk))
(define (render-base-type type)
  (if (C++-pointable-type? type)
      (render-simple-type type)
      type))

(: render-simple-type (C++-qualified-type -> Chunk))
(define (render-simple-type type)
  (match type
    [(C++-integer-type base qualifiers size signedness)
     #;=>
     (between-spaces `(,size ,signedness ,(render-base-type base) ,@qualifiers))]
    [(C++-sizable-type base qualifiers size)
     #;=>
     (between-spaces `(,size ,(render-base-type base) ,@qualifiers))]
    [(C++-qualified-type base qualifiers)
     #;=>
     (between-spaces `(,(render-base-type base) ,@qualifiers))]))

(: fmr-variable-decl ((U C++-type C++-base-type) Chunk -> Chunk))
(define (fmr-variable-decl type name)
  (match type
    [(C++-reference-type (and base (C++-array-type _ _ _)) qualifiers)
     #;=>
     (fmr-variable-decl base
      (concat "(&" (between-spaces `(,@qualifiers ,name)) ")"))]
    [(C++-pointer-type (and base (C++-array-type _ _ _)) qualifiers)
     #;=>
     (fmr-variable-decl base
      (concat "(*" (between-spaces `(,@qualifiers ,name)) ")"))]
    [(C++-reference-type base qualifiers)
     #;=>
     (fmr-variable-decl base
      (concat "&" (between-spaces `(,@qualifiers ,name))))]
    [(C++-pointer-type base qualifiers)
     #;=>
     (fmr-variable-decl base
      (concat "*" (between-spaces `(,@qualifiers ,name))))]
    [(C++-array-type base _ length)
     #;=>
     (fmr-variable-decl base
      (concat name "[" (number->string length) "]"))] ; The number->string bit will go away when number literals are chunks
    [(C++-templated-type base qualifiers parameters)
     #;=>
     (between-spaces
      `(,(concat (fmr-type-decl base)
                 "< "
                 (apply between/attach "," " " (map fmr-type-decl parameters))
                 " >") ,@qualifiers ,name))]
    [(and t (C++-qualified-type _ _))
     #;=>
     (between-spaces `(,(render-simple-type t) ,name))]
    [(C++-type base)
     #;=>
     (between-spaces `(,(render-base-type base) ,name))]
    [else
     #;=>
     (if (symbol? type)
         (between-spaces `(,type ,name))
         (error "Unexpected type: " type))]))

(: fmr-type-decl ((U C++-type C++-base-type) -> Chunk))
(define (fmr-type-decl type)
  (fmr-variable-decl type '()))