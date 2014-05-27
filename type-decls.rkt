#lang typed/racket

;;; GET FULMAR CHUNK-MAKING FUNCTIONS ;;;

(require "private/fulmar-core.rkt"
         "private/core-chunk.rkt"
         "standard-chunk.rkt"
         "utility.rkt")

; This really belongs in an abbreviations file or somewhere. Anywhere but here.
; It's here now because I don't have a complete typed abbreviations file, and
; if I did, I'm not sure requiring it in this file would be a good idea.
(: between-spaces ((Listof Chunk) -> Chunk))
(define (between-spaces chunks)
  (apply between " " chunks))

;;; TYPE DEFINITIONS ;;;

(define-type NDBoolean (U Boolean 'unspecified))
(define-type C++-base-type (U C++-pointable-type Symbol))
(define-type C++-type-size (U "" 'long 'short 'longlong))
(define-type C++-type-signedness (U "" 'signed 'unsigned))
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
  ([length : Chunk])
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

(provide typ-float
         typ-double
         typ-long-double
         typ-int
         typ-char
         typ-pointer
         typ-reference
         typ-array
         typ-template-type)

(: typ-float (C++-type-qualifier * -> C++-pointable-type))
(define (typ-float . qualifiers)
  (C++-pointable-type 'float qualifiers))

(: typ-double (C++-type-qualifier * -> C++-pointable-type))
(define (typ-double . qualifiers)
  (C++-pointable-type 'double qualifiers))

(: typ-long-double (C++-type-qualifier * -> C++-sizable-type))
(define (typ-long-double . qualifiers)
  (C++-sizable-type 'double qualifiers 'long))

(: typ-int (C++-type-size
            C++-type-signedness
            C++-type-qualifier * -> C++-integer-type))
(define (typ-int size signedness . qualifiers)
  (C++-integer-type 'int qualifiers size signedness))

(: typ-char (C++-type-signedness C++-type-qualifier * -> C++-integer-type))
(define (typ-char signedness . qualifiers)
  (C++-integer-type 'char qualifiers "" signedness))

(: typ-pointer (C++-pointable-type C++-type-qualifier * -> C++-pointer-type))
(define (typ-pointer base . qualifiers)
  (C++-pointer-type base qualifiers))

(: typ-reference (C++-pointable-type C++-type-qualifier * -> C++-reference-type))
(define (typ-reference base . qualifiers)
  (C++-reference-type base qualifiers))

(: typ-array (C++-pointable-type
              Integer
              C++-type-qualifier * -> C++-array-type))
(define (typ-array base length . qualifiers)
  (C++-array-type base qualifiers length))

(: typ-template-type (C++-base-type
                      (U C++-type C++-type-qualifier) * -> C++-templated-type))
(define (typ-template-type base . qualifiers-and-params)
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

(provide dcl-variable
         dcl-type)

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

(: dcl-variable ((U C++-type C++-base-type) Chunk -> Chunk))
(define (dcl-variable type name)
  (match type
    [(C++-reference-type (and base (C++-array-type _ _ _)) qualifiers)
     #;=>
     (dcl-variable base
      (concat "(&" (between-spaces `(,@qualifiers ,name)) ")"))]
    [(C++-pointer-type (and base (C++-array-type _ _ _)) qualifiers)
     #;=>
     (dcl-variable base
      (concat "(*" (between-spaces `(,@qualifiers ,name)) ")"))]
    [(C++-reference-type base qualifiers)
     #;=>
     (dcl-variable base
      (concat "&" (between-spaces `(,@qualifiers ,name))))]
    [(C++-pointer-type base qualifiers)
     #;=>
     (dcl-variable base
      (concat "*" (between-spaces `(,@qualifiers ,name))))]
    [(C++-array-type base _ length)
     #;=>
     (dcl-variable base
      (concat name "[" length "]"))]
    [(C++-templated-type base qualifiers parameters)
     #;=>
     (between-spaces
      `(,(concat (dcl-type base)
                 "< "
                 (apply between/attach "," " " (map dcl-type parameters))
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

(: dcl-type ((U C++-type C++-base-type) -> Chunk))
(define (dcl-type type)
  (dcl-variable type empty))