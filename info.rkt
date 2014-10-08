#lang info

(define version "0.9.2")
(define collection "fulmar")
(define deps '("base"
               "scribble-lib"
               "typed-racket-lib"
               "rackunit-lib"
               "sandbox-lib"
               "at-exp-lib"))
(define build-deps '("at-exp-lib"))
(define scribblings '(("doc/fulmar-doc.scrbl" ())))
