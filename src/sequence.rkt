#lang racket

(require "basics.rkt")

;structures, functions, and contracts for sequence

;structures

;sequence structure
(struct seq-struct (IR) #:transparent)

;predicates

;predicate for sequence internal representation
(define/contract (seq-IR? g)
  pred/c
  (list-of? (or? char?
                 indent?) 2 g))
(provide seq-IR?)

;predicate for building sequence
(define/contract (seq-input? g)
  pred/c
  (list-of? (or? seq?
                 char?
                 indent?) 0 g))
(provide seq-input?)

;predicate for sequence
(define/contract (seq? g)
  pred/c
  (and (seq-struct? g)
       (seq-IR? (seq-struct-IR g))))
(provide seq?)

;predicate for output of sequence
(define/contract (seq-output? g)
  pred/c
  ((or? seq?
        char?
        indent?) g))
(provide seq-output?)

;constructors

;sequence constructor
(define/contract (seq . g)
  (->* () #:rest seq-input? seq-output?)
  (build-seq g))
(provide seq)

;accessors

;sequence internal representation accessor
(define/contract (seq-IR g)
  (-> seq? seq-IR?)
  (seq-struct-IR g))
(provide seq-IR)

;general procedures

;procedure to build a sequence
(define/contract (build-seq g)
  (-> seq-input? seq-output?)
  (let ([items (simplify-seq-IR (flatten* g))])
    (cond [((or? seq?
                 char?
                 indent?) items)
           items]
          [(list? items)
           ;foldl is used to reverse list
           (seq-struct (foldl (λ (item sir)
                                (cond [(seq? item)
                                       (append (seq-IR item) sir)]
                                      [((or? char?
                                             indent?) item)
                                       (cons item sir)]
                                      [else
                                       (error "Unrecognized item for sequence; unrecognized: " item "; given: " g)]))
                              null
                              items))]
          [else
           (error "Unrecognized item for sequence; unrecognized: " g)])))
(provide build-seq)

;procedure to simplify a sequence internal representation
(define/contract (simplify-seq-IR g)
  (-> (listof (or? seq? char? indent?)) (or? seq?
                                             char?
                                             indent?
                                             (list-of? (or? seq? char? indent?) 1)))
  ;foldr is used to keep list in same order as given
  (let ([new-sir (foldr (λ (item sir)
                          (cond [(and (indent? item)
                                      (= 0 item))
                                 sir]
                                [(and (indent? item)
                                      (non-empty-list? sir)
                                      (indent? (first sir)))
                                 (cons (+ item (first sir))
                                       (rest sir))]
                                [else (cons item sir)]))
                        null
                        g)])
    (cond [(null? new-sir) 0]
          [(= 1 (length new-sir)) (first new-sir)]
          [else new-sir])))
(provide simplify-seq-IR)
