#lang typed/racket

(provide (all-defined-out))

; A simple maybe monad for racket

(struct: Nothing ())
(struct: (a) Just ([v : a]))

(define-type (Maybe a) (U Nothing (Just a)))

; Cons together everything that isn't nothing

(: cons-with-maybe (All (a) ((Maybe a)
                             (Listof a)
                             -> (Listof a))))
(define (cons-with-maybe ar dr)
  (cond
    [(Nothing? ar) dr]
    [(Just? ar) (let ([v (Just-v ar)]) (cons v dr))]))

; Take a list of Maybe something and produce a list of only the Justs

(: all-justs (All (a) ((Listof (Maybe a)) -> (Listof a))))
(define (all-justs in-list)
  (: helper ((Listof (Maybe a)) (Listof a) -> (Listof a)))
  (define (helper ls acc)
    (match ls
      ['()
       #;=>
       acc]
      [(cons x xs)
       #;=>
       (helper xs (cons-with-maybe x acc))]))
  (reverse (helper in-list '())))

; Take a predicate and a list of two things and separate the list.

(: segregate (All (a b) (((U a b) -> (Pairof (Maybe a) (Maybe b)))
                         (Listof (U a b))
                         -> (Pairof (Listof a) (Listof b)))))
(define (segregate sifter lst)
  (foldr
   (λ: ([e : (U a b)] [p : (Pairof (Listof a) (Listof b))])
     (let ([se (sifter e)])
       (let ([l (car se)]
             [r (cdr se)]
             [ls (car p)]
             [rs (cdr p)])
         (ann (cons (cons-with-maybe l ls)
                    (cons-with-maybe r rs)) (Pairof (Listof a) (Listof b))))))
   (ann '(() . ()) (Pairof (Listof a) (Listof b)))
   lst))
