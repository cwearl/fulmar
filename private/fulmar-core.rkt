#lang racket

(provide (all-defined-out))

(define (flatten* . lst)
  (flatten lst))

(struct s-chunk (name body) #:transparent)
(struct f-line (string) #:transparent)

(provide (all-defined-out))

(define (make-whitespace n)
  (f-line (make-list n #\space)))

(define (remove-whitespace line)
  (f-line (or (let ((length (length (f-line-string line))))
                (for/first ([i (in-range (- length 1) -1 -1)]
                            #:when (not (equal? #\space (list-ref (f-line-string line) i))))
                           (take (f-line-string line) (+ i 1))))
              '())))

(define (is-whitespace? line)
  (zero? (length (remove-whitespace (f-line-string line)))))

(define finish-lineo
  (lambda (given-line indentation out)
    (conde
     [(== given-line indentation)
      (== (f-line '()) out)]
     [(=/= given-line indentation)
      (remove-whitespaceo given-line out)])))

(define appendo
    (lambda (l s out)
      (conde
        [(fresh (a d res)
           (== (cons a d) l)
           (appendo d s res)
           (== (cons a res) out))]
        [(== '() l)
         (== s out)])))

;lst1 is at least as long as lst2
(define as-longo
  (lambda (lst1 lst2 out)
    (conde
     [(== '() lst2)
      (== #t out)]
     [(fresh (a d)
             (== (cons a d) lst2)
             (== '() lst1)
             (== #f out))]
     [(fresh (a1 d1 a2 d2)
             (== (cons a2 d2) lst2)
             (== (cons a1 d1) lst1)
             (as-longo d1 d2 out))])))

(define (add-literalo string line mode indentation line-length out)
  (conde
   ;empty string
   [(== '() string)
    (== (cons line '()) out)]
   ;non empty string
   [(fresh (s-line)
           (=/= '() string)
           (== (f-line s-line) line)
           (conde
            ;fits on one line
            [(conde
              [(== 'immediate mode)]
              [(fresh (res length)
                      (appendo s-line string res)
                      (make-whitespaceo line-length length)
                      (as-longo res length #t))]
              [(fresh (length)
                      (make-whitespaceo indentation length)
                      (as-longo length s-line #t))])
             (== (cons (f-line (appendo s-line string)) '()) out)]
            ;need new line
            [(=/= 'immediate mode)
             (fresh (res length)
                    (appendo s-line string res)
                    (make-whitespaceo line-length length)
                    (as-longo res length #f))
             (fresh (length)
                    (make-whitespaceo indentation length)
                    (as-longo length s-line #f))
             (fresh (res)
                    (appendo indentation string res)
                    (== out
                        (cons (f-line res)
                              (cons (finish-lineo line)
                                    '()))))]))]))

(define add-spaceo
  (lambda (line mode indentation line-length)
    (fresh (s-line)
           (== (f-line s-line) line)
           (conde
            [(conde
              [(== 'immediate mode)]
              [???

(define (add-space line mode indentation line-length)
  (if (or (equal? 'immediate mode)
          (< (length (f-line-string line)) line-length))
      (f-line (list (append (f-line-string line) (list #\space))))
      (list (f-line '()) (finish-line line indentation))))

(define (add-concatenated nekots line mode indentation line-length in-comment?)
  (for/fold ([lines (list line)]) ([nekot (in-list nekots)])
    (append (write-chunk nekot (car lines) mode indentation line-length in-comment?)
            (cdr lines))))

(define (add-speculative body line mode indentation line-length in-comment?)
  (match-let* ([(list attempt success? backup) body]
               [attempted (write-chunk attempt line mode indentation line-length in-comment?)])
    (if (success? attempted)
        attempted
        (write-chunk backup line mode indentation line-length in-comment?))))

(define write-chunk
  (case-lambda 
    [(chunk)
     (write-chunk chunk (f-line '()) 'normal '() 80 #f)]
    [(chunk line mode indentation line-length in-comment?)
     (define new-line (if (equal? line (f-line '()))
                          (f-line indentation)
                          line))
     (match chunk
       [(? string?)
        (add-literal chunk new-line mode indentation line-length)]
       [(? symbol?)
        (add-literal (string->list (symbol->string chunk)) new-line mode indentation line-length)]
       [(? exact-nonnegative-integer?)
        (add-literal (string->list (number->string chunk)) new-line mode indentation line-length)]
       [(s-chunk 'space _)
        (add-space new-line mode indentation line-length)]
       [(s-chunk 'new-line _)
        (list (f-line '())
              (finish-line line indentation line-length))]
       [(s-chunk 'concat body) 
        (add-concatenated body new-line mode indentation line-length in-comment?)]
       [(s-chunk 'immediate body) 
        (write-chunk body line 'immediate indentation line-length in-comment?)]
       [(s-chunk 'speculative body) 
        (add-speculative body new-line mode indentation line-length)]
       [(s-chunk 'position-indent body) 
        (write-chunk body
                     line
                     mode
                     (if (equal? line (f-line '()))
                         indentation
                         (make-whitespace (length (f-line-string line))))
                     line-length
                     in-comment?)]
       [(s-chunk 'indent (list body length))
        (write-chunk body
                     line
                     mode
                     (append indentation (make-whitespace length))
                     line-length
                     in-comment?)]
       [(s-chunk 'comment (list body init-char))
        (write-chunk
         (s-chunk 'concat
                  (flatten (list "/*"
                                 (string init-char)
                                 (s-chunk 'position-indent body)
                                 (if in-comment?
                                     " **"
                                     " */"))))
         line
         mode
         indentation
         line-length
         #t)])]))
