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

(define (finish-line given-line indentation)
  (if (equal? given-line indentation)
      (f-line '())
      (remove-whitespace given-line)))

(define (add-literal string line mode indentation line-length)
  (let* ((stringl (length string))
         (linel (length (f-line-string line))))
    (cond [(= 0 stringl)
           (list line)]
          [(or (equal? 'immediate mode)
               (<= (+ stringl linel)
                   line-length)
               (>= (length indentation)
                   linel))
           (list (f-line (append (f-line-string line) string)))]
          [else
           (list (f-line (append indentation string))
                 (finish-line line))])))

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
