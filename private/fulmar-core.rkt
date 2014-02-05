#lang racket

(provide (all-defined-out))

(define (flatten* . lst)
  (flatten lst))

(struct s-chunk (name body) #:transparent)

(provide (except-out (all-defined-out) mode indention line-length))

(define mode (make-parameter 'normal))
(define indention (make-parameter ""))
(define line-length (make-parameter 80))

(define (make-whitespace n)
  (make-string n #\space))

(define (remove-whitespace line)
  (or (let ((length (string-length line)))
        (for/first ([i (in-range (- length 1) -1 -1)]
                    #:when (not (equal? #\space (string-ref line i))))
          (substring line 0 (+ i 1))
          )) 
      ""))

(define (is-whitespace? line)
  (zero? (string-length (remove-whitespace line))))

(define (finish-line given-line)
  (if (equal? given-line (indention))
      ""
      (remove-whitespace given-line)))

(define (add-literal string line)
  (let* ((stringl (string-length string))
         (linel (string-length line)))
    (cond [(= 0 stringl)
           (list line)]
          [(or (equal? 'immediate (mode)) 
               (<= (+ stringl linel)
                   (line-length))
               (>= (string-length (indention))
                   linel))
           (list (string-append line string))]
          [else
           (list (string-append (indention) string)
                 (finish-line line))])))

(define (add-space line)
  (if (or (equal? (mode) 'immediate)
          (< (string-length line) (line-length)))
      (list (string-append line " "))
      (list "" (finish-line line))))

(define (add-concatenated nekots line)
  (for/fold ([lines (list line)]) ([nekot (in-list nekots)])
    (append (write-chunk nekot (car lines))
            (cdr lines))))

(define (add-speculative body line)
  (match-let* ([(list attempt success? backup) body]
               [attempted (write-chunk attempt line)])
    (if (success? attempted)
        attempted
        (write-chunk backup line))))

(define write-chunk
  (case-lambda 
    [(chunk)
     (write-chunk chunk "")]
    [(chunk line)
     (define new-line (if (equal? line "")
                          (indention)
                          line))
     (match chunk
       [(? string?)
        (add-literal chunk new-line)]
       [(? symbol?)
        (add-literal (symbol->string chunk) new-line)]
       [(? exact-nonnegative-integer?)
        (add-space new-line)]
       [(s-chunk 'new-line _)
        (list "" (finish-line line))]
       [(s-chunk 'concat body) 
        (add-concatenated body new-line)]
       [(s-chunk 'immediate body) 
        (parameterize ([mode 'immediate])
          (write-chunk body line))]
       [(s-chunk 'speculative body) 
        (add-speculative body new-line)]
       [(s-chunk 'position-indent body) 
        (parameterize ([indention (make-whitespace (string-length line))])
          (write-chunk body line))]
       [(s-chunk 'indent (list body length))
        (parameterize ([indention (string-append (indention) (make-whitespace length))])
          (write-chunk body line))]
       )]))