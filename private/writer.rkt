#lang racket

(require "fulmar-core.rkt")
(require "core-chunk.rkt")

(provide (except-out (all-defined-out) mode environment context-indent initial-position line-length))

(define mode (make-parameter 'normal))
(define environment (make-parameter 'empty))
(define context-indent (make-parameter 0))
(define initial-position (make-parameter 0))
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

(define (build-indentation)
  (match (environment)
    ['empty
     (make-whitespace (context-indent))]
    [_ 
     (string-append 
      (make-whitespace (initial-position))
      "/* "
      (let ([remaining (- (context-indent)
                          (initial-position))])
        (make-whitespace (max 0 remaining))))]))

(define (finish-line given-line)
  (define line (remove-whitespace given-line))
  (if (equal? line (remove-whitespace (build-indentation)))
      ""
      (match (environment)
        ['empty line]
        ['comment (string-append line " */")])))

(define (add-literal string line)
  (let* ((stringl (string-length string))
         (linel (string-length line)))
    (cond [(= 0 stringl)
           (list line)]
          [(or (equal? 'immediate (mode)) 
               (<= (+ stringl linel)
                   (line-length))
               (>= (string-length (build-indentation))
                   linel))
           (list (string-append line string))]
          [else
           (list (string-append (build-indentation) string)
                 (finish-line line))])))

(define (add-space line)
  (if (or (equal? (mode) 'immediate)
          (< (string-length line) (line-length)))
      (list (string-append line " "))
      (list "" (finish-line line))))

(define (add-pp-directive line)
  (list (cond [(= 0 (string-length line)) "#"]
              [(is-whitespace? line)
               (string-append "#" (substring line 1))]
              [else
               (string-append (substring line 0 (- (string-length line) 1))
                              "#")])))

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

(define (add-comment chunk line)
  (define middle 
    (list
     (string #\space)
     (s-chunk 'real-comment-env chunk)))
  
  (write-chunk 
   (concat 
    (match (environment) 
      ['comment (list "//" middle)]
      [_ (list "/*" middle " */")])) 
   line))

(define write-chunk
  (case-lambda 
    [(chunk)
     (write-chunk chunk "")]
    [(chunk line)
     (define new-line (if (equal? line "")
                          (build-indentation)
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
       [(s-chunk 'pp-directive _)
        (add-pp-directive new-line)]
       [(s-chunk 'concat body) 
        (add-concatenated body new-line)]
       [(s-chunk 'immediate body) 
        (parameterize ([mode 'immediate])
          (write-chunk body line))]
       [(s-chunk 'speculative body) 
        (add-speculative body new-line)]
       [(s-chunk 'position-indent body) 
        (parameterize ([context-indent (string-length line)])
          (write-chunk body line))]
       [(s-chunk 'indent (list body length))
        (parameterize ([context-indent (+ length (context-indent))])
          (write-chunk body line))]
       [(s-chunk 'real-comment-env body)
        (parameterize ([environment 'comment]
                       [initial-position (context-indent)])
          (write-chunk body line))]
       [(s-chunk 'comment-env (list body))
        (add-comment body line)]
       )]))
