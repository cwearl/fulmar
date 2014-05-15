#lang typed/racket

#;(provide (all-defined-out))

(struct: S-chunk () #:transparent)
(define new-line-chunk (S-chunk))

(define-type Chunk (U String Symbol Integer S-chunk))
(define-type NestofChunks (Rec T (U (Listof T) Chunk)))

(struct: Immediate       S-chunk ([body : Chunk]) #:transparent)
(struct: Position-indent S-chunk ([body : Chunk]) #:transparent)
(struct: Concat          S-chunk ([chunks : (Listof Chunk)]) #:transparent)
(struct: Indent          S-chunk ([body : Chunk]
                                  [length : Integer]) #:transparent)
(struct: Speculative     S-chunk ([attempt : Chunk]
                                  [success? : ((Listof String) -> Boolean)]
                                  [backup : Chunk]) #:transparent)

(require/typed typed/racket
               [flatten ((Listof NestofChunks) -> (Listof Chunk))])

(: flatten* (NestofChunks * -> (Listof Chunk)))
(define (flatten* . lst)
    (flatten lst))

(provide (except-out (all-defined-out) mode indention line-length))

(define mode (make-parameter 'normal))
(define indention (make-parameter ""))
(define line-length (make-parameter 80))

(: make-whitespace (Integer -> String))
(define (make-whitespace n)
  (make-string n #\space))

; Removes TRAILING whitespace from the end of a line
(: remove-whitespace (String -> String))
(define (remove-whitespace line)
;  (string-trim line #:left? #f))
  (define: (last-non-whitespace-index [ii : Integer]) : Integer
    (let ([i (- ii 1)])
      (cond
        [(> 0 i) 0]
        [(not (equal? #\space (string-ref line i))) ii]
        [else (last-non-whitespace-index i)])))
  (let ([i (last-non-whitespace-index (string-length line))])
    (substring line 0 i)))
; Another tested alternative:
;  (let ([index (do: : Integer ([i (- (string-length line) 1) (- i 1)])
;                 ((or (> 0 i) (not (equal? #\space (string-ref line i))))
;                  (+ i 1)))])
;    (substring line 0 index)))

(: is-whitespace? (String -> Boolean))
(define (is-whitespace? line)
  (zero? (string-length (remove-whitespace line))))

(: finish-line (String -> String))
(define (finish-line given-line)
  (if (equal? given-line (indention))
      ""
      (remove-whitespace given-line)))

(: add-literal (String String -> (Listof String)))
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

(: add-space (String -> (Listof String)))
(define (add-space line)
  (if (or (equal? (mode) 'immediate)
          (< (string-length line) (line-length)))
      (list (string-append line " "))
      (list "" (finish-line line))))

(: add-concatenated ((Listof Chunk) String -> (Listof String)))
(define (add-concatenated chunks line)
  (for/fold: ([lines : (Listof String) (list line)])
    ([chunk : Chunk (in-list chunks)])
    (append (write-chunk chunk (car lines))
            (cdr lines))))

(: add-speculative ((List Chunk ((Listof String) -> Boolean) Chunk) String -> (Listof String)))
(define (add-speculative body line)
  (match-let* ([(list attempt success? backup) body]
               [attempted (write-chunk attempt line)])
    (if (success? attempted)
        attempted
        (write-chunk backup line))))

(: write-chunk (case->
                [Chunk -> (Listof String)]
                [Chunk String -> (Listof String)]))
(define write-chunk
  (case-lambda 
    [(chunk)
     (write-chunk chunk "")]
    [(chunk line)
     (define new-line (if (equal? line "")
                          (indention)
                          line))
     (match chunk
       [(and (? string?) ch)
        (add-literal ch new-line)]
       [(and (? symbol?) ch)
        (add-literal (symbol->string ch) new-line)]
       [(? exact-nonnegative-integer?)
        (add-space new-line)]
       [(Speculative attempt success? backup) 
        (add-speculative `(,attempt ,success? ,backup) new-line)]
       [(Indent body length)
        (parameterize ([indention (string-append (indention) (make-whitespace length))])
          (write-chunk body line))]
       [(Position-indent body) 
        (parameterize ([indention (make-whitespace (string-length line))])
          (write-chunk body line))]
       [(Concat chunks) 
        (add-concatenated chunks new-line)]
       [(Immediate body) 
        (parameterize ([mode 'immediate])
          (write-chunk body line))]
       [(S-chunk)
        (list "" (finish-line line))]
       )]))