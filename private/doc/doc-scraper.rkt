#lang racket/base
(require racket/stream
         racket/match
         racket/dict
         )

(provide read-sym-stream
         fancy-filter
         crush)

(define (sym-stream name port)
  (let ([next-sym (read-syntax name port)])
    (if (eof-object? next-sym)
        empty-stream
        (stream-cons next-sym (sym-stream name port)))))

(define (read-sym-stream name port)
  (read-language port)
  (sym-stream name port))

(define (fancy-filter e)
  (match (syntax->datum e)
    [`(: . ,_) #t]
    [`(define-type . ,_) #t]
    [`(struct: . ,_) #t]
    [`(document . ,_) #t]
    [_ #f]))

(define (crush sym-str)
  (stream-fold
   (λ (d s)
     (match (syntax->datum s)
       [`(,_ ,k . ,_) (dict-set d k (cons s (dict-ref d k '())))]
       [_ d]))
   '() ; Replace with any empty dictionary type
   sym-str))
