#lang racket

(require scribble/manual
         scribble/racket
         racket/stream
         racket/dict
         "doc-scraper.rkt")

(provide gen-doc)

(define (gen-doc filepath)
  (dict-map (crush (stream-filter fancy-filter
                                  (read-sym-stream filepath
                                                   (open-input-file filepath))))
            (λ (k es) (cons
                       (subsection (symbol->string k))
                       (map (λ (e)
                              (match (syntax->datum e)
                                [`(document ,_ . ,ps) (print "hi!") (map para ps)]
                                [_ (para (to-element e))])) es)))))