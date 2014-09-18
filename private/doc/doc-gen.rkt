#lang racket

(require scribble/manual
         scribble/racket
         racket/stream
         racket/dict
         "doc-scraper.rkt")

(provide gen-doc doc-gen)

(define (gen-doc filepath)
  (dict-map (crush (stream-filter fancy-filter
                                  (read-sym-stream filepath
                                                   (open-input-file filepath))))
            (λ (k es) (cons
                       (subsection (symbol->string k))
                       (map (λ (e)
                              (match (syntax->datum e)
                                [`(document ,_ . ,ps) (map para ps)]
                                [_ (para (to-element e))])) es)))))

(define (doc-gen filename)
  (let-values ([(path) (current-directory)]
               [(parent x y) (split-path (current-directory))])
    (if (file-exists? (build-path path filename))
         (gen-doc (build-path path filename))
         (gen-doc (build-path parent filename)))))
