#lang racket

(require "fulmar-core.rkt")
(require "standard-chunk.rkt") ; for eval

;input/output structures, procedures, and contracts for fulmar

;manage port or location
(define (manage-port/location! port/location port? proc open-port close-port)
  (cond [(port? port/location) (proc port/location)]
        [(string? port/location)
         (let* ([port (open-port (path->complete-path (string->path port/location)))]
                [result (proc port)])
                (close-port port)
                result)]
        [else (error "Not a port or a file name: " port/location)]))
;(provide manage-port/location!)

;Open output port
; - saves a simple backup
(define (open-output! location)
  ; Overwrites old backup file (if exists) without backing up the backup...
  (if (file-exists? location)
      (rename-file-or-directory location
                                (string->path (string-append (path->string location) "~r~"))
                                #true)
      (void))
  (open-output-file location #:exists 'replace))
;(provide open-output!)

;Print written code with port
(define (print-code-with-port! lines port)
  (if (null? lines)
      (void)
      (begin (print-code-with-port! (rest lines) port)
             (displayln (first lines) port))))
;(provide print-code-with-port!)

;Print written code
(define (print-code! lines port/location)
  (manage-port/location! port/location
                         output-port?
                         (λ (port) (print-code-with-port! lines port))
                         open-output!
                         close-output-port))
(provide print-code!)

;open input port
(define (open-input! location)
  (if (not (file-exists? location))
      (error "Could not open file at: " location)
      (open-input-file location)))
;(provide open-input!)

;read file with given port
(define (read-file-with-port! port)
  (let ([input (read port)])
    (if (eof-object? input)
        null
        (cons input
              (read-file-with-port! port)))))
;(provide read-file-with-port!)

;read file
(define (read-file! port/location)
  (manage-port/location! port/location
                         input-port?
                         read-file-with-port!
                         open-input!
                         close-input-port))
;(provide read-file!)

;clean input of everything but chunks
(define (clean-input input)
  (foldl (λ (next chunks)
            (if (chunk? next)
                (cons next chunks)
                chunks))
         null
         (flatten* input)))
;(provide clean-input)

;TODO: change this namespace (and/or namespace-anchor) to only include standard chunks
;defintions needed for eval in read-chunks
(define-namespace-anchor fulmar-chunk-namespace-anchor)
;(provide fulmar-namespace-anchor)
(define fulmar-chunk-namespace (namespace-anchor->namespace fulmar-chunk-namespace-anchor))
;(provide fulmar-namespace)

;eval for fulmar
(define (fulmar-eval chunk)
  (eval chunk fulmar-chunk-namespace))
;(provide fulmar-eval

;read and evaluate chunks from file
(define (read-eval-chunks! port/location alt-location)
  (let ([source (read-file! port/location)])
    (parameterize ([current-directory alt-location])
      (reverse (clean-input (map fulmar-eval source))))))
(provide read-eval-chunks!)

;copy file directly from a port
(define (copy-file-from-port! port)
  (let ([input (read-line port)])
    (if (eof-object? input)
        null
        (cons input
              (copy-file-from-port! port)))))
;(provide copy-file-from-port!)

;copy file
(define (copy-file! port/location)
  (concat (manage-port/location! port/location
                                 input-port?
                                 copy-file-from-port!
                                 open-input!
                                 close-input-port)))
(provide copy-file!)
