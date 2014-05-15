#lang racket

(require racket/sandbox)
(require fulmar/version)

(provide (all-defined-out))

(define (hidden? path)
  (ormap (λ (path-element) 
           (let* ((as-string (path->string path-element))
                  (first-character (string-ref as-string 0)))
             (eqv? first-character #\.)))
         (explode-path path)))

(define (find-fmr root-dir)
  (let* ((relative-paths (map ((curry find-relative-path) root-dir)
                              (sequence->list (in-directory root-dir))))
         (non-hidden-paths (filter (negate hidden?) relative-paths)))
    (filter (λ (path) (equal? (filename-extension path) #"fmr")) non-hidden-paths)))

(define (find-generated fmr-path)
  (let ((trimmed (string->path (string-trim (path->string fmr-path) ".fmr"))))
    (if (filename-extension trimmed)
        trimmed
        #f)))

(define (evaluate-fmr fmr-path import-paths)
  (parameterize ([sandbox-output 'string]
                 [sandbox-memory-limit 1000]
                 [sandbox-eval-limits '(60 1000)]
                 [compile-enforce-module-constants #f])
    (string-append
     ; This hack is required because we print the version info with a configure-runtime submodule, which won't run in the 
     generated-string
     (get-output (make-module-evaluator fmr-path #:allow-for-require import-paths)))))