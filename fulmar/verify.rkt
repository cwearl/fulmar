#lang racket

(require racket/sandbox)
(require fulmar/version)

(define root-dir
  (normalize-path
   (command-line
    #:program "verify"
    #:usage-help "apply fulmar to all .fmr files in a directory (recursing through non-hidden subdirectories) and compare the output to corresponding .h and .cpp files for testing."
    #:args (filename)
    filename)
   ))

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
    (filter (λ (path) (equal? (filename-extension path) #"fmr")) non-hidden-paths)
    )
  )

(define (find-generated fmr-path)
  (let ((trimmed (string->path (string-trim (path->string fmr-path) ".fmr"))))
    (if (filename-extension trimmed)
        trimmed
        #f)))


(module+ main
  
  (define fmr-files (map ((curry build-path) root-dir) (find-fmr root-dir)))
  (define top-fmr-files (filter find-generated fmr-files))
  
  (for ([fmr top-fmr-files])
    (let* ((relative-fmr-string (path->string (find-relative-path root-dir fmr)))
           (comparison-file (find-generated fmr))
           (comparison (file->string comparison-file))
           (output (parameterize ([sandbox-output 'string]
                                  [sandbox-memory-limit 1000]
                                  [sandbox-eval-limits '(60 1000)]
                                  [compile-enforce-module-constants #f])
                     (get-output (make-module-evaluator fmr #:allow-for-require fmr-files))))
           
           ; This hack is required because we print the version info with a configure-runtime submodule, which won't run in the sandbox.
           (output-with-header (string-append generated-string output))
           (matched (equal? output-with-header comparison)))
      
      (displayln (format "~a: ~a" (if matched "matched" "FAILED") relative-fmr-string)))))



;  (let* ((tmp (make-temporary-file))
;         (out (open-output-file tmp #:exists 'truncate)))
;    (display output-with-header out)
;    (display tmp)
;    (display comparison-file)
;  )
