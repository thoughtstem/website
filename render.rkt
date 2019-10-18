#lang racket

(provide render site-dir page-dir)

(require "./page.rkt")

(require scribble/html/xml)

(define site-dir (make-parameter #f))
(define page-dir (make-parameter #f))

(define (render site #:to output-dir)
  (parameterize ([site-dir (build-path output-dir)])
    (for ([p site])
      (define path-parts (page-path p))  

      (define folder-parts (reverse (drop (reverse path-parts) 1)))

      (define path (apply build-path 
                          (cons output-dir path-parts))) 
      (define folder-path (apply build-path 
                                 (cons output-dir folder-parts))) 


      (make-directory* folder-path)

      (parameterize
        ([page-dir folder-path])
        (with-output-to-file path 
                             #:exists 'replace
                             (thunk
                               (define c (page-content p))

                               (if (string? c)
                                 (displayln c)
                                 (displayln 
                                   (xml->string 
                                     (preprocess (page-content p)))))))))))


(define (preprocess content)
  content)

