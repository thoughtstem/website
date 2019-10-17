#lang racket

(provide render)

(require "./page.rkt")

(require scribble/html/xml)

(define (render site #:to output-dir)
  (for ([p site])
    (define path-parts (page-path p))  

    (define folder-parts (reverse (drop (reverse path-parts) 1)))

    (define path (apply build-path 
                        (cons output-dir path-parts))) 
    (define folder-path (apply build-path 
                               (cons output-dir folder-parts))) 


    (make-directory* folder-path)

    (with-output-to-file path 
                         #:exists 'replace
      (thunk
         (displayln 
           (xml->string (page-content p)))))))

