#lang racket

(provide link-to)

(require scribble/html/html
         "./page.rkt"
         "./path-prefix.rkt")


(define (link-to 
          #:element 
          (el
            (lambda (stuff) 
              (apply a 
                     (flatten stuff))))
          page text)
  (thunk
    (define path
      (cond 
        [(string? page) page]
        [(list? page) (string-join page "/")]
        [(page? page)
         (string-join (page-path page) "/")]
        [else (error "link-to can only take a page, string, or list of strings")]))

    (el
      (list
        'href: (~a 
          (if (path-prefix)
            (~a "/" (path-prefix)) 
            "")
          "/" path)
        text) )))




