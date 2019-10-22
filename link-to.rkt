#lang racket

(provide link-to)

(require scribble/html/html
         "./page.rkt"
         "./path-prefix.rkt")


;A thunk so this works with the letrec implied by (site ...)
(define-syntax-rule (link-to page text)
  (thunk
    (define path
      (cond 
        [(string? page) page]
        [(list? page) (string-join page "/")]
        [(page? page)
         (string-join (page-path page) "/")]
        [else (error "link-to can only take a page, string, or list of strings")]))

    (a 'href: 
       (~a 
         (if (path-prefix)
           (~a "/" (path-prefix)) 
           "")
         "/" path)
       text)))




