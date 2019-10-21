#lang racket

(provide link-to
         path-prefix
         with-prefix
         add-path-prefix
         pathify)

(require scribble/html/html
         "./page.rkt")

(define path-prefix (make-parameter #f))

(define (pathify path)
  (cond
      [(string? path) path]
      [(list? path) (~a "/" (string-join path "/"))]))

(define (add-path-prefix path)
  (if (not (path-prefix))
    path
    (cond
      [(string? path) (~a "/" (path-prefix) "/" path)]
      [(list? path) (cons (path-prefix) path)])))

(define-syntax-rule (with-prefix prefix do-stuff ...)
  (parameterize ([path-prefix prefix])
    do-stuff ...))

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




