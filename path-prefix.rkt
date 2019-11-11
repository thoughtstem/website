#lang racket 

(provide path-prefix
         with-prefix
         add-path-prefix
         pathify)

(define path-prefix (make-parameter #f))

(define (pathify path)
  (cond
    [(string? path) 
     (if (string-prefix? path "/")
       path
       (if (or
             (string-prefix? path "http:")
             (string-prefix? path "https:"))
         path
         (~a "/" path))

       #;
       (~a "/" path))]
    [(list? path) (~a "/" (string-join path "/"))]))

(define (add-path-prefix path)
  (if (not (path-prefix))
    path
    (cond
      [(string? path) 
       (~a "/" (path-prefix) "/" path)]
      [(list? path) (cons (path-prefix) path)])))

(define-syntax-rule (with-prefix prefix do-stuff ...)
  (parameterize ([path-prefix prefix])
    do-stuff ...))
