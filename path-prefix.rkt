#lang racket 

(provide path-prefix
         with-prefix
         add-path-prefix
         pathify
         prefix/pathify)

(define path-prefix (make-parameter #f))

(define (simplify-slashes s)
  (regexp-replaces s
                   `([#rx"//" "/"])))

(define (prefix/pathify p)
  (pathify
    (add-path-prefix p)))

;Crunches lists down to strings.
(define (pathify path)
  (simplify-slashes
    (cond
      [(string? path) 
       (if (string-prefix? path "/")
         path
         (~a "/" path))]
      [(list? path) (~a "/" (string-join path "/"))])))

;Keeps paths as lists (if they are), but pushes the prefix
(define (add-path-prefix path)
  (if (not (path-prefix))
    path
    (cond
      [(string? path) (~a "/" (path-prefix) "/" path)]
      [(list? path) (cons (path-prefix) path)])) )

(define-syntax-rule (with-prefix prefix do-stuff ...)
  (parameterize ([path-prefix prefix])
    do-stuff ...))



