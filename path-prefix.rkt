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
       (if (has-protocol? path) 
         path
         (~a "/" path))

       #;
       (~a "/" path))]
    [(list? path) (~a "/" (string-join path "/"))])))

(define (has-protocol? p)
 (cond
  [(string? p) 
   (or
    (string-prefix? p "http:")
    (string-prefix? p "https:"))]
  [(list? p) (has-protocol? (first p))]
  [else (error "Paths must be strings or lists")]))

;Keeps paths as lists (if they are), but pushes the prefix
(define (add-path-prefix path)
  (if (or (not (path-prefix))
          (has-protocol? path))
    path
    (cond
      [(string? path) (~a "/" (path-prefix) "/" path)]
      [(and (list? path) 
            (not (has-protocol? (first path))))
        (cons (path-prefix) path)]
      [else path])))

(define-syntax-rule (with-prefix prefix do-stuff ...)
  (parameterize ([path-prefix prefix])
    do-stuff ...))


