#lang racket

(provide (except-out (struct-out page)
                     page) 
         (rename-out [make-page page])
         site) 

(struct page (path content))

(define-syntax-rule (make-page path content)
  (page (string-split (~a 'path) "/")
        content))

(define-syntax-rule (site [id p] ...)
  (letrec ([id p] ...)
    (list id ...)))


