#lang racket

(provide define-extensible-element
         define/provide-extensible-element
         replace)

(require "./attr.rkt")

(define (replace a b)
  (or b a))

(define-syntax-rule (define-extensible-element 
                      name 
                      base-type
                      (attr: default joiner)
                      ... )
  (define (name #:element (elem #f) . content)
    (apply (or elem base-type)
           (flatten
             (list
               (list attr: (joiner 
                             default
                             (get-attr attr: content)))
               ... 

               content
	       )))))

(define-syntax-rule (define/provide-extensible-element name base settings ...)
  (begin
    (provide name) 
    (define-extensible-element 
      name 
      base 
      settings ...)))
