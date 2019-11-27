#lang racket

(provide navbar nav-item nav-link)

(require "../main.rkt")

(define (navbar #:brand (brand "BRAND/LOGO HERE")
                . content)
  (nav class: "navbar sticky-top navbar-expand-md navbar-dark bg-dark"
            (a href: (prefix/pathify "/index.html") 
               class: "navbar-brand" 
               brand) 
            (ul class: "navbar-nav ml-auto"
                content)))

(define (nav-item content)
  (li class: "nav-item"
      content))

(define (nav-link to text)
  (nav-item
    (a class: "nav-link mr-3" 
       href: (pathify (add-path-prefix to)) 
       text)))


