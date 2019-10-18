#lang racket

(provide bootstrap
         content
         navbar
         container
         row
         nav-item
         nav-link
         (all-from-out "./bootstrap/buttons.rkt")
         (all-from-out "./bootstrap/cards.rkt")
         (all-from-out "./bootstrap/col.rkt")
         (all-from-out "./main.rkt"))

(require (except-in "./main.rkt" col)
         "./bootstrap/buttons.rkt"
         "./bootstrap/cards.rkt"
         "./bootstrap/col.rkt"
         "./util.rkt"
         racket/runtime-path)

(define-runtime-path js "bootstrap/js")
(define-runtime-path css "bootstrap/css")

(define (include-bootstrap-js)
  (list 
    (include-js "/js/jquery-3.2.1.slim.min.js")
    (include-js "/js/bootstrap.bundle.min.js")))

(define (include-bootstrap-css)
  (list 
    (include-css "/css/bootstrap.min.css")))

(define (content #:head (head-content '()) 
                           .  body-content)
  (html
    (head
      head-content
      (include-bootstrap-css))

    (body
      body-content
      (include-bootstrap-js))))

(define (navbar #:brand (brand "BRAND/LOGO HERE")
                . content)
  (nav class: "navbar navbar-expand-md navbar-dark bg-dark"
    (div class: "container"
      (a href: "/" class: "navbar-brand" 
         brand) 
      (ul class: "navbar-nav mr-auto"
        content))))

(define (nav-item content)
  (li class: "nav-item"
      content))

(define (nav-link to text)
  (nav-item
    (a class: "nav-link" 
       href: to 
       text)))


(define (container . content)
  (div class: "container"
       content))

(define (row . content)
  (div class: "row"
       content))


(define (bootstrap site)
  (append
    site
    (list
      (page js/jquery-3.2.1.slim.min.js 
            (file->string (build-path js "jquery-3.2.1.slim.min.js")))

      (page js/bootstrap.bundle.min.js 
            (file->string (build-path js "bootstrap.bundle.min.js")))

      (page css/bootstrap.min.css 
            (file->string 
              (build-path css "bootstrap.min.css"))))))



