#lang racket

(provide bootstrap
         content
         navbar
         container
         row
         nav-item
         nav-link
         include-js
         include-css
         (all-from-out "./bootstrap/col.rkt")
         (all-from-out "./main.rkt"))

(require (except-in "./main.rkt" col)
         "./bootstrap/col.rkt"
         racket/runtime-path)

(define-runtime-path js "bootstrap/js")
(define-runtime-path css "bootstrap/css")

(define (include-js)
  (list 
    (script 'src: "/js/jquery-3.2.1.slim.min.js")
    (script 'src: "/js/bootstrap.bundle.min.js")))

(define (include-css)
  (list 
    (link 'rel: "stylesheet" 'type: "text/css" 'href: "/css/bootstrap.min.css")  ))

(define (content #:head (head-content '()) 
                           .  body-content)
  (html
    (head
      head-content
      (include-css))

    (body
      body-content
      (include-js))))

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



