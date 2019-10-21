#lang racket

(provide bootstrap
         content
         navbar
;         container
         row
         nav-item
         nav-link
         bootstrap-files
         include-bootstrap-js
         include-bootstrap-css
         (all-from-out "./bootstrap/buttons.rkt")
         (all-from-out "./bootstrap/cards.rkt")
         (all-from-out "./bootstrap/col.rkt")
         (all-from-out "./main.rkt"))

(require (except-in "./main.rkt" col)
         "./define-extensible-element.rkt"
         "./bootstrap/buttons.rkt"
         "./bootstrap/cards.rkt"
         "./bootstrap/col.rkt"
         "./util.rkt"
         (only-in "./link-to.rkt" add-path-prefix)
         racket/runtime-path)

(define-runtime-path js "bootstrap/js")
(define-runtime-path css "bootstrap/css")


(define (include-bootstrap-js)
  (list 
    (include-js (add-path-prefix "/js/jquery-3.2.1.slim.min.js"))
    (include-js (add-path-prefix "/js/bootstrap.bundle.min.js"))))

(define (include-bootstrap-css)
  (list 
    (include-css (add-path-prefix "/css/bootstrap.min.css"))))

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
            (a href: (add-path-prefix "/index.html") 
               class: "navbar-brand" 
               brand) 
            (ul class: "navbar-nav mr-auto"
                content))))

(define (nav-item content)
  (li class: "nav-item"
      content))

(define (nav-link to text)
  (nav-item
    (a class: "nav-link" 
       href: (pathify (add-path-prefix to)) 
       text)))


#;
(define (container . content)
  (div class: "container"
       content))

(define/provide-extensible-element
  container
  div
  [class: "container" class-join])

(define (row . content)
  (div class: "row"
       content))

(define (bootstrap-files)
  (list
    (page js/jquery-3.2.1.slim.min.js 
          (file->string (build-path js "jquery-3.2.1.slim.min.js")))

    (page js/bootstrap.bundle.min.js 
          (file->string (build-path js "bootstrap.bundle.min.js")))

    (page css/bootstrap.min.css 
          (file->string 
            (build-path css "bootstrap.min.css")))))

(define (bootstrap site)
  (append
    site
    (bootstrap-files)))



