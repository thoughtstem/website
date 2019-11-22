#lang at-exp racket

(provide bootstrap
         content
         navbar
;         container
         row
         responsive-row
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
         "./path-prefix.rkt"
         racket/runtime-path)

(define-runtime-path js "bootstrap/js")
(define-runtime-path css "bootstrap/css")


(define (include-bootstrap-js)
  (list 
    (include-js "/js/bootstrap.bundle.min.js" )))

(define (include-bootstrap-css)
  (list 
    (include-css "/css/bootstrap.min.css" )))

(define (include-font-awesome-js)
  (list 
    (include-js "/js/font-awesome.js" )))

(define (content #:head (head-content '()) 
                 .  body-content)
  (html
    (head
      head-content
      (include-bootstrap-css))

    (body
      (include-js "/js/jquery-3.2.1.slim.min.js")
      body-content
      (include-bootstrap-js)
      (script/inline 
        @~a{
          $(function () {
            $('[data-toggle="tooltip"]').tooltip()
          })
        })
      
      (include-font-awesome-js))))

(define (navbar #:brand (brand "BRAND/LOGO HERE")
                . content)
  (nav class: "navbar sticky-top navbar-expand-md navbar-dark bg-dark"
            (a href: (prefix/pathify "/index.html") 
               class: "navbar-brand" 
               brand) 
            (ul class: "navbar-nav ml-auto"
                content))
       )

(define (nav-item content)
  (li class: "nav-item"
      content))

(define (nav-link to text)
  (nav-item
    (a class: "nav-link mr-3" 
       href: (pathify (add-path-prefix to)) 
       text)))


(define/provide-extensible-element
  container
  div
  [class: "container" class-join])

(define/provide-extensible-element 
  row
  div
  (class: "row" class-join))

(define/provide-extensible-element 
  jumbotron
  div
  (class: "jumbotron" class-join))

(define/provide-extensible-element 
  carousel
  div
  (class: "carousel" class-join))

(define (bootstrap-files)
  (list
    (page js/jquery-3.2.1.slim.min.js 
          (file->string (build-path js "jquery-3.2.1.slim.min.js")))
    
    (page js/font-awesome.js 
          (file->string (build-path js "font-awesome.js")))

    (page js/bootstrap.bundle.min.js 
          (file->string (build-path js "bootstrap.bundle.min.js")))

    (page css/bootstrap.min.css 
          (file->string 
            (build-path css "bootstrap.min.css")))))

(define (bootstrap site)
  (append
    (flatten site)
    (bootstrap-files)))

(define (responsive-row #:columns columns . items)
  (define row-size (max 1 (min 12 (exact-round (/ 12 columns)))))
  (apply row (map (curry div class: (~a "col-lg-" row-size
                                        " col-sm-6 col-xs-12 my-3")) items)))



