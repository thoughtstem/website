#lang at-exp racket

(provide bootstrap
         content
;         container
         row
         responsive-row
         bootstrap-files
         include-bootstrap-js
         include-bootstrap-css
         include-p5-js
         
         (all-from-out "./bootstrap/buttons.rkt")
         (all-from-out "./bootstrap/modal.rkt")
         (all-from-out "./bootstrap/cards.rkt")
         (all-from-out "./bootstrap/badges.rkt")
         (all-from-out "./bootstrap/col.rkt")
         (all-from-out "./bootstrap/tabs.rkt")
         (all-from-out "./bootstrap/nav.rkt")
         (all-from-out "./bootstrap/accordion.rkt")
         (all-from-out "./main.rkt"))

(require (except-in "./main.rkt" col)
         "./define-extensible-element.rkt"
         "./bootstrap/buttons.rkt"
         "./bootstrap/cards.rkt"
         "./bootstrap/badges.rkt"
         "./bootstrap/modal.rkt"
         "./bootstrap/col.rkt"
         "./bootstrap/tabs.rkt"
         "./bootstrap/nav.rkt"
         "./bootstrap/accordion.rkt"
         "./util.rkt"
         "./path-prefix.rkt"
         racket/runtime-path)

(define-runtime-path js "bootstrap/js")
(define-runtime-path css "bootstrap/css")


(define (include-bootstrap-js)
  (list 
    (include-js "/js/bootstrap.bundle.min.js")))

(define (include-bootstrap-css #:defer [defer #f])
  (if defer
      (list
       (include-deferred-css "/css/bootstrap.min.css"))
      (list 
       (include-css "/css/bootstrap.min.css"))))

(define (include-font-awesome-js)
  (list 
    (include-js "/js/font-awesome.js" #:tag 'async)))

;TODO: include-font-awesome-css

(define (include-p5-js)
  (list 
    (include-js "/js/p5.min.js" #:tag 'async)))

(define (content #:head (head-content '()) 
                 .  body-content)
  (html
    (head
      ;head-content
      (include-bootstrap-css #:defer #t)
      (meta 'charset: "utf-8")
      (meta name: "viewport"
            content: "width=device-width, initial-scale=1, shrink-to-fit=no")
      head-content
      )

    (body
      #;
      (include-js "/js/jquery-3.2.1.slim.min.js")
      (include-js "/js/jquery-3.4.1.min.js")
      (include-js "/js/moment.min.js" #:tag 'async)
      body-content
      (include-bootstrap-js)
      (script/inline 
        @~a{
          $(function () {
            $('[data-toggle="tooltip"]').tooltip()
          })
        })
      
      (include-font-awesome-js))))

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

(define/provide-extensible-element 
  carousel-inner
  div
  (class: "carousel-inner" class-join))

(define/provide-extensible-element 
  carousel-item
  div
  (class: "carousel-item" class-join))


(define (bootstrap-files)
  (list
    (page js/moment.min.js
          (file->string (build-path js "moment.min.js")))

    (page ;js/jquery-3.2.1.slim.min.js 
          js/jquery-3.4.1.min.js
          (file->string 
            (build-path js "jquery-3.4.1.min.js")

            #;
            (build-path js "jquery-3.2.1.slim.min.js")))
    
    (page js/font-awesome.js 
          (file->string (build-path js "font-awesome.js")))

    (page js/p5.min.js 
          (file->string (build-path js "p5.min.js")))

    (page js/bootstrap.bundle.min.js 
          (file->string (build-path js "bootstrap.bundle.min.js")))

    (page css/bootstrap.min.css 
          (file->string 
            (build-path css "bootstrap.min.css")))))

(define (bootstrap site)
  (append
    (flatten site)
    (bootstrap-files)))

(define (responsive-row #:columns columns #:padding [padding 3] . items)
  (define row-size (max 1 (min 12 (exact-round (/ 12 columns)))))
  (apply row (map (curry div class: (~a "col-lg-" row-size
                                        " col-sm-6 col-xs-12 my-3 px-" padding)) items)))



