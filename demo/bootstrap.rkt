#lang racket

(require website 
         web-server/templates
         (only-in 2htdp/image
                  circle))

(provide my-site)


(define (blog-page title . stuff)
  (html
    (body
      (a 'href: "/" "Back Home")
      title
      stuff)))

(define my-site
  (site
    (jquery (page js/jquery-3.2.1.slim.min.js 
                  (file->string "js/jquery-3.2.1.slim.min.js")))

    (bootstrap (page js/bootstrap.bundle.min.js 
                     (file->string "js/bootstrap.bundle.min.js")))
    (bootstrap-css 
      (page css/bootstrap.min.css 
            (include-template 
              #:command-char #\$
              "css/bootstrap.min.css")   
            #;
            (file->string "css/bootstrap.min.css")))
    (index
      (page index.html
            (html
              (head
                (link 'rel: "stylesheet" 'type: "text/css" 'href: "/css/bootstrap.min.css"))
              (body
                (h1 "HI")
                (div 'class: "container" 
                     (p "bootstrap demo"))
                (script 'src: "/js/jquery-3.2.1.slim.min.js")
                (script 'src: "/js/bootstrap.bundle.min.js")))))))


;TODO:
;  * Add missing attributes, src:, href:, etc...
;  Translate a relevant template, make some starters 
; view-source:https://getbootstrap.com/docs/4.0/examples/cover/#
;  Get templates to work?   Wow... taht was easy
