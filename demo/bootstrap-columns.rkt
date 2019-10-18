#lang racket

(require website/bootstrap)

(define nav
  (navbar
    #:brand "Stephen R. Foster"
    (nav-item 
      (nav-link "/about.html"
                "About"))))


(define (normal-content title . stuff)
  (content
    nav 
    (container
      (h1 title)
      stuff)))


(define my-site
  (bootstrap
    (site
      (index
        (page index.html
              (normal-content
                "Welcome"

                (row
                  (col style: "background-color: red"  "1") 
                  (col-6 style: "background-color: green" "2") 
                  (col style: "background-color: blue" "3"))
                
                (row
                  (col style: "background-color: red"  "1") 
                  (col-5 style: "background-color: green" "2") 
                  (col style: "background-color: blue" "3"))


                (row
                  (col-6 class: "col-md-4" style: "background-color: red"  "1") 
                  (col-6 class: "col-md-4" style: "background-color: green"  "1") 
                  (col-6 class: "col-md-4" style: "background-color: blue"  "1") 
                  )

                )))

      (about
        (page about.html
              (normal-content
                "About"))))))


(render my-site #:to "out")
