#lang racket

(require website)

(provide my-site)


(define index
  (page (list "index.html")
        (html
          (body
            (h1 "HI")
            #;
            (a href: "hi")
            (make-element 'a `(,(cons 'href "/blog/post1.html")) "Blog Post 1") 
            ))))

(define blog-post1
  (page (list "blog" "post1.html")
        (html
          (body
            (h1 "This is a post")))))

(define my-site
  (list index blog-post1))



