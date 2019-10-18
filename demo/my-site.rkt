#lang racket

(require website 
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
    (index
      (page index.html
            (html
              (body
                (h1 "HI")
                (link-to blog-post1 
                         "Blog Post 1 yay")))))

    (blog-post1
      (page blog/first-post.html
            (blog-page
              (h1 "This is a post!")    
              (write-image (circle 40 'solid 'red))
              (link-to blog-post2 "Second post"))))

    (blog-post2
      (page blog/second-post.html
            (blog-page
              (h1 "This is a post!")     
              (write-image (circle 40 'solid 'green))
              (link-to blog-post3 "Last post..."))))

    (blog-post3
      (page blog/third-post.html
            (blog-page
              (h1 "This is a post!")       
              (write-image (circle 40 'solid 'blue))
              (link-to blog-post1 "Back to beginning")) ))))


;TODO:
;  * Add missing attributes, src:, href:, etc...



