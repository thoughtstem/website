#lang racket
(require web-server/servlet
         web-server/servlet-env)
 
(define (my-app req)
    (response/xexpr 
      `(html (head (title "Starter"))
             (body (p "Welcome to website-preview")
                   (a ([href "/index.html"])
                      "Click to go to my site")))))
 
(serve/servlet my-app
               #:server-root-path
               (build-path "./")  
               #:extra-files-paths
               (list
                 (build-path "./")))
