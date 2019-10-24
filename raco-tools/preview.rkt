#lang racket
(require web-server/servlet
         web-server/servlet-env)
(require web-server/private/mime-types)

(define prefix-file (build-path "site-prefix"))

(define path-prefix 
  (if (file-exists? prefix-file)
   (file->string prefix-file)   
   #f))


(define run-dir (current-directory))

(define (welcome)
  (response/xexpr 
    `(html (head (title "Starter"))
           (body (p "Welcome to website-preview")
                 (a ([href ,(if (not path-prefix)
                              (~a "index.html")
                              (~a "/" path-prefix "/index.html")) ])
                    "Click to go to my site")))))

(define (serve-file path)

  (define last-part (~a (last path)))

 (response/full
   200 #"OK"
   (current-seconds) (cond 
                       [(string-contains? last-part ".png") #"image/png"]
                       [(string-contains? last-part ".svg") #"image/svg+xml"]
                       [else
                         TEXT/HTML-MIME-TYPE] )
   '() ;Headers
   (list 
     (if (string-contains? last-part ".png")
       (file->bytes (build-path 
                      run-dir 
                      (apply build-path path))) 
       (string->bytes/utf-8
         (file->string (build-path 
                         run-dir 
                         (apply build-path path))))))) )

(define (req->path req)
  (define url (request-uri req)) 

  (define path 
    (map path/param-path (url-path url)))

  (when path-prefix
    (set! path (rest path)))

  (when (string=? (first path)
                  "website-preview")
    (set! path (rest path)))

  (filter-not 
    (curry string=? "")
    path))

(define (my-app req)
  (define file-path (req->path req))
  (if (not (empty? file-path))
    (serve-file file-path)
    (welcome)))


(if path-prefix
  (serve/servlet my-app
                 #:servlet-path (~a "/" path-prefix "/")
                 #:servlet-regexp (regexp (~a path-prefix ".*"))
                 #:extra-files-paths
                 (list (build-path "./")))
  (serve/servlet my-app
                 #:servlet-path (~a "/website-preview")
                 #:extra-files-paths
                 (list (build-path "./"))))
