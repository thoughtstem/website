#lang racket
(require reloadable
	 web-server/servlet
	 web-server/servlet-env
	 webapp/server/util/responses
	 (only-in website-js container page-content page-path prefix/pathify page? element?)
	 (only-in website/bootstrap website-bootstrap-path)
	 net/url-structs)

(define site (reloadable-entry-point->procedure
	       (make-reloadable-entry-point 'site "./main.rkt")))

#;
(define static-site (reloadable-entry-point->procedure
		      (make-reloadable-entry-point 'static-site "./main.rkt")))
(reload!)


;Cache
(define time-of-last-request (current-milliseconds))
(define cache (site))

(define (get-from-cache path)
  (if (>= (- (current-milliseconds) time-of-last-request) 1000)
    (begin
      (set! time-of-last-request (current-milliseconds))
      (set! cache (site)))    
    (begin
      (set! time-of-last-request (current-milliseconds))))
  
  (find-page path cache))


(define (welcome r)
  (define path 
    (string-join
      (map path/param-path
	   (url-path
	     (request-uri r)))
      "/"))

  (displayln path)

  ;Mabye a better fix is to create a short lived cache,
  ;  So the html page load hits (site) and caches,
  ;  and calls to embedded things like pngs/js/css hit the cache.

  ;Then we can nix the (site) (static-site) distinction, which is 
  ;  a bit of an abstraction leak.
  (define p
    (get-from-cache path)
    #;
   (if (or
	 (string-suffix? (~a path) ".js")
	 (string-suffix? (~a path) ".css"))
       (find-page path (static-site))
       (find-page path (site))
       ))

  (define content
    (and p (page-content p)))

  (cond
    [(not p) (response/string "Not found")] 
    [(element? content)
     (response/html/content content)]
    [(path? content)
     (serve-file content)
     #; 
     (response/string
       (file->string content))])
  )

(define (serve-file path)
  (define last-part (~a (last (explode-path path))))

  (response/full
    200 #"OK"
    (current-seconds) (cond
			[(string-contains? last-part ".png") #"image/png"]
			[(string-contains? last-part ".jpg") #"image/jpeg"]
			[(string-contains? last-part ".svg") #"image/svg+xml"]
			[(string-contains? last-part ".css") #"text/css"]
			[(string-contains? last-part ".js")  #"text/js"]
			[else
			  TEXT/HTML-MIME-TYPE] )
    '() ;Headers
    (list
      (if (or (string-contains? last-part ".png")
	      (string-contains? last-part ".jpg"))
	  (file->bytes path)
	  (string->bytes/utf-8
	    (file->string path))))))


(define (response/string s)
  (response/full
    200 #"OK"
    (current-seconds) TEXT/HTML-MIME-TYPE
    (list )
    (list 
      (string->bytes/utf-8 s))))

(define (find-page path site)
  (findf 
    (lambda (p)
      (define path2
	(if (page? p) ;subsites break this
	    (page-path p)
	    #f))

      (and path2
	   (string=?
	     path
	     (string-join
	       (map ~a path2)
	       "/")) ))
    (flatten site)))



(module+ main
	 (serve/servlet welcome
			#:port 8081
			#:servlet-regexp #rx""
			#:servlet-path "/index.html"
			#:extra-files-paths
			(list website-bootstrap-path)))
