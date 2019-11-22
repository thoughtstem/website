#lang racket

(provide render site-dir page-dir
         (rename-out [make-sub sub-site]))

(require "./page.rkt" 
         "./path-prefix.rkt"
         (only-in 2htdp/image 
                  image?
                  save-image))

(require scribble/html/xml)

(define site-dir (make-parameter #f))
(define page-dir (make-parameter #f))

(define should-write-prefix (make-parameter #t))

(define (write-prefix-file! out)
  (when (and (path-prefix) 
             (should-write-prefix))

    (define prefix-file
      (build-path out "site-prefix"))   

    (when (file-exists? prefix-file)
      (delete-file prefix-file))
    (displayln "Writing site prefix")
    (displayln (path-prefix))

    (with-output-to-file 
      prefix-file
      (thunk
        (display (path-prefix))))))


(struct sub (path site))
(define (make-sub sub-path site)
  ;Marks the site so that at render time, it will produce page paths with the sub-path cons'ed (push-path), and with the links rendered appropriately (with-prefix)
  (sub sub-path site))

(define (render site #:to output-dir)
  (write-prefix-file! output-dir)

  (parameterize ([site-dir (build-path output-dir)])
    (for ([p (flatten site)])
      (cond
        [(page? p) (render-page p output-dir)]
        [(sub?  p) (render-sub p output-dir)]
        [else (error "You can only render a list of pages or subs")]))))

(define (render-sub s output-dir)
  (define prefix (sub-path s)) 
  (define site   (sub-site s)) 

  ;Push the prefix on both the one that generates links and the page locations themselves
  (parameterize ([should-write-prefix #f])
    (with-prefix (~a (path-prefix) "/" prefix)
                 (render (push-path prefix site)
                         #:to output-dir))))

(define (render-page p output-dir)
  (define path-parts (page-path p))

  (define folder-parts (reverse (drop (reverse path-parts) 1)))

  (define path (apply build-path 
                      (cons output-dir path-parts))) 

  (define folder-path (apply build-path 
                             (cons output-dir folder-parts))) 


  (make-directory* folder-path)

  (parameterize
    ([page-dir folder-path])
    (with-output-to-file path 
                         #:exists 'replace
                         (thunk
                           (render-page-content p path)))))

(define (render-page-content p path)
  (define c (page-content p))
  (cond
    [(string? c) (displayln c)]
    [(image? c) (save-image c path)]
    [else (displayln 
            (xml->string 
              (preprocess (page-content p))))]))


(define (preprocess content)
  content)




