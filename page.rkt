#lang racket

(provide (except-out (struct-out page)
                     page) 
         (rename-out [make-page page])
         push-path
         site
         page->path
	 change-page-path) 

(require syntax/parse/define
         (for-syntax racket/string))

(struct page (path content) #:transparent)

(define-syntax (make-page stx)
  (syntax-parse stx
    ;Usually it's (list parts ...), but we'll count anything starting with a paren as stuff to evaluate the normal way
    [(_ (usually-list parts ...) content)
     #'(page (usually-list parts ...) content)]
    [(_ id content)
     (define id-string 
       (symbol->string (syntax->datum #'id)))
     (if (or (string-contains? id-string ".")
             (string-contains? id-string "/"))
       #`(page '#,(string-split id-string "/") content)
       #'(page id content))]))

(define-syntax-rule (site [id p] ...)
  (letrec ([id p] ...)
    (list id ...)))


(define (page->path p)
  (string-join 
    (page-path p)
    "/"))

(define (change-page-path p new-path)
  (struct-copy p page
	       [path new-path]))

(module+ test
  (require rackunit)

  (define content "content")

  (check-equal?
    (make-page index.html 
               content)
    (page (list "index.html")
          content))

  (check-equal?
    (make-page dir/index.html
               content)
    (page (list "dir" "index.html")
          content))

  (define index-location (list "dir" "index.html"))
  (check-equal?
    (make-page index-location 
               content)
    (page (list "dir" "index.html")
          content))
  )


(define (push-path part p)

  (if (list? p)
    (map (curry push-path part) p)
    (let ()
      (define path
        (cons part (page-path p)))
      (page 
        path
        (page-content p)))))


