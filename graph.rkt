#lang racket

(provide site->page-graph
         page-graph->link-graph)

(require "./page.rkt"
         "./path-prefix.rkt" 
         "./attr.rkt" 
         "./util.rkt" 
         scribble/html/html
         graph)

(define (page-graph->link-graph pg)
  (define g (unweighted-graph/directed '()))

  (for ([p (get-vertices pg)])
    (add-vertex! g (pathify (page-path p))))

  (for ([p (get-vertices pg)])
    (for ([l (get-neighbors pg p)])
      (add-directed-edge! g
                          (pathify (page-path p))
                          (pathify (page-path l))) ))
  
  g)

(define (site->page-graph pages)
  (set! pages (flatten pages)) 

  (define g (unweighted-graph/directed '()))

  (for ([p pages])
    (add-vertex! g p))
  
  (for ([p pages])
    (for ([l (links-from p pages)])
      (add-directed-edge! g p l)))

  g)

(define (links-from p pages)
  (define hrefs
    (map (curry get-attribute href:)
         (filter-element #:force? #t
           (query a)
           (page-content p)))) 



  (define linked-pages 
    (filter 
      (lambda (other-p)
        (define my-path (pathify (page-path other-p))) 

        (member my-path hrefs string=?))
      pages))

  linked-pages)


(module+ test
  (require "./link-to.rkt")

  (define index
    (page index.html
          (div
            (a href: "/index.html")
            (a href: "/about.html")
            (a href: "/cities.html"))))

  (define cities
    (page cities.html
            (div
              (link-to index "Home")
              (a href: "/cities/washington.html"))))

  (define washington
    (page cities/washington.html
          (div
            (link-to index "Home"))))

  (define about
    (page about.html
          (div
            (link-to index "Home")
            (a href: "/cities.html"))) )

  (define ps 
    (list
      index
      about
      cities 
      washington))

  (define g (site->page-graph ps))

  (require rackunit)
  (check-true
    (has-vertex? g washington)
    "Graph should have washington page.")

  (check-true
    (has-edge? g cities washington)
    "Graph should link cities to washington")  

  (check-true
    (has-edge? g washington index)
    "Graph should link washington back home")
  
  )




