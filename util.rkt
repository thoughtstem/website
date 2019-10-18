#lang racket 

(provide include-js 
         include-css)

(require scribble/html/html
         "./page.rkt")

(define (get-path p)
  (if (page? p)
    (page->path p)
    p))

(define (include-js src)
  (script 'src: 
          (get-path src)))

(define (include-css href)
   (link 'rel: "stylesheet" 'type: "text/css" 'href: 
         (get-path href)))

