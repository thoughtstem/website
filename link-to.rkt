#lang racket

(provide link-to)

(require scribble/html/html
         "./page.rkt")

;A thunk so this works with the letrec implied by (site ...)
(define-syntax-rule (link-to page text)
  (thunk
    (a 'href: 
       (~a "/" (string-join (page-path page) "/"))
       text)))

