#lang racket


(require scribble/html/xml
         "./attr.rkt" 
         "./style.rkt")

(define/provide-elements/not-empty rect)
(define/provide-elements/not-empty text)

(define/provide-style fill:)



(define/provide-attr x:)
(define/provide-attr y:)
