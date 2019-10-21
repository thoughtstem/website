#lang racket

(provide properties)

(define (properties . ps)
  (if (empty? ps)
    ""
    (~a 
       (~a (first ps)
           (style-render (second ps)))
       ";"
       (apply properties (drop ps 2)))))

(define (style-render x)
  (cond
    [(number? x) (~a x "px")] ;Assume numbers mean pixels.
    [else x]))


(define-syntax-rule
  (define/provide-style type)
  (begin
    (provide type)
    (define type 'type)))

(define/provide-style background-color:)

(define/provide-style background-image:)

(define/provide-style color:)


(define/provide-style margin:)
(define/provide-style margin-top:)
(define/provide-style margin-left:)
(define/provide-style margin-right:)
(define/provide-style margin-bottom:)


(define/provide-style padding:)
(define/provide-style padding-top:)
(define/provide-style padding-left:)
(define/provide-style padding-right:)
(define/provide-style padding-bottom:)


(define/provide-style border:)
(define/provide-style border-top:)
(define/provide-style border-left:)
(define/provide-style border-right:)
(define/provide-style border-bottom:)
(define/provide-style border-radius:)



(define/provide-style cursor:)

(define/provide-style display:)

(define/provide-style filter:)
(define/provide-style float:)

(define/provide-style font:)
(define/provide-style font-family:)
(define/provide-style font-size:)


(define/provide-style overflow:)

(define/provide-style width:)
(define/provide-style height:)

(define/provide-style vertical-align:)
(define/provide-style text-align:)


(define/provide-style top:)
(define/provide-style left:)
(define/provide-style right:)
(define/provide-style bottom:)


(define/provide-style visibility:)

(define/provide-style z-index:)


(define/provide-style object-fit:)

(define/provide-style clear:)

