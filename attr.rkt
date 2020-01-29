#lang racket

(provide get-attr class-join
         define/provide-attr)

(define (get-attr attr content)
  (define i (index-of content attr)) 

  (and i (list-ref content (add1 i))))

(define (class-join val other-value)
  (if (and val other-value)
      (string-append val " " other-value)
      (or val other-value)))




(define-syntax-rule
  (define/provide-attr type)
  (begin
    (provide type)
    (define type 'type)))

(define/provide-attr class:)
(define/provide-attr href:)
(define/provide-attr src:)
(define/provide-attr style:)
(define/provide-attr id:)
(define/provide-attr align:)
(define/provide-attr type:)
(define/provide-attr action:)
(define/provide-attr dir:)
(define/provide-attr name:)
(define/provide-attr rows:)
(define/provide-attr cols:)
(define/provide-attr role:)
(define/provide-attr data-ride:)
(define/provide-attr data-toggle:)
(define/provide-attr data-target:)
(define/provide-attr data-slide:)
(define/provide-attr data-slide-to:)
(define/provide-attr target:)
(define/provide-attr allow:)
(define/provide-attr frameborder:)
(define/provide-attr allowfullscreen:)
(define/provide-attr alt:)
(define/provide-attr srcset:)

