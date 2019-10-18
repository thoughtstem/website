#lang racket

(provide get-attr class-join)

(define-syntax-rule
  (define/provide-attr type)
  (begin
    (provide type)
    (define type 'type)))

(define/provide-attr class:)
(define/provide-attr href:)
(define/provide-attr src:)
(define/provide-attr style:)

(define (get-attr attr content)
  (define i (index-of content attr)) 

  (and i (list-ref content (add1 i))))

(define (class-join val other-value)
  (if (and val other-value)
      (string-append val " " other-value)
      (or val other-value)))



