#lang racket

(require (except-in "../main.rkt" col))

(define-syntax-rule (define/provide-col type)
  (begin
    (provide type)
    (define-extensible-element
      type
      div
      (class: (~a 'type) class-join))))



(define/provide-col col)

(define/provide-col col-1)
(define/provide-col col-2)
(define/provide-col col-3)
(define/provide-col col-4)
(define/provide-col col-5)
(define/provide-col col-6)
(define/provide-col col-7)
(define/provide-col col-8)
(define/provide-col col-9)
(define/provide-col col-10)
(define/provide-col col-11)
(define/provide-col col-12)


(define/provide-col col-sm-auto)
(define/provide-col col-sm-1)
(define/provide-col col-sm-2)
(define/provide-col col-sm-3)
(define/provide-col col-sm-4)
(define/provide-col col-sm-5)
(define/provide-col col-sm-6)
(define/provide-col col-sm-7)
(define/provide-col col-sm-8)
(define/provide-col col-sm-9)
(define/provide-col col-sm-10)
(define/provide-col col-sm-11)
(define/provide-col col-sm-12)


(define/provide-col col-md-auto)
(define/provide-col col-md-1)
(define/provide-col col-md-2)
(define/provide-col col-md-3)
(define/provide-col col-md-4)
(define/provide-col col-md-5)
(define/provide-col col-md-6)
(define/provide-col col-md-7)
(define/provide-col col-md-8)
(define/provide-col col-md-9)
(define/provide-col col-md-10)
(define/provide-col col-md-11)
(define/provide-col col-md-12)



(define/provide-col col-lg-auto)
(define/provide-col col-lg-1)
(define/provide-col col-lg-2)
(define/provide-col col-lg-3)
(define/provide-col col-lg-4)
(define/provide-col col-lg-5)
(define/provide-col col-lg-6)
(define/provide-col col-lg-7)
(define/provide-col col-lg-8)
(define/provide-col col-lg-9)
(define/provide-col col-lg-10)
(define/provide-col col-lg-11)
(define/provide-col col-lg-12)


(define/provide-col col-xl-auto)
(define/provide-col col-xl-1)
(define/provide-col col-xl-2)
(define/provide-col col-xl-3)
(define/provide-col col-xl-4)
(define/provide-col col-xl-5)
(define/provide-col col-xl-6)
(define/provide-col col-xl-7)
(define/provide-col col-xl-8)
(define/provide-col col-xl-9)
(define/provide-col col-xl-10)
(define/provide-col col-xl-11)
(define/provide-col col-xl-12)



