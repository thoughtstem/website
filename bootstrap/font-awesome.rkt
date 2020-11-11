#lang racket

(require "../main.rkt")

(define-syntax-rule 
  (define/provide-fa id)
  (define/provide-extensible-element 
    id
    i
    (class: (~a "fas " (symbol->string 'id)) class-join)))

(define/provide-fa fa-gem)
(define/provide-fa fa-clock)
(define/provide-fa fa-user-graduate)
(define/provide-fa fa-shield-alt)
(define/provide-fa fa-gamepad)
(define/provide-fa fa-laptop-code)
(define/provide-fa fa-code-branch)
(define/provide-fa fa-calendar-day)
(define/provide-fa fa-calendar-week)
(define/provide-fa fa-calendar-alt)

(define/provide-fa fa-pizza-slice)
(define/provide-fa fa-cookie)
(define/provide-fa fa-cookie-bite)

(define/provide-fa fa-angle-double-right)
(define/provide-fa fa-angle-double-left)
(define/provide-fa fa-angle-double-up)
(define/provide-fa fa-angle-double-down)

(define/provide-fa fa-angle-right)
(define/provide-fa fa-angle-left)
(define/provide-fa fa-angle-up)
(define/provide-fa fa-angle-down)

(define/provide-fa fa-book)
(define/provide-fa fa-envelope)

(define/provide-fa fa-windows)
