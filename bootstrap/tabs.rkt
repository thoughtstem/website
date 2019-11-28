#lang racket

(provide tabify)

(require "../main.rkt"
         website/bootstrap/nav
         website/util)

(define/provide-extensible-element 
  nav-tabs 
  ul
  (class: "nav nav-tabs" class-join)
  ('role: "tablist" class-join))


(define/provide-extensible-element 
  active-nav-item
  li
  (class: "nav-item active" class-join))

(define/provide-extensible-element 
  tab-content
  div
  (class: "tab-content" class-join))
  
(define/provide-extensible-element 
  tab-pane
  div
  (class: "tab-pane fade" class-join))

(define/provide-extensible-element 
  active-tab-pane
  div
  (class: "tab-pane fade in active" class-join))

(define/provide-extensible-element 
  tab-nav-link 
  a
  (class: "nav-link" class-join)
  ('data-toggle: "tab" class-join))

(define/provide-extensible-element 
  active-tab-nav-link
  a
  (class: "nav-link active" class-join)
  ('data-toggle: "tab" class-join))

(define (tabify . stuff)
  (define links (filter (curry has-class? "nav-link") (flatten stuff)))
  (define panes (filter (curry has-class? "tab-pane") (flatten stuff)))

  (list
    (nav-tabs (map nav-item links))
    (tab-content panes)))
