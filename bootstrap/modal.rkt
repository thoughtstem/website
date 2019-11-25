#lang racket

(require "../main.rkt")

;; default modal has fade transition
(define/provide-extensible-element 
  modal 
  div
  (class: "modal fade" class-join))

(define/provide-extensible-element 
  modal-dialog
  div
  (class: "modal-dialog" class-join))

(define/provide-extensible-element 
  modal-content
  div
  (class: "modal-content" class-join))

(define/provide-extensible-element 
  modal-header
  div
  (class: "modal-header" class-join))

(define/provide-extensible-element 
  modal-title
  h5
  (class: "modal-title" class-join))

(define/provide-extensible-element 
  modal-body
  div
  (class: "modal-body" class-join))

(define/provide-extensible-element 
  modal-footer
  div
  (class: "modal-footer" class-join))
