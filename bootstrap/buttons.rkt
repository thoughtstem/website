#lang racket

(require "../main.rkt")

(define/provide-extensible-element 
  button-primary
  button
  (type: "button" replace)
  (class: "btn btn-primary" class-join))

(define/provide-extensible-element 
  button-secondary
  button
  (type: "button" replace)
  (class: "btn btn-secondary" class-join))

(define/provide-extensible-element 
  button-success
  button
  (type: "button" replace)
  (class: "btn btn-success" class-join))

(define/provide-extensible-element 
  button-danger
  button
  (type: "button" replace)
  (class: "btn btn-danger" class-join))

(define/provide-extensible-element 
  button-warning
  button
  (type: "button" replace)
  (class: "btn btn-warning" class-join))


(define/provide-extensible-element 
  button-info
  button
  (type: "button" replace)
  (class: "btn btn-info" class-join))

(define/provide-extensible-element 
  button-light
  button
  (type: "button" replace)
  (class: "btn btn-light" class-join))


(define/provide-extensible-element 
  button-dark
  button
  (type: "button" replace)
  (class: "btn btn-dark" class-join))


(define/provide-extensible-element 
  button-link
  button
  (type: "button" replace)
  (class: "btn btn-link" class-join))



