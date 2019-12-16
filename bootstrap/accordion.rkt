#lang racket 

(require "../main.rkt")

(define/provide-extensible-element 
  accordion
  div
  (class: "accordion" class-join))

