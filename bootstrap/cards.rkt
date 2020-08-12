#lang racket

(require "../main.rkt")

(define/provide-extensible-element 
  card-deck
  div
  (class: "card-deck" class-join))

(define/provide-extensible-element 
  card-columns
  div
  (class: "card-columns" class-join))

(define/provide-extensible-element 
  card
  div
  (class: "card" class-join))


(define/provide-extensible-element 
  card-img-top
  img
  (class: "card-img-top" class-join)
  (src: (picsum 300 300) replace))

(define/provide-extensible-element 
  card-img
  img
  (class: "card-img" class-join)
  (src: (picsum 300 300) replace))

(define/provide-extensible-element 
  card-img-overlay
  div
  (class: "card-img-overlay" class-join))

(define/provide-extensible-element 
  card-body
  div
  (class: "card-body" class-join))

(define/provide-extensible-element 
  card-title
  h5
  (class: "card-title" class-join))

(define/provide-extensible-element 
  card-subtitle
  h6
  (class: "card-subtitle mb-2 text-muted" class-join))

(define/provide-extensible-element 
  card-text
  p
  (class: "card-text" class-join))


(define/provide-extensible-element 
  card-link
  a
  (class: "card-link" class-join))

(define/provide-extensible-element 
  card-group
  div
  (class: "card-group" class-join))


(define/provide-extensible-element 
  card-header
  div
  (class: "card-header" class-join))

(define/provide-extensible-element 
  card-footer
  div
  (class: "card-footer" class-join))





