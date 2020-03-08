#lang racket

(require "../main.rkt")

(define/provide-extensible-element 
  alert-primary
  div
  (class: "alert alert-primary" class-join))

(define/provide-extensible-element 
  alert-secondary
  div
  (class: "alert alert-secondary" class-join))

(define/provide-extensible-element 
  alert-success
  div
  (class: "alert alert-success" class-join))

(define/provide-extensible-element 
  alert-danger
  div
  (class: "alert alert-danger" class-join))

(define/provide-extensible-element 
  alert-warning
  div
  (class: "alert alert-warning" class-join))

(define/provide-extensible-element 
  alert-light
  div
  (class: "alert alert-light" class-join))

(define/provide-extensible-element 
  alert-dark
  div
  (class: "alert alert-dark" class-join))


