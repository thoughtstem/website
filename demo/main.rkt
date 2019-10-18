#lang racket

(require website/render)
(require 

  "./bootstrap.rkt"
;  "./my-site.rkt"
  
  )

(render my-site #:to "demo-output")
