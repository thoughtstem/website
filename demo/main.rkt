#lang racket

(require website/render)
(require "./my-site.rkt")

(render my-site #:to "demo-output")
