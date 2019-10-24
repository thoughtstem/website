#lang info
(define collection "website")
(define deps '("base" "graph"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/website.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(thoughtstem))
(define raco-commands
  '(("website-preview" website/raco-tools/preview "runs a server in your current directory and serves everything" 100)
    ))
