#lang info
(define collection "website")
(define deps '("base" "graph" "markdown"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/website.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(thoughtstem))
(define raco-commands
  '(("website-preview" (submod website/raco-tools/preview main) "runs a server in your current directory and serves everything" 100)
    ("website-s3-deploy" (submod website/raco-tools/s3-deploy main) "Deploys to S3" 100)
    ))
