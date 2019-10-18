#lang racket

(provide write-image)

(require
  "./render.rkt"
  scribble/html/html
  (only-in 2htdp/image
           save-svg-image)) 

(define (write-image i)
  (thunk
    (define r (random 100000))

    (save-svg-image i (build-path (site-dir) (~a r ".svg")))

    (img 'src: (~a "/" r ".svg"))))
