#lang racket

(provide write-image
         (rename-out [write-image write-img]))

(require
  "./render.rkt"
  scribble/html/html
  (only-in 2htdp/image
           save-svg-image)) 

(define (write-image . content)
  (thunk
    (define r (random 100000))

    (save-svg-image (last content) (build-path (site-dir) (~a r ".svg")))

    (apply img 
           'src: (~a "/" r ".svg")   
           (drop-right content 1))))
