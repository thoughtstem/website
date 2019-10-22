#lang racket

(provide write-image
         (rename-out [write-image write-img]))

(require
  "./render.rkt"
  "./path-prefix.rkt"
  scribble/html/html
  (only-in pict
           pict->bitmap)
  (only-in 2htdp/image
           image?
           save-svg-image)) 

(define (write-image . content)
  (thunk
    (define i (last content))

    (define save-path
      (save-image i))

    (apply img 
           'src: (add-path-prefix (~a "/" save-path))
           (drop-right content 1))))

(define next-i 0)
(define (save-image i)
  (set! next-i (add1 next-i))

  (define r next-i)

  (if (image? i)
    (let* ([f (~a r ".svg")] 
           [ path 
             (build-path (site-dir) 
                         f)])

      (save-svg-image i path)
      f)

    (let* ([f (~a r ".png")]
           [path 
             (build-path (site-dir) 
                         f)])
      (save-pict i path 'png)
      f)))

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))

