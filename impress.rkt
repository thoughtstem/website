#lang racket

(provide (all-from-out website)
         include-impress-js
         impress-init
         impress
         impress-site
         step
         )

(require website
         racket/runtime-path)

(define-runtime-path js "impress/js")
(define-runtime-path css "impress/css")

(define (guard this that)
  (if this that #f))

(define (step #:x (x #f) 
              #:y (y #f) 
              #:z (z #f) 
              #:rel-x (rel-x #f) 
              #:rel-y (rel-y #f) 
              #:rel-z (rel-z #f) 
              #:rotate-x (rotate-x #f) 
              #:rotate-y (rotate-y #f) 
              #:rotate-z (rotate-z #f) 
              #:scale (scale #f) 
              #:rotate (rotate #f) 
              #:autoplay (autoplay #f) 
              #:goto (goto #f) 
              #:key-list (key-list #f) 
              #:next-list (next-list #f) 
              . contents)
  (apply div
         (filter identity
                 (flatten
                   (list
                     class: "step"
                     (guard x (list 'data-x: (~a x)))
                     (guard y (list 'data-y: (~a y)))
                     (guard z (list 'data-z: (~a z)))
                     (guard rel-x (list 'data-rel-x: (~a rel-x)))
                     (guard rel-y (list 'data-rel-y: (~a rel-y)))
                     (guard rel-z (list 'data-rel-z: (~a rel-z)))
                     (guard rotate-x (list 'data-rotate-x: (~a rotate-x)))
                     (guard rotate-y (list 'data-rotate-y: (~a rotate-y)))
                     (guard rotate-z (list 'data-rotate-z: (~a rotate-z)))
                     (guard scale (list 'data-scale: (~a scale)))
                     (guard rotate (list 'data-rotate: (~a rotate)))
                     (guard autoplay (list 'data-autoplay: (~a autoplay)))

                     (guard goto (list 'data-goto: (~a goto)))
                     (guard key-list (list 'data-goto-key-list: (~a key-list)))
                     (guard next-list (list 'data-goto-next-list: (~a next-list)))
                     contents)))))

(define (impress-files)
  (page js/impress.js
        (file->string (build-path js "impress.js"))))

(define (include-impress-js)
  (list 
    (include-js "/js/impress.js")))

(define (impress-init)
  (script "impress().init()"))

(define (impress-site #:head (head-content #f) . contents)
  (list
    (impress-files)
    (page index.html
      (html
        (head
          head-content)
        (body class: "impress-not-supported"
          (impress contents)
          (include-impress-js)
          (impress-init))))))

(define (impress . contents)
  (div id: "impress" 
       'data-transition-duration: "1000" 
       contents))
