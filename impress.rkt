#lang at-exp racket

(provide (except-out (all-from-out website) col)
         include-impress-js
         impress-init
         impress
         impress-files
         impress-site
         step
         impress-debug
         debug-impress)

(require website
         racket/runtime-path)

(define-runtime-path js "impress/js")
(define-runtime-path css "impress/css")

(define impress-debug (make-parameter #f))

(define-syntax-rule (debug-impress stuff ...)
  (parameterize ([impress-debug #t])
    stuff ...))


(define (fix-format that)
  (define v (second that))
  (list-set
    that
    1
    (cond 
      [(number? v)
       (~r (exact->inexact v)
           #:precision 3)]
      [else v]))) 

(define (guard this that)
  (if this 
    (fix-format that)
    #f))

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
                     (guard x (list 'data-x: x ))
                     (guard y (list 'data-y: y ))
                     (guard z (list 'data-z: z ))
                     (guard rel-x (list 'data-rel-x: rel-x ))
                     (guard rel-y (list 'data-rel-y: rel-y ))
                     (guard rel-z (list 'data-rel-z: rel-z ))
                     (guard rotate-x (list 'data-rotate-x: rotate-x ))
                     (guard rotate-y (list 'data-rotate-y: rotate-y ))
                     (guard rotate-z (list 'data-rotate-z: rotate-z ))
                     (guard scale (list 'data-scale: scale ))
                     (guard rotate (list 'data-rotate: rotate ))
                     (guard autoplay (list 'data-autoplay: autoplay ))

                     (guard goto (list 'data-goto: goto ))
                     (guard key-list (list 'data-goto-key-list: key-list ))
                     (guard next-list (list 'data-goto-next-list: next-list ))
                     contents
                     (when (impress-debug)
                       @div{x:@x, y:@y, scale:@scale})
                     )))))

(define (impress-files)
  (page js/impress.js
        (file->string (build-path js "impress.js"))))

(define (include-impress-js)
  (list 
    (include-js "/js/impress.js")))

(define (impress-init)
  (script "impress().init()"))

(define (impress-site 
          #:transition-duration (td 1000)
          #:head (head-content #f) . contents)
  (list
    (impress-files)
    (page index.html
      (html
        (head
          head-content)
        (body class: "impress-not-supported"
          (impress #:transition-duration td
                   contents))))))

(define (impress 
          #:transition-duration (td 1000)
          #:body-scrollbar? (body-scrollbar? #f)
          . contents)
  (list
    (div id: "impress" 
         'data-transition-duration: (~a td)
         contents)
    (include-impress-js)
    (impress-init)
    ;Impress hides the body scrollbar by default,
    ;  This js line below reverses that.
    (when body-scrollbar?
      @script/inline{document.querySelector("body").style.overflow = "scroll"})))


