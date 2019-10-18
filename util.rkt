#lang racket 

(provide include-js 
         include-css
         findf-element
         element?
         element->contents
         query)

(require scribble/html/html
         (only-in scribble/html/xml
                  attribute?)
         "./page.rkt")

(define (get-path p)
  (if (page? p)
    (page->path p)
    p))

(define (include-js src)
  (script 'src: 
          (get-path src)))

(define (include-css href)
   (link 'rel: "stylesheet" 'type: "text/css" 'href: 
         (get-path href)))

(define (element? x)
  (and (struct? x)
       (eq? 
         'struct:element
         (vector-ref (struct->vector x) 0) )))

(module+ test
  (require rackunit)
  (check-true
    (element? (h1 "HI"))))

(define/contract (element->contents e)
  (-> (or/c string? element?)
      (or/c #f any/c))
  (if (string? e)
    #f
    (filter identity
            (flatten
              (vector-ref (struct->vector e) 3)))))


(define (element->attributes e)
  (flatten
    (vector-ref (struct->vector e) 2)))

(module+ test
  (check-equal?
    (element->attributes (h1 'class: "c"
                             'id: "i"
                             "HI"))
    '(class "c" id "i")))

(define (element->kind e)
  (vector-ref (struct->vector e) 1))

(module+ test
  (check-equal?
    (element->contents
      (h1 "HI"))
    '("HI"))
  
  (check-equal?
    (element->contents
      (h1 "HI" "There"))
    '("HI" "There"))
  
  (check-equal?
    (element->kind
      (div (h1 "HI")))
    'div) )

(define (elem-proc->symbol p)
  (if (symbol? p)
    p
    (string->symbol
      (string-replace
        (second (string-split (~a p) ":"))
        ">" ""))))

(define/contract (findf-element q? elem)
  (-> procedure? element? (or/c #f element?))

  (if (q? elem)
    elem
    (let ([contents (element->contents elem)])
      (if (not contents)
        #f 
        (findf 
          identity
          (map (curry findf-element q?) 
               (element->contents elem)))))))


(module+ test
  (check-equal?
    (findf-element
      (query h1)
      (div (h1 "HI")))
    (h1 "HI"))

  
  
  (check-equal?
    (element->contents
      (findf-element
        (query h1)
        (div
          'class: "c1"
          (list
            (p "dummy")
            (div 
              'class: "c2"
              (p "dummy")
              (h1 
                'class: "c3"
                "HI"))))))
   '("HI"))
  )


(define (has-attribute? e attr)
  (member 
    (attribute? attr)
    (element->attributes e)))

(define (get-attribute e attr)
  (define attrs 
    (element->attributes e))

  (define i
    (index-where 
      attrs
      (curry eq? (attribute? attr))))
  
  (list-ref attrs (add1 i))
  )

(define (list->pairs l)
  (if (empty? l)
    '()
    (cons (take l 2) 
          (list->pairs (drop l 2)))))

(define (query kind . attr-qs)
  (define (kind-matches k e)
    (or (eq? 
          (elem-proc->symbol k) 
          (element->kind e))))

  (define (attr-matches attr-q e)
    (define attr (first attr-q))      
    (define val  (second attr-q))      

    (and (has-attribute? e attr)
         (equal?
           (get-attribute e attr)
           val)))

  (define (attrs-match attr-qs e)
    (andmap 
      (curryr attr-matches e)  
      (list->pairs attr-qs)))

  (lambda (e)
    (and
      (kind-matches kind e) 
      (attrs-match attr-qs e)) ))


(module+ test
  (check-equal? 
    (findf-element
      (query div 'id: "yesme" 'class: "classy")
      (div
        (p "not me")
        (div 'id: "notme"
          (list
            (p "not me") 
            (div 'id: "yesme") ;not really
            (div 'id: "notme" 'class: "wrongclass")
            (div 'id: "yesme" 'class: "classy") ;this one!
            (p "not me")))))
    (div 'id: "yesme" 'class: "classy")))

