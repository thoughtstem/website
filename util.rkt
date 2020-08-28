#lang at-exp racket 

(provide include-js 
         include-css
         include-deferred-css
         
         findf-element
         filter-element
         element?
         element->contents
         element->string
         query
         paras
         
         get-attribute
         set-attribute
         has-attribute?
         has-class?

         html/inline
         
         map-element
         collect-all
         scrape-out
         check-elements-equal?

         inline-pre)

;XML stuff
(provide html->element
	 html->list)

(require scribble/html/html
         (only-in scribble/html/xml
                  attribute?)
         "./page.rkt"
         "./style.rkt"
         "./path-prefix.rkt"
         (only-in rackunit check-equal?))

(define html-inline-id 0)

(define (next-html-inline-id)
  (set! html-inline-id (add1 html-inline-id))
  html-inline-id)

(define (html/inline str)
  (define id (next-html-inline-id))
  (define fixed-str (string-replace str "'" "\""))
  (list (span 'id: (~a "html-inline-" id))
        @script/inline{
(function(){
  var element = document.getElementById('html-inline-@id');
  element.innerHTML = '@(string-replace fixed-str "\n" "")';
})();}))

(define (get-path p)
  (if (page? p)
    (page->path p)
    p))

(define (include-js src #:tag [tag 'normal])
  (cond
    [(eq? tag 'async) (script 'async: ""
                              'src: 
                              (pathify (add-path-prefix (get-path src))))]
    [(eq? tag 'defer) (script 'defer: ""
                              'src: 
                              (pathify (add-path-prefix (get-path src))))]
    [else             (script 'src: 
                              (pathify (add-path-prefix (get-path src))))]

    )

  )

;<link rel="preload" href="styles.css" as="style" onload="this.onload=null;this.rel='stylesheet'">
;<noscript><link rel="stylesheet" href="styles.css"></noscript>

(define (include-deferred-css href)
  (list #;(link 'rel: "preload"
              'href: (pathify (add-path-prefix (get-path href)))
              'as: "style"
              'onload: "this.onload=null;this.rel='stylesheet'")
        @script/inline{
 if (preloadSupported()){
  var cssLink = document.createElement('link');
  cssLink.rel = 'preload';
  cssLink.as = 'style';
  cssLink.onload = function(){this.onload=null;this.rel="stylesheet"};
  cssLink.href = '@(pathify (add-path-prefix (get-path href)))';
  document.head.appendChild(cssLink);
 }
 else{
  var cssLink = document.createElement('link');
  cssLink.rel = 'stylesheet';
  cssLink.href = '@(pathify (add-path-prefix (get-path href)))';
  document.head.appendChild(cssLink);
 }
}
        (noscript (link 'rel: "stylesheet"
                        'href: (pathify (add-path-prefix (get-path href)))))))

(define (include-css href)
   (link 'rel: "stylesheet" 'type: "text/css" 'href: 
         (pathify (add-path-prefix (get-path href)))))

(define (element? x)
  (and (struct? x)
       (eq? 
         'struct:element
         (vector-ref (struct->vector x) 0) )))

(module+ test
  (require rackunit)
  (check-true
    (element? (h1 "HI"))))

(define (element->string e)
  (with-output-to-string
    (thunk (output-xml e))))

(module+ test
  (check-equal? 
    (element->string (p "HI"))
    "<p>HI</p>"))

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
    '(class "c" id "i"))
  
  (check-equal?
    (element->attributes 
      (set-attribute
	(h1 'class: "c"
	    'id: "i"
	    "HI")
	'data-x: "this is data"))
    '(class "c" id "i" data-x "this is data"))
  )

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
  (-> procedure? any/c
      (or/c #f element? list?))

  (if (q? elem)
    elem
    (let ([contents 
            (and 
              (element? elem)
              (element->contents elem))])
      (if (not contents)
        #f 
        (findf 
          identity
          (map (curry findf-element q?) 
               contents))))))



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


(define (has-class? class e )
  (and (has-attribute? 'class: e)
       (member class 
         (string-split (get-attribute 'class: e) " "))))

(define (has-attribute? attr e )
  (member 
    (attribute? attr)
    (element->attributes e)))

(define (get-attribute attr e )
  (define attrs 
    (element->attributes e))

  (define i
    (index-where 
      attrs
      (curry eq? (attribute? attr))))
  
  (list-ref attrs (add1 i)))

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

    (and (has-attribute? attr e )
         (equal?
           (get-attribute attr e )
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
        (thunk (p "NOT ME"))
        (div 'id: "yesme" 'class: "classy")))
    (div 'id: "yesme" 'class: "classy"))

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

(define (paras . ss)
  (map maybe-p-ify ss))

(define (lone-newline s)
  (and (string? s)
       (string=? "\n" s)))

(define (maybe-p-ify s)
  (if (lone-newline s)
    (br)
    s))


(define/contract (filter-element #:force? (force? #f) q? elem-or-proc)
  (->* (procedure? any/c)
       (#:force? boolean?)
       (listof element?))

  (define elem
    (if (and (procedure? elem-or-proc)
             (not (element? elem-or-proc)) ;Elements are apparently procs too, so we need to further distinguish?
             force?)
      (begin
        (elem-or-proc))  
      elem-or-proc))

  (define has-contents? (and (element? elem)
                             (element->contents elem)))

  (define (recurse)
    (apply append 
           (map (curry filter-element #:force? force? q?) 
                (element->contents elem))))

  (if (q? elem)
    (if (not has-contents?)
      (list elem)  
      (cons elem
            (recurse) 
            ))
    (let ()
      (if (not has-contents?)
        '()
        (recurse)))))

(module+ test
  
  (check-equal?
    (length
      (filter-element (query h1)
                      (div
                        (h1 "1") 
                        (h1 "2")))) 

    2)

 (check-equal? 
   (length
     (filter-element (query h1)
                     (div
                       (h1 "1") 
                       (h1 "2"
                           (h1 "3")))))
   3)
 
  (check-equal?
    (length
      (filter-element #:force? #t
                      (query h1)
                      (div
                        (thunk (h1 "1")) 
                        (h1 "2")))) 

    2))


(require scribble/html/xml)


(define (add-back-colons l)
  (for/list ([thing l]
             [even? (map even? (range (length l)))])

    (define ret
      (if even?
        (string->symbol (~a thing ":"))
        thing))

    ret))


(define (map-element f elem)
  (define (recurse)
    (define kind (element->kind elem))

    (f
      (apply (curry element/not-empty kind)
             (flatten
               (append
                 (add-back-colons (element->attributes elem))
                 (map (curry map-element f)
                      (element->contents elem)))))))

  (define has-contents? 
    (and (element? elem)
         (element->contents elem)))

  (cond 
    [has-contents? (recurse)]
    [(list? elem) (map (curry map-element f) elem)]
    [else elem]))


(define (set-attribute elem attr-name attr-value)
  (apply 
    element/not-empty 
    (flatten
      (list 
	(element->kind elem)
	(add-back-colons (element->attributes elem))
	attr-name 
	attr-value
	(element->contents elem)))))



(module+ test
  (check-false
    (findf-element (query script)
      (map-element  (lambda (e) 
                      (if ((query script) e)
                        (void)
                        e))
                    (div
                      (div "HI") 
                      (script/inline
                        @~a{alert("HI")})))) ))

(define (scrape-out kind element)
  (map-element  (lambda (e) 
                  (if ((query kind) e)
                    (void)
                    e))
                element))

(define (collect-all kind element)
  (define ret '())
  (map-element  (lambda (e) 
                  (when ((query kind) e)
                    (set! ret (cons e ret)))
                  e)
                element)
  
  ret)

(define (check-elements-equal? x y)
  (check-equal?
   (element->string x)
   (element->string y)))


(define (inline-pre #:light-text? [light-text? #f] . content)
  (define (text-color)
    (if light-text?
        "text-white"
        ""))
  (pre 'class: (text-color)
       'style: (properties display: "inline")
    content))





(require (only-in xml 
		  xml->xexpr
		  eliminate-whitespace
		  document-element
		  read-xml))

;TODO: Move this to website when it works

(define (convert-attr a)
  (define k (first a))
  (define v (second a))

  (define k: (string->symbol
	       (~a 
		 (symbol->string k)
		 ":")))

  (list k: v))

(define (convert-to-website-syntax l)

  (if (list? l)
      (let ()
	(define tag (first l))
	(define attrs (second l))
	(define content (drop l 2))
	(append (list tag)
		(flatten (map convert-attr attrs))
		(map convert-to-website-syntax content)))
      l))


(define (html->element html-string 
		       [ns 
			 (module->namespace 'website)])
  (eval
    (html->list html-string)
    ns))

(define (html->list html-string)
  (convert-to-website-syntax
    (xml->xexpr
      ((eliminate-whitespace '(span nav html body head header footer))
       (document-element
	 (read-xml (open-input-string html-string)))))))


(module+ 
  test
  (dynamic-require 'website #f)
  (html->element
    @~a{
    <div class="container cf">
    <img src="wp-content/uploads/2016/03/WebBanner-ClearBackground_ParentVertical-e1458841641347.png" alt=""> </img>
    <h1>
    LearnToMod is Free for Educators!		</h1>
    <p>By partnering with the National Science Foundation, we're able to bring LearnToMod to all teachers at 0 cost.</p>
    <p><a href="http://www.learntomod.com/teacher-sign-up/" class="button">Start Your Minecraft Education</a></p>
    </div>
    }))

