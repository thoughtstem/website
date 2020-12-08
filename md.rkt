#lang at-exp racket

(provide md)

(require markdown
         website/util)

(define (parse-md x)
  (define p
    (parse-markdown x))

  (define s
    (string-join (map xexpr->string p)))

  ;(html->element (string-append "<div>" s "</div>"))

  s
  )

(define (md . ss)
  ;Need to change this.
  ;  Recursively take up to double+ newlines,
  ;  map across that list an if-string->parse-md (returning element),
  ;  then merge the list, slerping all contents into the first element

  ;OOOR:
  ;Parse everything as if it were markdown, leaving sigils in the text 
  ;  in place of any non string elements.  Then, in the resulting html
  ;  string, replace the sigils with element->string for each corresponding 
  ;  element.  Then do html->element on the whole thing, converting everyting
  ;  back to an element again...


  (define sigils (hash))

  (define i 0)
  (define (create-sigil-for-non-string s)
    (if (not (string? s))
        (let ()
	  
	  (define sigil (~a "$$$$SIGIL_" i "$$$$"))
	  (set! sigils 
	    (hash-set sigils sigil s))

	  (set! i (add1 i))
	  sigil)	
	s))

  (define ss-with-sigils (map create-sigil-for-non-string ss))

  (define ret 
    (html->element (~a "<div>" 
		       (parse-md (string-join ss-with-sigils ""))
		       "</div>")))




  ;Takes string, returns list
  (define (replace-sigils-in-string c)
    (define ret (string-split c "$$$$"))


    (map
      (lambda (s)
	(if (not (string-contains? s "SIGIL_"))
	    s
	    (hash-ref sigils
		      (~a "$$$$" s "$$$$")))) 
      ret))

  (define (replace-sigil e)
    (define c (element->contents e))
    (set-contents e
		  (cond 
		    [(list? c) 
		     (map (lambda (s)
			    (if (string? s)
				(replace-sigils-in-string s)
				s)) 
			  c) ]
		    [(string? c) 
		     (replace-sigils-in-string c) ]
		    [else c]))
    
    )

  (map-element replace-sigil ret)




  #|
  (define ret
    (foldl
      (lambda (current accum)
	(cond
	  [(< (length accum) 1) (list current)]
	  [(and
	     (string? (last accum))
	     (string? current))

	   ;Two strings? We build up the string at the end, delaying the md parse
	   (append
	     (drop-right accum 1)
	     (list
	       (string-append (last accum)
			      current))) ]
	  [(and
	     (string? (last accum))
	     (not (string? current)))


	   ;Seeing an element triggers a markdown parse,
	   ;  And we simply add on the element


	   (append
	     (drop-right accum 1)
	     (list
	       (parse-md (last accum))
	       current)) ]

	  [(and
	     (not (string? (last accum)))
	     (string? current))

	   (append accum
		   (list current)) ]

	  [(and
	     (not (string? (last accum)))
	     (not (string? current)))

	   (append accum
		   (list current)) ]

	  ))
      '()
      ss
      ))

  (if (string? (last ret)) ;The fold might not parse the last run of strings
      (list-set
	ret
	(sub1 (length ret))
	(parse-md (last ret)))
      ret)
  |#
  )
