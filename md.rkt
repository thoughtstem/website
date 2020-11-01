#lang at-exp racket

(provide md)

(require markdown
         website/util)

(define (parse-md x)
  (define p
    (parse-markdown x))

  (html/inline
    (string-join (map xexpr->string p))))

(define (md . ss)

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
      ss))

  (if (string? (last ret)) ;The fold might not parse the last run of strings
      (list-set
	ret
	(sub1 (length ret))
	(parse-md (last ret)))
      ret))
