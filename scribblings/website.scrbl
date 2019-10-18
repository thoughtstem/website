#lang scribble/manual
@require[@for-label[website
                    racket/base]]

@title{website}
@author{thoughtstem}

@defmodule[website]

@defproc[(render [site (listof page?)] [#:to to path-string?]) void?]{

  @codeblock{
    #lang racket
    
    (require website)

    (define my-site
      (site
         (index
           (page index.html
             (html (body (h1 "Hello World")))))))

    (render my-site #:to "output")
  }

  Rendering @racket[my-site] out to the output directory.
}






