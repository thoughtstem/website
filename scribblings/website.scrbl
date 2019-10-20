#lang scribble/manual
@require[@for-label[website]]

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



@defstruct[page ([name (list-of string?)] [content element?])]{
  A page is an abstraction of a file in a project.  For example:

  @verbatim{
    project:
      index.html
      css/
        custom.css
      js/
        custom.js
  }

  This project would be represented as a site with three pages:

  @codeblock{
          (define project
           (list
            (page index.html ...)
            (page css/custom.css ...)
            (page js/custom.js ...)))
  }
 
  The actual files would be created later, with a call to @racket[render]. 
  
}


