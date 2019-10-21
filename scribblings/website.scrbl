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
      (list
        (page index.html
             (html (body (h1 "Hello World"))))))

    (render my-site #:to "output")
  }

  Rendering @racket[my-site] out to the output directory.  

  A "site" is just a list of @racket[page] structures.
}


@defstruct[page ([name (list-of string?)] [content page-content?])]{
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
  
  Note, although a @racket[page] struct stores its path as a list of strings.
  You should make use of the fact that the @racket[page] cosntructor is actually a macro
  that parses the first identifier into a list of strings based on the forward
  slashes.  If you don't want this behaviour, simply pass in a list directly
  or an id that does not contain slashes or dots.  

  This example uses all three:

  @codeblock{
    (define my-css-path (list "css" "custom.css"))
    (define project
     (list
      (page (list "index.html") ...)
      (page my-css-path ...)
      (page js/custom.js ...)))
  }

  As for content, normally, you'll use the html generating functions that are reprovided by @racket[website] from @racket[scribble/html/html].

  Basically, for (almost) every html tag, there is a constructor function with that name.  And for (almost) every attribute, there is an identifier ending with a colon.  For example:

  @codeblock{
    (html
      (head
        (title "My Site"))
      (body
        (h1 "My Site")
        (img src: "path-to-some-image" alt: "alt text")
        (div style: (properties 
                      background-color: "red"
                      color: "green")
          "I am a weird div tag from the 90s")))
  } 

  Style attributes can make use of the @racket[properties] helper which gives a special syntax for constructing css strings.
}

@defproc[(link-to [page page?] [text string?]) element?]{
  This constructs an @racket[a] @racket[element?] whose @racket[href:] attribute is constructed from the page's path and whose content comes from @racket[text].

  It is convenient if you want to link from one page to another without thinking about the target page's path.  If you decide to change the path later, the links will update accordingly.

  Just use an old-fashioned @racket[a] tag, though, if you want finer-grained control or if you don't have the target @racket[page] in scope.
}


@defthing[element? (or/c procedure? outputtable/c)]{
  Should be an @racket[outputtable/c] returned by one of the html generating functions: e.g. @racket[html], @racket[head], @racket[body], @racket[p], @racket[h1], etc...

  Or it can be a procedure that would return such a thing.  This can be useful in cases like @racket[write-img], which will write out an image file at @racket[render] time, and will return an @racket[img] @racket[element?] with the appropriate @racket[src:] attribute.
}

@defthing[site? (listof page?)]{
  Yep, a site is just a list. 

  Joining two sites together is as simple as using @racket[append].
}

@defmodule[website/bootstrap]

This provides features for building websites that use bootstrap's visual vocabularly.

Here's a (simplified) example from a version of the @hyperlink["https://metacoders.org"]{metacoders.org} website (which is built on top of @racket[website/bootstrap]).

It shows how to construct an index page and three content pages, each connected by links on a bootstrap navbar. 

@codeblock{
#lang racket

(require website/bootstrap)

(define index-path       (list "index.html"))
(define city-search-path (list "city-search.html"))
(define learn-more-path  (list "learn-more.html"))
(define get-to-work-path (list "get-to-work.html"))

(define my-nav
  (navbar
    #:brand "MetaCoders"
    (nav-link learn-more-path  "Learn More")
    (nav-link city-search-path "Enroll Kids")
    (nav-link get-to-work-path "Get To Work")))

(define (normal-content . more)
  (content
    my-nav
    (container more)))

(define index
  (page index-path
        (normal-content
          (h1 "Index"))))

(define learn-more
  (page learn-more-path
        (normal-content
          (h1 "Learn More"))))

(define city-search
  (page city-search-path
        (normal-content
          (h1 "City Search"))))

(define get-to-work
  (page get-to-work-path
        (normal-content
          (h1 "Get To Work"))))

(define my-site
 (append
  (list
   index
   learn-more
   city-search
   get-to-work)
  (bootstrap-files)))

(render my-site #:to "out")
}


@defproc[(bootstrap-files) site?]{
  Appending this in your site adds in the files:

  @verbatim{
    js/
      jquery-3.2.1.slim.min.js
      bootstrap.bundle.min.js
    css/
      bootstrap.min.css
  }

  Note that it's still up to you to actually include these files in the html pages of your site.  For that, you would use @racket[include-bootstrap-js] usually at the end of the @racket[(body ...)] portion of a page, @racket[include-bootstrap-css]  usually in the @racket[(head ...)] portion of a page, or use @racket[content] to automatically do both.
}


@defproc[(content [#:head head element?] [body-content element?] ...) element?]{
  Returns a @racket[html] element with the bootstrap css and js included in the traditional @racket[head] and @racket[body] locations respectively.

  Use this as the basic building block for any bootstrap-enabled page in your bootstrap-enabled site.   (Don't forget to append the @racket[(bootstrap-files)] to your site before rendering)
}

@defproc[(navbar [#:brand brand element?] [content element?] ...) element?]{
  Returns a bootstrap navbar element.  The content is placed inside a @racket[ul] tag (and @racket[nav-link] returns a @racket[li] item).  So you add elements to a bootstrap page as follows:

 @codeblock{
   (content
     (navbar #:brand "My Site"
       (nav-link "about.html" "About")))
 } 
}

@defproc[(container [content element?] ...) element?]{
  Wraps the given content in a @racket[div] whose @racket[class:] is @racket["container"].
}







