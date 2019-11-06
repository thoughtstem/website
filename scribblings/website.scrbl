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

@defthing[attribute? symbol?]{
  A symbol that ends in a colon.  When passed into a element constructor, it must be followed by a value.  This becomes the value on the rendered html element.

  @codeblock{
    (h1 id: "title" "HI")
  }  

  Becomes:

  @verbatim{
    <h1 id="title">HI</h1>
  }
}

@defthing[site? (listof page?)]{
  Yep, a site is just a list. 

  Joining two sites together is as simple as using @racket[append].
}

@defproc[(push-path [part string?]
                    [page (or/c page? (listof page?))]) page?]{
  Adds the @racket[part] onto the page's path.  If the supplied page is actually a list, @racket[push-path] maps itself across them to push them all down a (virtual) directory.  Makes it easy to include pages as a sub-site within an existing site, where the subsite is served at a fixed subpath. 
}

@defproc[(get-attribute 
            [attr: symbol?]
            [element element?]) any/c]{

  Gets the attribute from the element.

  @codeblock{
    (get-attribute id: (div id: "id"))
  }

  Gives @racket["id"].

  Fails if it doesn't exist.
}

@defproc[(has-attribute?
            [attr: symbol?]
            [element element?]) boolean?]{

  Returns true if the element has the attribute.
}

@defproc[(get-property
            [prop: symbol?]
            [style string?]) string?]{

  This returns the property within a style string:

  @codeblock{
    (get-property color:
      (get-attribute style:
        (div style: (properties color: "red"))))
  }

  Gives you @racket["red"].
}

@defproc[(de-url
            [string-with-url string?]) string?]{

  Takes strings like @racket["url(http://example.com/img.png)"] and gives you back just the url, like @racket["http://example.com/img.png"].
}

@defform[(define/provide-extensible-element name base overrides ...)]{

@codeblock{
(define/provide-extensible-element
  page-item
  li
  (class: "page-item" class-join))
}
}

@section{Bootstrap}

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

@section{Grids}

Bootstraps grid system is what allows for responsive design.  Constructors are provided for @racket[row] and columns of various sizes -- all of which correspond to Bootstrap classes in a straight-forward way.


@defproc[(row [#:element element div] [content (or/c element? attribute?)]) element?]{
  A wrapper for column elements.  A full example for context:

    @codeblock{
        #lang racket

        (require website/bootstrap)

        (define (normal-content title . stuff)
         (content
          (container
           (h1 title)
           stuff)))

        (define my-site
         (append
          (bootstrap-files)
          (list
           (page index.html
            (normal-content
             "Column Demos"

             (row
              (col style: "background-color: red"  "1") 
              (col-6 style: "background-color: green" "2") 
              (col style: "background-color: blue" "3"))

             (row
              (col style: "background-color: red"  "1") 
              (col-5 style: "background-color: green" "2") 
              (col style: "background-color: blue" "3"))


             (row
              (col-6 class: "col-md-4" style: "background-color: red"  "1") 
              (col-6 class: "col-md-4" style: "background-color: green"  "1") 
              (col-6 class: "col-md-4" style: "background-color: blue"  "1")))))))

        (render my-site #:to "out")
    }
}

@defproc[(col [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-1 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-2 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-3 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-4 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-5 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-6 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-7 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-8 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-9 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-10 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-11 [#:element element div] [content (or/c element? attribute?)]) element?]{}
@defproc[(col-12 [#:element element div] [content (or/c element? attribute?)]) element?]{}

Also available (but I am too lazy to list them) are the variants with @tt{sm}, @tt{md}, @tt{lg}, or @tt{xl}.

Once such example:

@defproc[(col-sm-4 [#:element element div] [content (or/c element? attribute?)]) element?]{}

@section{Cards}

Bootstrap's cards are a key aspect of Bootstrap's visual language. 

@defproc[(card [#:element element div] [content (or/c element? attribute?)]) element?]{
  This is a psuedo element -- a @racket[div] with the @racket[class:] @racket["card"]  

  Things that can be nested inside of it: @racket[card-img-top], @racket[card-body], @racket[card-title], @racket[card-subtitle], @racket[card-text], and @racket[card-link].

  Use these as building blocks to make your own sorts of cards.  

  Examples:

  @codeblock{
    (card
      (card-img-top)
      (card-body
        (card-title "I am a card")
        (card-subtitle "with a subtitle")
        (card-text "Lorem ipsum ....") 
        (button-primary
          "Learn More")))
  }
}


@section{Buttons}

There are a variety of constructors for common Bootstrap buttons.

Each is implemented as a psuedo element, meaning that although they return @racket[button] values, you can still pass in content and attributes as you would with any html element constructors.

@defproc[(button-primary [#:element element button?] [content (or/c element? attribute?)]) element?]{

  @codeblock{
    (button-primary 
       id: "main-button"
       "My Button")
  } 
}

@defproc[(button-secondary [#:element element button?] [content (or/c element? attribute?)]) element?]{
}

@defproc[(button-success [#:element element button?] [content (or/c element? attribute?)]) element?]{
}

@defproc[(button-danger [#:element element button?] [content (or/c element? attribute?)]) element?]{
}

@defproc[(button-warning [#:element element button?] [content (or/c element? attribute?)]) element?]{
}

@defproc[(button-info [#:element element button?] [content (or/c element? attribute?)]) element?]{
}

@defproc[(button-light [#:element element button?] [content (or/c element? attribute?)]) element?]{
}

@defproc[(button-dark [#:element element button?] [content (or/c element? attribute?)]) element?]{
}

@defproc[(button-link [#:element element button?] [content (or/c element? attribute?)]) element?]{
}


