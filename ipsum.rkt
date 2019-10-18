#lang at-exp racket

(provide ipsum picsum)

(define (picsum w h 
                #:grayscale? (grayscale? #t)
                #:blur (blur #f)
                #:random (r (random 1 1000)))

  (define params
    (string-join
      (filter 
        identity
        (list
          (and grayscale? "grayscale")
          (and blur (~a "blur=" blur))
          (and random (~a "random=" r))))
      "&"))

  (~a "https://picsum.photos/" w "/" h

      "?" params))

(define (ipsum words)
  (string-join
    (take (string-split (ipsum-text) " ") 
          words)
    " "))

(define (ipsum-text)
  @~a{
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer ex purus, consequat quis lobortis in, maximus sed diam. Vestibulum id enim mauris. Ut eu augue mauris. Praesent rutrum metus vel neque pharetra, ut tempus ante blandit. Nulla augue sapien, dignissim in diam in, hendrerit malesuada metus. Suspendisse vitae rutrum justo. Vivamus tellus velit, scelerisque vel elit consectetur, pretium vestibulum quam.  Aliquam iaculis lobortis mi, ac feugiat leo. Sed tempus accumsan eros, et venenatis lacus luctus nec. Mauris condimentum ac dolor commodo aliquam. Aenean rutrum suscipit mi, nec volutpat nibh cursus vitae. Vivamus pulvinar metus purus, nec interdum risus rhoncus eu. Ut at nibh tempor, commodo orci ac, accumsan felis. Aliquam fermentum commodo sapien id fringilla. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Integer lacinia tellus quis neque elementum, non euismod dui molestie. Vestibulum nisl nunc, tincidunt ut aliquet vitae, pretium sit amet sem. Nunc convallis luctus urna et condimentum. Pellentesque hendrerit dictum faucibus. Suspendisse ut arcu et ex accumsan ullamcorper. Etiam laoreet imperdiet luctus. Mauris tempor vehicula sapien, sed sagittis odio fringilla non.  Aliquam sed sem vehicula, consequat metus eu, eleifend eros. Donec quis nulla eu tellus vestibulum vehicula sit amet non turpis. Vestibulum purus nulla, laoreet id tincidunt vestibulum, porta maximus ipsum. Nullam vehicula convallis fermentum. Aliquam ac tortor ac sapien dictum pellentesque. Nam feugiat, nisl in imperdiet suscipit, velit leo faucibus odio, eu dapibus nisi sapien ac leo. Aliquam eget quam vitae metus pretium hendrerit. Integer vitae volutpat sapien, ac eleifend magna. Vestibulum viverra a tortor et pulvinar.  
  Phasellus nec tortor in massa imperdiet placerat. In id elit auctor, dictum metus quis, pharetra felis. Morbi at facilisis justo. Phasellus ut augue in felis efficitur condimentum vehicula vel lacus. Sed nec velit a tortor viverra molestie. Curabitur tempor ligula est, in laoreet diam dictum vel. Etiam ullamcorper porta eros, ac consequat quam. Morbi posuere sapien vitae elit pharetra lobortis. Maecenas faucibus ipsum scelerisque, tristique elit sit amet, egestas nisi. Vestibulum sodales gravida felis tristique finibus. Donec iaculis sapien vitae ante laoreet lobortis. Aliquam porta mi sed maximus pharetra. Sed eget scelerisque tortor, sit amet bibendum enim. Maecenas orci eros, efficitur eu sapien sit amet, condimentum fringilla risus.

  Integer id mollis enim. Sed vitae fringilla nunc. Donec scelerisque vestibulum aliquet. Quisque pharetra, dui ut tincidunt posuere, felis magna tristique risus, vitae facilisis mi dolor nec risus. Donec vel sagittis nulla. Praesent faucibus, odio sit amet dapibus porttitor, mi orci vulputate felis, vitae viverra orci dolor sed libero. Curabitur egestas metus eu lobortis condimentum. Quisque eu augue ligula. Donec imperdiet erat arcu. Curabitur interdum blandit consectetur. Nullam non lorem non lorem lacinia hendrerit a in arcu. Aenean euismod, augue ac mollis sollicitudin, nisl ipsum maximus massa, vitae bibendum nulla elit sit amet eros.})
