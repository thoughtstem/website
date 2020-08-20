#lang racket


(require scribble/html/xml
         "./attr.rkt" 
         "./style.rkt")

(define/provide-elements/not-empty rect)
(define/provide-elements/not-empty text)
(define/provide-elements/not-empty circle)
(define/provide-elements/not-empty defs)
(define/provide-elements/not-empty path)
(define/provide-elements/not-empty g)


(define/provide-attr version:)
(define/provide-attr viewBox:)
(define/provide-attr xmlns:)
(define/provide-attr xmlns:xlink:)


(define/provide-attr d:)
(define/provide-attr transform:)
(define/provide-attr x:)
(define/provide-attr y:)
(define/provide-attr cx:)
(define/provide-attr cy:)
(define/provide-attr r:)
(define/provide-attr rx:)
(define/provide-attr stroke:)
(define/provide-attr stroke-width:)
(define/provide-attr fill:)

;Filters
(define/provide-elements/not-empty filter)
(define/provide-elements/not-empty feTurbulence)
(define/provide-elements/not-empty feColorMatrix)
(define/provide-elements/not-empty feComposite)
(define/provide-elements/not-empty feMorphology)
(define/provide-elements/not-empty feGaussianBlur)
(define/provide-elements/not-empty feDisplacementMap)
(define/provide-elements/not-empty feBlend)
(define/provide-elements/not-empty feFlood)
(define/provide-elements/not-empty feOffset)
(define/provide-elements/not-empty feSpecularLighting)
(define/provide-elements/not-empty feDistantLight)

(define/provide-attr azimuth:)
(define/provide-attr elevation:)
(define/provide-attr surfaceScale:)
(define/provide-attr specularConstant:)
(define/provide-attr specularExponent:)
(define/provide-attr in:)
(define/provide-attr in2:)
(define/provide-attr result:)
(define/provide-attr operator:)
(define/provide-attr mode:)
(define/provide-attr k1:)
(define/provide-attr k2:)
(define/provide-attr k3:)
(define/provide-attr k4:)

(define/provide-attr numOctaves:)
(define/provide-attr stdDeviation:)
(define/provide-attr dx:)
(define/provide-attr dy:)
(define/provide-attr baseFrequency:)
(define/provide-attr seed:)
(define/provide-attr values:)
(define/provide-attr radius:)
(define/provide-attr xChannelSelector:)
(define/provide-attr yChannelSelector:)
(define/provide-attr scale:)
(define/provide-attr flood-opacity:)
(define/provide-attr flood-color:)


;Markers
(define/provide-elements/not-empty marker)

(define/provide-attr orient:)
(define/provide-attr refX:)
(define/provide-attr refY:)



