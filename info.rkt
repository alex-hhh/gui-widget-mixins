#lang info
(define collection "gui-widget-mixins")
(define license '(Apache-2.0 OR MIT))
(define pkg-desc "A collection of mixin classes to improve the GUI widgets")
(define version "0.0")
(define pkg-authors '(AlexHarsanyi@gmail.com))

(define scribblings '(("scribblings/gui-widget-mixins.scrbl" ())))

(define deps '("base" "gui-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "gui-doc"))
