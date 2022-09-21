#lang info
(define collection "string-searchers")
(define deps '("base" "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-doc" "rackunit-typed"))
(define scribblings '(("scribblings/string-searchers.scrbl" ())))
(define pkg-desc "String Search Algorithms")
(define version "0.7")
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))
