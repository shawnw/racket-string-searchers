#lang info
(define collection "string-searchers")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/string-searchers.scrbl" ())))
(define pkg-desc "String Search Algorithms")
(define version "0.5")
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))