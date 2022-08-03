#lang typed/racket/base

;;; Utility functions used by the searcher modules

(require/typed/provide srfi/13
  [check-substring-spec (-> Any String Integer Integer Any)])
(provide hash-ref/default substring=? substring-ci=? String=)

(define-type String= (-> String Index String Boolean))

(: hash-ref/default (All (k v) (-> (HashTable k v) k v v)))
(define (hash-ref/default table key default)
  (hash-ref table key (lambda () default)))

(define-syntax-rule (str= c=? text start pat)
  (for/and : Boolean ([i (in-naturals start)]
                      [j (in-range (string-length pat))])
    (c=? (string-ref text i) (string-ref pat j))))

#;(define-syntax-rule (str= c=? text start pat)
  (let ([patlen (string-length pat)])
    (let loop ([i start]
               [j 0])
      (cond ((= j patlen) #t)
            ((c=? (string-ref text i) (string-ref pat j)) (loop (+ i 1) (+ j 1)))
            (else #f)))))

(: substring=? String=)
(define (substring=? text start pat)
  (str= char=? text start pat))

(: substring-ci=? String=)
(define (substring-ci=? text start pat)
  (str= char-ci=? text start pat))
