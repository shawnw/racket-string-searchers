#lang typed/racket/base

;;; Utility functions used by the searcher modules

(provide hash-ref/default substring=? substring-ci=? String= check-substring-spec subbytes=?)

(: check-substring-spec (-> Symbol String Index Index Void))
(define (check-substring-spec proc s start end)
  (cond
    ((or (< start 0) (> start (string-length s)))
     (raise-range-error proc "string" "starting " start s 0 (string-length s) #f))
    ((or (> start end) (> end (string-length s)))
     (raise-range-error proc "string" "ending " end s start (string-length s) 0))
    (else (void))))

(define-type String= (-> String Index String Boolean))
(define-type Bytes= (-> Bytes Index Bytes Boolean))

(: hash-ref/default (All (k v) (-> (HashTable k v) k v v)))
(define (hash-ref/default table key default)
  (hash-ref table key (lambda () default)))

(: str= ((Char Char -> Boolean) String Index String -> Boolean))
(define (str= c=? text start pat)
  (for/and : Boolean ([i (in-naturals start)]
                      [j (in-range (string-length pat))])
    (c=? (string-ref text i) (string-ref pat j))))

(: substring=? String=)
(define (substring=? text start pat)
  (str= char=? text start pat))

(: substring-ci=? String=)
(define (substring-ci=? text start pat)
  (str= char-ci=? text start pat))

(: subbytes=? Bytes=)
(define (subbytes=? text start pat)
  (for/and : Boolean ([i (in-naturals start)]
                      [j (in-range (bytes-length pat))])
    (= (bytes-ref text i) (bytes-ref pat j))))
