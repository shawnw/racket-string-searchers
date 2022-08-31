#lang typed/racket/base

;;; Utility functions used by the searcher modules

(provide hash-ref/default substring=? substring-ci=? String= check-substring-spec)

;;; Taken from SRFI-13 reference. Most of these checks aren't needed in Typed Racket
(: substring-spec-ok? (-> String Index Index Boolean))
(define (substring-spec-ok? s start end)
  (and ;(string? s)
       ;(integer? start)
       ;(exact? start)
       ;(integer? end)
       ;(exact? end)
       (<= 0 start)
       (<= start end)
       (<= end (string-length s))))

;;; Taken from SRFI-13 reference
(: check-substring-spec (-> Any String Index Index Any))
(define (check-substring-spec proc s start end)
  (unless (substring-spec-ok? s start end)
      (error "Illegal substring spec." proc s start end)))

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
