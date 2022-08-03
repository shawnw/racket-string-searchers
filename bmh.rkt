#lang typed/racket/base

;;; Boyer-Moore-Horspool string searching

(require "private/strfuncs.rkt")

(provide string-contains string-contains-ci make-matcher matcher?
         matcher-ci? matcher-pattern find-string find-all-strings)

(struct matcher ([pattern : String] [table : (Immutable-HashTable Char Index)] [s= : String=]))

(: matcher-ci? (-> matcher Boolean))
(define (matcher-ci? m)
  (eq? (matcher-s= m) substring-ci=?))

;;; See https://stackoverflow.com/q/73175239/9952196 for discussion
;;; about the commented out functions here.

#;(: pick-first (All (a) (-> a a a)))
#;(define (pick-first v1 v2) v1)

#;(require/typed racket/hash
  [hash-union (All (v) (->* ((Immutable-HashTable Any v)) (#:combine (-> v v v) #:combine/key (-> Any v v v)) #:rest (HashTable Any v) (Immutable-HashTable Any v)))])
#;(: make-ci-table (-> String (Immutable-HashTable Char Index)))
#;(define (make-ci-table pat)
  (let ([patlen (string-length pat)])
    (cast ((inst hash-union Index)
           (for/hash : (Immutable-HashTable Char Index) ([i (in-range patlen)])
             (values (char-upcase (string-ref pat i)) (- patlen 1 i)))
           (for/hash  : (Immutable-HashTable Char Index) ([i (in-range patlen)])
             (values (char-downcase (string-ref pat i)) (- patlen 1 i)))
           #:combine pick-first)
          (Immutable-HashTable Char Index))))

(: make-ci-table (-> String (Immutable-HashTable Char Index)))
(define (make-ci-table pat)
  (let ([patlen (string-length pat)])
    (for/fold : (Immutable-HashTable Char Index)
              ([acc : (Immutable-HashTable Char Index) (make-immutable-hasheqv)])
              ([i (in-range (- patlen 1))])
      (let* ([c (string-ref pat i)]
             [uc (char-upcase c)]
             [lc (char-downcase c)]
             [len (cast (- patlen 1 i) Index)])
        (if (char=? uc lc)
            (hash-set acc uc len)
            (hash-set* acc uc len lc len))))))

(: make-table (-> String (Immutable-HashTable Char Index)))
(define (make-table pat)
  (let ([patlen (string-length pat)])
    (for/hasheqv : (Immutable-HashTable Char Index) ([i (in-range (- patlen 1))])
      (values (string-ref pat i) (cast (- patlen 1 i) Index)))))

(: make-matcher (->* (String) (#:case-insensitive Boolean) matcher))
(define (make-matcher pat #:case-insensitive [case-insensitive? #f])
  (let ([patlen (string-length pat)])
    (matcher (string->immutable-string pat)
             (if case-insensitive?
                 (make-ci-table pat)
                 (make-table pat))
             (if case-insensitive? substring-ci=? substring=?))))

(: %find-string (-> matcher String Index Index (Option Index)))
(define (%find-string m text start end)
  (let* ([table (matcher-table m)]
         [s= (matcher-s= m)]
         [pat (matcher-pattern m)]
         [patlen (string-length pat)]
         [textlen (+ start (- end start))])
    (let loop ([skip : Index start])
      (if (>= (- textlen skip) patlen)
          (if (s= text skip pat)
                skip
              (loop (cast (+ skip (hash-ref/default table (string-ref text (- (+ skip patlen) 1)) patlen)) Index)))
          #f))))

(: find-string (->* (matcher String) (Index Index) (Option Index)))
(define (find-string m text [start 0] [end (string-length text)])
  (check-substring-spec 'find-string text start end)
  (%find-string m text start end))

(: find-all-strings (->* (matcher String) (Index Index #:overlap Boolean) (Listof Index)))
(define (find-all-strings m text [start 0] [end (string-length text)] #:overlap [overlap? #t])
  (check-substring-spec 'find-all-strings text start end)
  (let* ([patlen (string-length (matcher-pattern m))]
         [offset : Index (if overlap? 1 patlen)])
    (let loop ([acc : (Listof Index) '()]
               [start : Index start])
      (if (< start end)
          (let ([pos (%find-string m text start end)])
            (if pos
                (loop (cons pos acc) (cast (+ pos offset) Index))
                (reverse acc)))
          (reverse acc)))))


(define-syntax-rule (%string-contains case-insensitive? text pat start end)
  (let ([bmh (make-matcher pat #:case-insensitive case-insensitive?)])
    (%find-string bmh text start end)))

(: string-contains (->* (String String) (Index Index) (Option Index)))
(define (string-contains text pat [start 0] [end (string-length text)])
  (check-substring-spec 'string-contains text start end)
  (%string-contains #f text pat start end))

(: string-contains-ci (->* (String String) (Index Index) (Option Index)))
(define (string-contains-ci text pat [start 0] [end (string-length text)])
  (check-substring-spec 'string-contains-ci text start end)
  (%string-contains #t text pat start end))
