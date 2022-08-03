#lang typed/racket/base

;;; Knuth-Morris-Pratt string searching

(define-type Char= (-> Char Char Boolean))

(require "private/strfuncs.rkt")
(require/typed srfi/13
  [string-contains (->* (String String) (Index Index) (Option Index))]
  [string-contains-ci (->* (String String) (Index Index) (Option Index))]
  [make-kmp-restart-vector (->* (String) (Char= Index Index) (Vectorof Integer))])

;; The srfi-13 string-contains does a KMP search already; re-use it.

(provide string-contains string-contains-ci make-matcher matcher?
         matcher-ci? matcher-pattern find-string find-all-strings)

(struct matcher ([pattern : String] [vec : (Immutable-Vectorof Integer)] [c= : Char=]))

(: make-matcher (->* (String) (#:case-insensitive Boolean) matcher))
(define (make-matcher pat #:case-insensitive [case-insensitive? #f])
  (let ([c= (if case-insensitive? char-ci=? char=?)])
    (matcher (string->immutable-string pat)
             (vector->immutable-vector (make-kmp-restart-vector pat c=))
             c=)))

(: matcher-ci? (-> matcher Boolean))
(define (matcher-ci? m)
  (eq? (matcher-c= m) char-ci=?))

(: %find-string (-> matcher String Index Index (Option Index)))
(define (%find-string m text start end)
  ;; Stolen from SRFI-13 source
  ;; Knuth-Morris-Pratt string searching
  ;;
  ;; See
  ;;     "Fast pattern matching in strings"
  ;;     SIAM J. Computing 6(2):323-350 1977
  ;;     D. E. Knuth, J. H. Morris and V. R. Pratt
  ;; also described in
  ;;     "Pattern matching in strings"
  ;;     Alfred V. Aho
  ;;     Formal Language Theory - Perspectives and Open Problems
  ;;     Ronald V. Brook (editor)
  ;; This algorithm is O(m + n) where m and n are the
  ;; lengths of the pattern and string respectively

  ;; KMP search source[start,end) for PATTERN. Return starting index of
  ;; leftmost match or #f.
  (let* ([pattern (matcher-pattern m)]
         [plen (string-length pattern)]
         [rv (matcher-vec m)]
         [c= (matcher-c= m)])
    ;; The search loop. TJ & PJ are redundant state.
    (let lp ([ti start]
             [pi 0]
             [tj (cast (- end start) Index)] ; (- tlen ti) -- how many chars left.
             [pj plen])        ; (- plen pi) -- how many chars left.

      (if (= pi plen)
          (cast (- ti plen) Index)                 ; Win.
          (and (<= pj tj)             ; Lose.
               (if (c= (string-ref text ti) ; Search.
                       (string-ref pattern pi))
               (lp (+ ti 1) (+ pi 1) (- tj 1) (- pj 1)) ; Advance.

               (let ([pi (vector-ref rv pi)]) ; Retreat.
                 (if (= pi -1)
                   (lp (+ ti 1) 0  (- tj 1) plen) ; Punt.
                   (lp ti       pi tj       (- plen pi))))))))))

(: find-string (->* (matcher String) (Index Index) (Option Index)))
(define (find-string m text [start 0] [end (string-length text)])
  (check-substring-spec 'find-string text start end)
  (%find-string m text start end))

(: find-all-strings (->* (matcher String) (Index Index #:overlap Boolean) (Listof Index)))
(define (find-all-strings m text [start 0] [end (string-length text)] #:overlap [overlap? #t])
  (check-substring-spec 'find-all-strings text start end)
  (let* ([patlen (string-length (matcher-pattern m))]
        [offset (if overlap? 1 patlen)])
    (let loop ([acc : (Listof Index) '()]
               [start : Index start])
      (if (< start end)
          (let ([pos (%find-string m text start end)])
            (if pos
                (loop (cons pos acc) (cast (+ pos offset) Index))
                (reverse acc)))
          (reverse acc)))))
