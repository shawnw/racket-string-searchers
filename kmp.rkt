#lang typed/racket/base

;;; Knuth-Morris-Pratt string searching

(define-type Char= (-> Char Char Boolean))

(require "private/strfuncs.rkt")
(provide string-contains string-contains-ci make-matcher matcher?
         matcher-ci? matcher-pattern find-string find-all-strings)

;;; Taken from SRFI-13 reference and converted to Typed Racket
;;; (make-kmp-restart-vector pattern [c= start end]) -> integer-vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compute the KMP restart vector RV for string PATTERN.  If
;;; we have matched chars 0..i-1 of PATTERN against a search string S, and
;;; PATTERN[i] doesn't match S[k], then reset i := RV[i], and try again to
;;; match S[k].  If RV[i] = -1, then punt S[k] completely, and move on to
;;; S[k+1] and PATTERN[0] -- no possible match of PAT[0..i] contains S[k].
;;;
;;; In other words, if you have matched the first i chars of PATTERN, but
;;; the i+1'th char doesn't match, RV[i] tells you what the next-longest
;;; prefix of PATTERN is that you have matched.
;;;
;;; - C= (default CHAR=?) is used to compare characters for equality.
;;;   Pass in CHAR-CI=? for case-folded string search.
;;;
;;; - START & END restrict the pattern to the indicated substring; the
;;;   returned vector will be of length END - START. The numbers stored
;;;   in the vector will be values in the range [0,END-START) -- that is,
;;;   they are valid indices into the restart vector; you have to add START
;;;   to them to use them as indices into PATTERN.
;;;
;;; I've split this out as a separate function in case other constant-string
;;; searchers might want to use it.
;;;
;;; E.g.:
;;;    a b d  a b x
;;; #(-1 0 0 -1 1 2)

(: make-kmp-restart-vector (->* (String) (Char= Index Index) (Vectorof Integer)))
(define (make-kmp-restart-vector pattern [c= char=?] [start 0] [end (string-length pattern)])
    (let* ((rvlen (- end start))
	   (rv (make-vector rvlen -1)))
      (when (> rvlen 0)
	  (let ((rvlen-1 (- rvlen 1))
		(c0 (string-ref pattern start)))

	    ;; Here's the main loop. We have set rv[0] ... rv[i].
	    ;; K = I + START -- it is the corresponding index into PATTERN.
	    (let lp1 ((i 0) (j -1) (k start))
	      (when (< i rvlen-1)
		  ;; lp2 invariant:
		  ;;   pat[(k-j) .. k-1] matches pat[start .. start+j-1]
		  ;;   or j = -1.
		  (let lp2 ((j j))
		    (cond ((= j -1)
			   (let ((i1 (+ 1 i)))
			     (when (not (c= (string-ref pattern (+ k 1)) c0))
				 (vector-set! rv i1 0))
			     (lp1 i1 0 (+ k 1))))
			  ;; pat[(k-j) .. k] matches pat[start..start+j].
			  ((c= (string-ref pattern k) (string-ref pattern (+ j start)))
			   (let* ((i1 (+ 1 i))
				  (j1 (+ 1 j)))
			     (vector-set! rv i1 j1)
			     (lp1 i1 j1 (+ k 1))))
			  (else (lp2 (vector-ref rv j)))))))))
      rv))

;;; Knuth-Morris-Pratt string searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See
;;;     "Fast pattern matching in strings"
;;;     SIAM J. Computing 6(2):323-350 1977
;;;     D. E. Knuth, J. H. Morris and V. R. Pratt
;;; also described in
;;;     "Pattern matching in strings"
;;;     Alfred V. Aho
;;;     Formal Language Theory - Perspectives and Open Problems
;;;     Ronald V. Brook (editor)
;;; This algorithm is O(m + n) where m and n are the 
;;; lengths of the pattern and string respectively

;;; KMP search source[start,end) for PATTERN. Return starting index of
;;; leftmost match or #f.

(: %kmp-search (-> String String Char= Index Index Index Index (Option Index)))
(define (%kmp-search pattern text c= p-start p-end t-start t-end)
  (let ((plen (- p-end p-start))
	(rv (make-kmp-restart-vector pattern c= p-start p-end)))

    ;; The search loop. TJ & PJ are redundant state.
    (let lp ((ti t-start) (pi 0)
	     (tj (- t-end t-start)) ; (- tlen ti) -- how many chars left.
	     (pj plen))		 ; (- plen pi) -- how many chars left.

      (if (= pi plen)
	  (cast (- ti plen) Index)			; Win.
	  (and (<= pj tj)		; Lose.
	       (if (c= (string-ref text ti) ; Search.
		       (string-ref pattern (+ p-start pi)))
		   (lp (+ 1 ti) (+ 1 pi) (- tj 1) (- pj 1)) ; Advance.
		   
		   (let ((pi (vector-ref rv pi))) ; Retreat.
		     (if (= pi -1)
			 (lp (+ ti 1) 0  (- tj 1) plen) ; Punt.
			 (lp ti       pi tj       (- plen pi))))))))))

(: string-contains (->* (String String) (Index Index) (Option Index)))
(define (string-contains text pattern [t-start 0] [t-end (string-length text)])
  (check-substring-spec 'string-contains text t-start t-end)
  (%kmp-search pattern text char=? 0 (string-length pattern) t-start t-end))

(: string-contains-ci (->* (String String) (Index Index) (Option Index)))
(define (string-contains-ci text pattern [t-start 0] [t-end (string-length text)])
  (check-substring-spec 'string-contains-ci text t-start t-end)
  (%kmp-search pattern text char-ci=? 0 (string-length pattern) t-start t-end))

(struct matcher ([pattern : String] [vec : (Vectorof Integer)] [c= : Char=]))

(: make-matcher (->* (String) (#:case-insensitive Boolean) matcher))
(define (make-matcher pat #:case-insensitive [case-insensitive? #f])
  (let ([c= (if case-insensitive? char-ci=? char=?)])
    (matcher (string->immutable-string pat)
             (make-kmp-restart-vector pat c=)
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
