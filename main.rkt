#lang typed/racket/base

(module+ test
  (require typed/rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require (prefix-in kmp- "kmp.rkt")
         (prefix-in bmh- "bmh.rkt")
         (prefix-in ahoc- "ahoc.rkt"))
(provide (all-from-out "kmp.rkt")
         (all-from-out "bmh.rkt")
         (all-from-out "ahoc.rkt"))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.
  (define kmp (kmp-make-matcher "test string"))
  (define kmp-ci (kmp-make-matcher "test STRing" #:case-insensitive #t))

  (define search-1 "this is a test string")
  (define search-2 "this does not have the test str")
  (define search-3 "this is a test string repeated test string")

  (check-not-false (kmp-string-contains "foo bar baz" "baz"))
  (check-false (kmp-string-contains "foo bar bad" "baz"))
  (check-not-false (kmp-string-contains-ci "foo bar baz" "BAZ"))
  (check-false (kmp-string-contains-ci "foo bar bad" "BAZ"))

  (check-pred kmp-matcher? kmp)
  (check-false (kmp-matcher-ci? kmp))
  (check-equal? (kmp-matcher-pattern kmp) "test string")
  (check-equal? (kmp-find-string kmp search-1) 10)
  (check-false (kmp-find-string kmp search-1 11))
  (check-false (kmp-find-string kmp search-1 5 15))
  (check-equal? (kmp-find-string kmp search-3 5 25) 10)
  (check-equal? (kmp-find-string kmp search-3 11) 31)
  (check-false (kmp-find-string kmp search-2))
  (check-equal? (kmp-find-all-strings kmp search-1) '(10))
  (check-equal? (kmp-find-all-strings kmp search-3) '(10 31))
  (check-equal? (kmp-find-all-strings kmp search-3 12) '(31))
  (check-equal? (kmp-find-all-strings kmp search-3 5 25) '(10))
  (check-equal? (kmp-find-all-strings kmp search-2) '())

  (check-pred kmp-matcher? kmp-ci)
  (check-true (kmp-matcher-ci? kmp-ci))
  (check-equal? (kmp-find-string kmp-ci search-1) 10)
  (check-false (kmp-find-string kmp-ci search-1 11))
  (check-false (kmp-find-string kmp-ci search-1 5 15))
  (check-equal? (kmp-find-string kmp-ci search-3 5 25) 10)
  (check-equal? (kmp-find-string kmp-ci search-3 11) 31)
  (check-false (kmp-find-string kmp-ci search-2))
  (check-equal? (kmp-find-all-strings kmp-ci search-1) '(10))
  (check-equal? (kmp-find-all-strings kmp-ci search-3) '(10 31))
  (check-equal? (kmp-find-all-strings kmp-ci search-3 12) '(31))
  (check-equal? (kmp-find-all-strings kmp-ci search-3 5 25) '(10))
  (check-equal? (kmp-find-all-strings kmp-ci search-2) '())

  (define kmp-bb (kmp-make-matcher "bb"))
  (check-equal? (kmp-find-all-strings kmp-bb "bbb") '(0 1))
  (check-equal? (kmp-find-all-strings kmp-bb "bbb" #:overlap #f) '(0))

  (define bmh (bmh-make-matcher "test string"))
  (define bmh-ci (bmh-make-matcher "test STRing" #:case-insensitive #t))

  (check-not-false (bmh-string-contains "foo bar baz" "baz"))
  (check-false (bmh-string-contains "foo bar bad" "baz"))
  (check-not-false (bmh-string-contains-ci "foo bar baz" "BAZ"))
  (check-false (bmh-string-contains-ci "foo bar bad" "BAZ"))

  (check-pred bmh-matcher? bmh)
  (check-false (bmh-matcher-ci? bmh))
  (check-equal? (bmh-matcher-pattern bmh) "test string")
  (check-equal? (bmh-find-string bmh search-1) 10)
  (check-false (bmh-find-string bmh search-1 11))
  (check-false (bmh-find-string bmh search-1 5 15))
  (check-equal? (bmh-find-string bmh search-3 5 25) 10)
  (check-equal? (bmh-find-string bmh search-3 11) 31)
  (check-false (bmh-find-string bmh search-2))
  (check-equal? (bmh-find-all-strings bmh search-1) '(10))
  (check-equal? (bmh-find-all-strings bmh search-3) '(10 31))
  (check-equal? (bmh-find-all-strings bmh search-3 12) '(31))
  (check-equal? (bmh-find-all-strings bmh search-3 5 25) '(10))
  (check-equal? (bmh-find-all-strings bmh search-2) '())

  (check-pred bmh-matcher? bmh-ci)
  (check-true (bmh-matcher-ci? bmh-ci))
  (check-equal? (bmh-find-string bmh-ci search-1) 10)
  (check-false (bmh-find-string bmh-ci search-1 11))
  (check-false (bmh-find-string bmh-ci search-1 5 15))
  (check-equal? (bmh-find-string bmh-ci search-3 5 25) 10)
  (check-equal? (bmh-find-string bmh-ci search-3 11) 31)
  (check-false (bmh-find-string bmh-ci search-2))
  (check-equal? (bmh-find-all-strings bmh-ci search-1) '(10))
  (check-equal? (bmh-find-all-strings bmh-ci search-3) '(10 31))
  (check-equal? (bmh-find-all-strings bmh-ci search-3 12) '(31))
  (check-equal? (bmh-find-all-strings bmh-ci search-3 5 25) '(10))
  (check-equal? (bmh-find-all-strings bmh-ci search-2) '())

  (define bmh-bb (bmh-make-matcher "bb"))
  (check-equal? (bmh-find-all-strings bmh-bb "bbb") '(0 1))
  (check-equal? (bmh-find-all-strings bmh-bb "bbb" #:overlap #f) '(0))

  (check-not-false (bmh-byte-string-contains #"foo bar baz" #"baz"))
  (check-false (bmh-byte-string-contains #"foo bar bad" #"baz"))
  (define bmh-bs-bb (bmh-make-byte-matcher #"bb"))
  (check-equal? (bmh-find-byte-string bmh-bs-bb #"goobfbball") 5)
  (check-equal? (bmh-find-all-byte-strings bmh-bs-bb #"bbb") '(0 1))
  (check-equal? (bmh-find-all-byte-strings bmh-bs-bb #"bbb" #:overlap #f) '(0))

  (define ac (ahoc-make-matcher "aa" "bb" "bba" "cc"))
  (check-pred ahoc-matcher? ac)
  (check-equal? (ahoc-matcher-patterns ac) '("aa" "bb" "bba" "cc"))
  (check-equal? (ahoc-find-string ac "foobbar") '(3 . "bb"))
  (check-false (ahoc-find-string ac "fnord"))
  (check-equal? (ahoc-find-all-strings ac "foobbor") '((3 . "bb")))
  (check-equal? (ahoc-find-all-strings ac "fnord") '())
  (check-equal? (ahoc-find-all-strings ac "foobbaar") '((3 . "bb") (3 . "bba") (5 . "aa")))
  )
