#lang typed/racket/base

;;; Aho-Corasick

(require "private/strfuncs.rkt")

(provide matcher? make-matcher list->matcher matcher-patterns find-string find-all-strings)

(struct node ([ch : Char] [word : (Option String)] [parent : (Option node)] [suffix : (Option node)]
              [dict-suffix : (Option node)] [children : (Mutable-HashTable Char node)])
  #:mutable #:transparent)

(: make-node (-> Char (Option node) node))
(define (make-node ch parent)
  (node ch #f parent #f #f (make-hasheqv)))

(: ensure-child! (-> (Mutable-HashTable Char node) Char (Option node) Any))
(define (ensure-child! table ch parent)
  (unless (hash-has-key? table ch)
    (hash-set! table ch (make-node ch parent))))

(: insert-string! (-> (Mutable-HashTable Char node) String Any))
(define (insert-string! table s)
  (ensure-child! table (string-ref s 0) #f)
  (let ([last-node (for/fold : node
                             ([n (hash-ref table (string-ref s 0))])
                             ([ch (in-string s 1)])
                     (let ([children (node-children n)])
                       (ensure-child! children ch n)
                       (hash-ref children ch)))])
    (set-node-word! last-node s)))

(: add-suffix-links! (-> node node Any))
(define (add-suffix-links! root n)
  (cond
    ((eq? root n)
     (set-node-suffix! n root)
     (set-node-dict-suffix! n root))
    ((eq? root (node-parent n))
     (set-node-suffix! n root)
     (if (node-word n)
         (set-node-dict-suffix! n n)
         (set-node-dict-suffix! n (node-dict-suffix root))))
    (else
     (let ([ch (node-ch n)])
       (let loop ([better (cast (node-suffix (cast (node-parent n) node)) node)])
         (cond
           ((hash-has-key? (node-children better) ch)
            (set-node-suffix! n (hash-ref (node-children better) ch)))
           ((eq? better root)
            (set-node-suffix! n root))
           (else
            (loop (cast (node-suffix better) node))))))
     (if (node-word n)
         (set-node-dict-suffix! n n)
         (set-node-dict-suffix! n (node-dict-suffix (cast (node-suffix n) node)))))))

(require/typed data/queue
  [#:opaque NodeQueue queue?]
  [make-queue (-> NodeQueue)]
  [enqueue! (-> NodeQueue node Void)]
  [dequeue! (-> NodeQueue node)]
  [queue-empty? (-> NodeQueue Boolean)])

(: add-links! (-> node Any))
(define (add-links! root)
  (let ([queue (make-queue)])
    (enqueue! queue root)
    (let loop ()
      (unless (queue-empty? queue)
        (let ([current (dequeue! queue)])
          (add-suffix-links! root current)
          (for ([n (in-hash-values (node-children current))]) (enqueue! queue n)))
        (loop)))))

#;(require (prefix-in bq- pfds/queue/bankers))
#;(define (add-links! root)
  (let loop ([queue (bq-queue root)])
    (unless (bq-empty? queue)
      (let ([current (bq-head queue)])
        (add-suffix-links! root current)
        (loop (for/fold : (bq-Queue node)
                        ([queue (bq-tail queue)])
                        ([n (in-hash-values (node-children current))])
                (bq-enqueue n queue)))))))

(struct matcher ([patterns : (Listof String)] [root : node]))

(: make-matcher (String * -> matcher))
(define (make-matcher . strings)
  (list->matcher strings))

(: list->matcher ((Listof String) -> matcher))
(define (list->matcher strings)
  (let* ([patterns (map string->immutable-string strings)]
         [table : (Mutable-HashTable Char node) (make-hasheqv)]
         [root (node #\u0000 #f #f #f #f table)])
    (for ([str : String (in-list patterns)]) (insert-string! table str))
    (for ([child : node (in-hash-values table)]) (set-node-parent! child root))
    (add-links! root)
    (matcher patterns root)))

;;; This is hideous. Must rewrite to be more schemey.
(: find-string (->* (matcher String) (Index Index) (Option (Pair Index String))))
(define (find-string m text (start 0) (end (string-length text)))
  (check-substring-spec 'find-string text start end)
  (let* ([root (matcher-root m)]
         [current root])
    (for/or : (Option (Pair Index String))
            ([i (in-range start end)]
             [ch (in-string text start end)])
      (let loop ()
        (cond
          ((hash-has-key? (node-children current) ch)
           (set! current (hash-ref (node-children current) ch)))
          ((eq? current root) (void))
          (else
           (set! current (cast (node-suffix current) node))
           (loop))))
      (let ([check (cast (node-dict-suffix current) node)])
        (cond
          ((eq? check root) #f)
          ((string? (node-word check))
           (cons (cast (- (+ i 1) (string-length (cast (node-word check) String))) Index)
                 (cast (node-word check) String)))
          (else #f))))))

(: find-all-strings (->* (matcher String) (Index Index) (Listof (Pair Index String))))
(define (find-all-strings m text (start 0) (end (string-length text)))
  (check-substring-spec 'find-all-strings text start end)
  (let* ([root (matcher-root m)]
         [current root])
    (for/fold : (Listof (Pair Index String))
              ([results : (Listof (Pair Index String)) '()]
               #:result (reverse results))
              ([i (in-range start end)]
               [ch (in-string text start end)])
      (let loop ()
        (cond
          ((hash-has-key? (node-children current) ch)
           (set! current (hash-ref (node-children current) ch)))
          ((eq? current root) (void))
          (else
           (set! current (cast (node-suffix current) node))
           (loop))))
      (let ([check (cast (node-dict-suffix current) node)])
        (cond
          ((eq? check root) results)
          ((node-word check)
           (cons (cons (cast (- (+ i 1) (string-length (cast (node-word check) String))) Index)
                       (cast (node-word check) String))
                 results))
          (else results))))))
