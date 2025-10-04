#lang racket

(require data/heap)

(define/contract (top-k-frequent nums k)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))

  (define ht (make-hash))

  (for ([n nums])
    (hash-update! ht n add1 0))

  (define heap (make-heap (lambda (x y) (> (hash-ref ht x) (hash-ref ht y)))))
  (for ([k (in-hash-keys ht)])
    (heap-add! heap k))
  (for/list ([_ (in-range k)])
    (let ([k (heap-min heap)])
      (heap-remove-min! heap)
      k)))
