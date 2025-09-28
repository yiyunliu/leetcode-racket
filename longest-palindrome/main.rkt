#lang racket

(define/contract (longest-palindrome s)
  (-> string? exact-integer?)
  (define ht (make-hash))
  (for ([ch s])
    (hash-update! ht ch add1 0))
  (let-values
      ([(result odd)
        (for/fold
            ([acc 0]
             [odd #f])
            ([(_ v) ht])
          (if (odd? v)
              (values (+ acc (sub1 v)) #t)
              (values (+ acc v) odd)))])
    (+ result (if odd 1 0))))
