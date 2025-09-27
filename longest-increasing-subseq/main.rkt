#lang racket

(module M typed/racket
  (provide length-of-lis)
  (: length-of-lis (-> (Listof Integer) Integer))
  (define (length-of-lis _nums)
    (: nums (Vectorof Integer))
    (define nums (list->vector _nums))
    (: longest-that-ends-at (Vectorof Integer))
    (define longest-that-ends-at (make-vector (vector-length nums) 1))
    (: max-len Integer)
    (define max-len 0)
    (for ([([num : Integer] [i : Integer]) (in-indexed nums)])
      (let ([result
             (for/fold
                 ([acc : Integer 1])
                 ([j : Integer (in-range 0 i)])
               (let ([elem-j (vector-ref nums j)])
                 (if (< elem-j num)
                     (max acc (add1 (vector-ref longest-that-ends-at j)))
                     acc)))])
        (vector-set! longest-that-ends-at i result)
        (set! max-len (max max-len result))))
    max-len))

(require 'M)
