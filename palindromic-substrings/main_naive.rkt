#lang racket

(define (is-palindrome? s)
  (define len (string-length s))
  (define mid (quotient len 2))
  (for/and
    ([idx (in-range mid)])
    (eqv? (string-ref s idx) (string-ref s (- len 1 idx)))))

(define/contract (count-substrings s)
  (-> string? exact-integer?)
  (define len (string-length s))
  (for*/fold 
    ([acc 0])
    ([start (in-range len)]
     [end (in-inclusive-range (add1 start) len)])
    (if (is-palindrome? (substring s start end))
        (add1 acc)
        acc)))
