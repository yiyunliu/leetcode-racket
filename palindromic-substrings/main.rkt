#lang racket

;; cache : array of size of the string
;; s[i..j] is a palindrome if s[(i+1)..(j-1)] is a palindrome


(define/contract (count-substrings s)
  (-> string? exact-integer?)
  (define total-len (string-length s))
  (define cache-1 (make-vector total-len #t))
  (define cache-2 (make-vector total-len #t))
  (for/fold
      ([acc total-len])
      ([len (in-inclusive-range 2 total-len)])
    (let ([acc (for/fold
                   ([acc acc])
                   ([i (in-range 0 total-len)])
                 #:break (> (+ i len) total-len)
                 (define is-palindrome? (and (vector-ref cache-2 (add1 i)) (eqv? (string-ref s i) (string-ref s (+ i (sub1 len))))))
                 (vector-set! cache-2 i is-palindrome?)
                 (if is-palindrome?
                     (add1 acc)
                     acc))])
      (define tmp cache-1)
      (set! cache-1 cache-2)
      (set! cache-2 tmp)
      acc)))
