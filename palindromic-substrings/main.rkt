#lang racket

;; cache : array of size of the string
;; s[i..j] is a palindrome if s[(i+1)..(j-1)] is a palindrome


(define/contract (count-substrings s)
  (-> string? exact-integer?)
  (define total-len (string-length s))
  (define cache-1 (make-vector total-len 1))
  (define cache-2 (make-vector total-len 1))
  (for ([len (in-range 2 total-len)])
    (for ([i (in-range 0 total-len)])
      #:break (> (+ i len) total-len)
      (vector-set! cache-2 i ()))))
