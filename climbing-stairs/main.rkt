#lang racket

(module M typed/racket
  (provide climb-stairs)
  (: climb-stairs (-> Integer Integer))
  (define (climb-stairs n)
    (: cache (Vectorof Integer))
    (define cache (make-vector n 1))
    ;; cache[i] records the number of ways when there are (i + 1) steps remaining
    (cond
      [(>= n 2)
       (vector-set! cache 1 2)
       (for ([i : Integer (in-range 2 n)])
         (vector-set! cache i (+ (vector-ref cache (sub1 i)) (vector-ref cache (- i 2)))))
       (vector-ref cache (sub1 n))]
      [else
       1])))

(require 'M)
