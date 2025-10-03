#lang racket

(define/contract (count-bits n)
  (-> exact-integer? (listof exact-integer?))
  (define (loop n acc)
    (cond
      [(zero? n) acc]
      [(odd? n)
       (loop (quotient n 2) (add1 acc))]
      [else (loop (quotient n 2) acc)]))
  (for/list ([i (in-range (add1 n))])
    (loop i 0)))
