#lang racket

(define/contract (can-jump _nums)
  (-> (listof exact-integer?) boolean?)
  (define nums (list->vector _nums))
  (<=
   (sub1 (vector-length nums))
   (for/fold ([max-reachable 0])
             ([(n idx) (in-indexed nums)])
     #:break (> idx max-reachable)
    (max max-reachable (+ idx n)))))
