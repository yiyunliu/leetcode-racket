#lang typed/racket

(: is-palindrome (-> Integer Boolean))
(define (is-palindrome x)
  (cond
    [(negative? x) #f]
    [else
     (= x (let loop : Integer ([acc : Integer 0] [x : Integer x])
            (cond
              [(zero? x) acc]
              [else
               (define-values (quot rem) (quotient/remainder x 10))
               (loop (+ (* 10 acc) rem) quot)])))]))
