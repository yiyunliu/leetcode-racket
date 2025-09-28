#lang racket

(struct list-node
  (val next) #:mutable #:transparent)

(define/contract (merge-two-lists list1 list2)
  (-> (or/c list-node? #f) (or/c list-node? #f) (or/c list-node? #f))
  (define (loop xs ys)
    (match (cons xs ys)
      [`(#f . ,_)
       ys]
      [`(,_ . #f)
       xs]
      [(cons (list-node x xs) (list-node y ys))
       (if (< x y)
           (list-node x (loop xs (list-node y ys)))
           (list-node y (loop (list-node x xs) ys)))]))
  (loop list1 list2))
