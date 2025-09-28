#lang racket

(struct list-node
  (val next) #:mutable #:transparent)

(define (make-list-node [val 0])
  (list-node val #f))


(define/contract (reverse-list head)
  (-> (or/c list-node? #f) (or/c list-node? #f))
  (define (loop prev head)
    (if (false? head)
        prev
        (let ([val (list-node-val head)]
              [next (list-node-next head)])
          (set-list-node-next! head prev)
          (loop head next))))
  (loop #f head))
