#lang racket
(struct list-node
  (val next) #:mutable #:transparent)

(define/contract (remove-nth-from-end head n)
  (-> (or/c list-node? #f) exact-integer? (or/c list-node? #f))
  (define fast-pointer
    (for/fold ([acc head])
              ([_ (in-range n)])
      (list-node-next acc)))
  (cond
    [(false? fast-pointer)
     (list-node-next head)]
    [else
     (define-values
       (node _)
       (for/fold
           ([slow head]
            [fast fast-pointer])
           ([_ (in-naturals)])
         #:break (false? (list-node-next fast))
         (values (list-node-next slow) (list-node-next fast))))
     (set-list-node-next! node (list-node-next (list-node-next node)))
     head]))
