#lang racket
(struct list-node
  (val next) #:mutable #:transparent)

(define (list->list-node xs)
  (cond
    [(null? xs) #f]
    [else (list-node (car xs) (list->list-node (cdr xs)))]))

(define (list-length head)
  (let loop ([head head] [acc 0])
    (if head (loop (list-node-next head) (add1 acc)) acc)))

(define (reverse-node head)
  (let loop ([prev #f] [head head])
    (if head
        (begin
          (let ([next (list-node-next head)])
            (set-list-node-next! head prev)
            (loop head next)))
        prev)))

(define/contract (reorder-list head)
  (-> (or/c list-node? #f) void?)
  (cond
    [(or (false? head) (false? (list-node-next head)))
     (void)]
    [else
     (define len (list-length head))
     (define rev-idx (quotient (sub1 len) 2))
     (cond
       [(zero? rev-idx)
        (void)]
       [else
        (define rev-node
          (for/fold ([next head])
                    ([_ (in-range rev-idx)])
            (list-node-next next)))

        (define rev-node-next
          (reverse-node (list-node-next rev-node)))
        (set-list-node-next! rev-node #f)

        (let loop ([prev head] [head0 rev-node-next] [head1 (list-node-next head)])
          (when head0
            (set-list-node-next! prev head0)
            (loop head0 head1 (list-node-next head0))))])]))
