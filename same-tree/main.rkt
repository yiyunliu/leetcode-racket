#lang racket

(require data/queue)

(struct tree-node
  (val left right) #:mutable #:transparent)

(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (is-same-tree p q)
  (-> (or/c tree-node? #f) (or/c tree-node? #f) boolean?)
  (define queue (make-queue))

  (enqueue! queue (cons p q))

  (define (loop)
    (cond
      [(queue-empty? queue) #t]
      [else
       (define pq (dequeue! queue))
       (define p (car pq))
       (define q (cdr pq))
       (cond
         [(and (false? p) (false? q))
          (loop)]
         [(and (tree-node? p) (tree-node? q))
          (and (= (tree-node-val p) (tree-node-val q))
               (begin
                 (enqueue! queue (cons (tree-node-left p) (tree-node-left q)))
                 (enqueue! queue (cons (tree-node-right p) (tree-node-right q)))
                 (loop)))]
         [else #f])]))

  (loop))
