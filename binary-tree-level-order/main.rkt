#lang racket

(require data/queue)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (level-order root)
  (-> (or/c tree-node? #f) (listof (listof exact-integer?)))
  (define q (make-queue))
  (enqueue! q root)
  (define (loop acc)
    (cond
      [(queue-empty? q) (reverse acc)]
      [else
       (define level-nodes
         (for/fold ([acc '()])
                   ([_ (in-range (queue-length q))])
           (define root (dequeue! q))
           (cond
             [root
              (define left (tree-node-left root))
              (define right (tree-node-right root))
              (define val (tree-node-val root))
              (enqueue! q left)
              (enqueue! q right)
              (cons val acc)]
             [else acc])))
       (loop (cond
               [(null? level-nodes) acc]
               [else (cons (reverse level-nodes) acc)]))]))
  (loop '()))
