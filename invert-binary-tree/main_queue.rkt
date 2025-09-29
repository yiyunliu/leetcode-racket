#lang racket

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(require data/queue)

(define/contract (invert-tree root)
  (-> (or/c tree-node? #f) (or/c tree-node? #f))
  (define qu (make-queue))
  (enqueue! qu root)
  (define (loop)
    (unless (queue-empty? qu)
      (define root (dequeue! qu))
      (when root
        (define left (tree-node-left root))
        (define right (tree-node-right root))
        (set-tree-node-left! root right)
        (set-tree-node-right! root left)
        (enqueue! qu left)
        (enqueue! qu right))
      (loop)))
  (loop)
  root)
