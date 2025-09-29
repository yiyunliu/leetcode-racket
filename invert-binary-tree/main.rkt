#lang racket

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (invert-tree root)
  (-> (or/c tree-node? #f) (or/c tree-node? #f))
  (define (loop root)
    (when root
      (define left (tree-node-left root))
      (define right (tree-node-right root))
      (set-tree-node-left! root right)
      (set-tree-node-right! root left)
      (loop left)
      (loop right)))
  (loop root)
  root)
