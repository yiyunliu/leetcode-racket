#lang racket


(struct tree-node
  (val left right) #:mutable #:transparent)

(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (max-path-sum root)
  (-> (or/c tree-node? #f) exact-integer?)
  (define (loop root)
    (cond
      [root
       (define val (tree-node-val root))
       (define left (tree-node-left root))
       (define right (tree-node-right root))
       (let-values
           ([(max-up-l max-down-l) (loop left)]
            [(max-up-r max-down-r) (loop right)])
         (define max-up
           (max (+ (or max-up-l 0) val) (+ (or max-up-r 0) val) val ))
         (define max-down
           (max max-up (+ (or max-up-l 0) (or max-up-r 0) val) (or max-down-l max-up) (or max-down-r max-up)))
         (values max-up max-down))]
      [else
       (values #f #f)]))
  (define-values (_ max-down) (loop root))
  max-down)
