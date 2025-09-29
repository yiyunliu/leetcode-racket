#lang racket

(require data/queue)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(require racket/mutable-treelist)

(define/contract (level-order root)
  (-> (or/c tree-node? #f) (listof (listof exact-integer?)))
  (define visited (mutable-treelist))
  (define q (make-queue))
  (enqueue! q (cons 0 root))
  (define (loop)
    (cond
      [(queue-empty? q) (void)]
      [else
       (match-define (cons height root) (dequeue! q))
       (cond
         [(eqv? root 'push)]
         [root
          (define left (tree-node-left root))
          (define right (tree-node-right root))
          (define val (tree-node-val root))
          (enqueue! q (cons (add1 height) left))
          (enqueue! q (cons (add1 height) right))
          (when (>= height (mutable-treelist-length visited))
            (mutable-treelist-add! visited '()))
          (mutable-treelist-set! visited height (cons val (mutable-treelist-ref visited height)))
          (loop)]
         [else (loop)])]))
  (loop)
  (map reverse (mutable-treelist->list visited)))
