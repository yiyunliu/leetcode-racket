#lang racket

(struct tree-node
  (val left right) #:transparent)


(define (build-tree _preorder _inorder)
  (define preorder (list->vector _preorder))
  (define inorder (list->vector _inorder))
  (define inorder-map
    (for/hash ([(a idx) (in-indexed inorder)])
      (values a idx)))
  (define (get-inorder i)
    (hash-ref inorder-map i))

  (let loop
      ([pre-start 0]
       [pre-end (vector-length preorder)]
       [in-start 0]
       [in-end  (vector-length preorder)])
    (cond
      [(zero? (- in-end in-start)) #f]
      [else
       (define node (vector-ref preorder pre-start))
       (define left-size (- (get-inorder node) in-start))
       (define left-node (loop (add1 pre-start)
                               (+ 1 left-size pre-start)
                               in-start
                               (+ in-start left-size)))
       (define right-node (loop (+ pre-start 1 left-size)
                                pre-end
                                (+ in-start 1 left-size)
                                in-end))
       (tree-node node left-node right-node)])))
