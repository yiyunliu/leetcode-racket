#lang typed/racket

(define-type
  tree (U tree-node #f))

(define-struct
  tree-node ([val : Integer]
             [left : tree]
             [right : tree])
  #:transparent)

(: build-tree (-> (Listof Integer) (Listof Integer) tree))
(define (build-tree _preorder _inorder)
  (define preorder #{(list->vector _preorder) : (Vectorof Integer)})
  (define inorder #{(list->vector _inorder) : (Vectorof Integer)})
  (define inorder-map
    (for/hash : (HashTable Integer Integer)
              ([(a idx) (in-indexed inorder)])
      (values a idx)))
  (: get-inorder (-> Integer Integer))
  (define (get-inorder i)
    (hash-ref inorder-map i))


  (let loop : tree
       ([pre-start : Integer 0]
        [pre-end : Integer (vector-length preorder)]
        [in-start : Integer 0]
        [in-end : Integer (vector-length preorder)])
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
