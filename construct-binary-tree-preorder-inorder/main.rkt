#lang typed/racket

(define-type
  tree (U tree-node #f))

(define-struct
  tree-node ([val : Integer]
             [left : tree]
             [right : tree]))

(: build-tree (-> (Listof Integer) (Listof Integer) tree))
(define (build-tree preorder inorder)
  (define preorder-map
    (for/hash : (HashTable Integer Integer)
        ([(a idx) (in-indexed preorder)])
      (values a idx)))
  (define (get-preorder i)
    (hash-ref preorder-map i))
  (let loop : (Values (Listof Integer)
                      (U (Pair (U 'left 'right) tree) #f))
       ([vals : (Listof Integer) inorder]
        [parent-preorder : Integer ])
    (cond
      [(null? vals) (values #f vals)]
      [else
       (match-define (cons val rest-vals) vals)
       (define val-preorder (get-preorder val))
       _]))
  #f)
