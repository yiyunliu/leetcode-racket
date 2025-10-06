#lang racket

(struct tree-node
  (val left right))

(define/contract (is-valid-bst root)
  (-> (or/c tree-node? #f) boolean?)
  (let loop [(root root) (lower #f) (upper #f)]
    (or (false? root) (match-let ([(tree-node val left right) root])
                (and (or (false? lower) (> val lower))
                     (or (false? upper) (< val upper))
                     (loop left lower (min (or upper val) val))
                     (loop right (max (or lower val) val) upper))))))
