#lang racket
(struct tree-node
  (val left right) #:mutable #:transparent)


(define (tree-equal? root subroot)
  (cond
    [(and (false? subroot) (false? root)) #t]
    [(and (tree-node? root) (tree-node? subroot))
     (match-define (tree-node val left right) root)
     (match-define (tree-node sval sleft sright) subroot)
     (and (= val sval) (tree-equal? left sleft) (tree-equal? right sright))]
    [else #f]))

(define (is-subtree root subRoot)
  (if (or (tree-equal? root subRoot)
          (and root
              (or (is-subtree (tree-node-left root) subRoot)
                  (is-subtree (tree-node-right root) subRoot))))
      #t
      #f))
