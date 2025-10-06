#lang racket


(struct tree-node
  (val left right) #:transparent)

(require racket/stream)
(define (dstream-empty s) s)
(define (dstream-concat . s0) (apply compose s0))
(define (dstream-singleton s)
  (lambda (x) (stream-cons s x)))

(define (dstream->stream s)
  (s empty-stream))

(define/contract (kth-smallest root k)
  (-> (or/c tree-node? #f) exact-integer? exact-integer?)
  (define dlist
    (let loop ([root root])
      (cond
        [(false? root) dstream-empty]
        [else (match-define (tree-node val left right) root)
              (dstream-concat (loop left) (dstream-singleton val) (loop right))])))
  (stream-ref (dstream->stream dlist) (sub1 k)))
