#lang racket


(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(require data/heap)

(define/contract (merge-k-lists lists)
  (-> (listof (or/c list-node? #f)) (or/c list-node? #f))
  (define heap (make-heap (lambda (x y) (< (list-node-val x) (list-node-val y)))))
  (for ([ls lists])
    (when ls
      (heap-add! heap ls)))
  (define (loop)
    (if (zero? (heap-count heap))
        #f
        (let* ([min-lst (heap-min heap)]
               [val (list-node-val min-lst)]
               [next (list-node-next min-lst)])
          (heap-remove-min! heap)
          (when next
            (heap-add! heap next))
          (list-node val (loop)))))
  (loop))
