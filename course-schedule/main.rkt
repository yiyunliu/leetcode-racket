#lang typed/racket

(define-type Graph
  (Mutable-Vectorof (Listof Integer)))

(: can-finish (-> Integer (Listof (List Integer Integer)) Boolean))
(define (can-finish num-courses prereqs)
  (not (let ([graph (build-empty-graph num-courses)]
             [processed : (Vectorof Boolean) (make-vector num-courses #f)])
         (begin
           (for ([edge : (List Integer Integer) prereqs])
             (insert-node (first edge) (second edge) graph))
           (ann (for/or ([i (in-range num-courses)])
                  (let ([discovered : (Vectorof Boolean) (make-vector num-courses #f)])
                    (detect-cycle i graph discovered processed)))
                Boolean)))))

(: build-empty-graph (-> Integer Graph))
(define (build-empty-graph n)
  ((inst make-vector (Listof Integer)) n '()))

(: insert-node (-> Integer Integer Graph Void))
(define (insert-node x y g)
  (vector-set! g x (cons y (vector-ref g x))))

(: detect-cycle (-> Integer Graph (Vectorof Boolean) (Vectorof Boolean) Boolean))
(define (detect-cycle node graph discovered processed)
  (if (vector-ref processed node)
      false
      (let ([res : Boolean
                 (or (vector-ref discovered node)
                     (begin
                       (vector-set! discovered node true)
                       (let ([neighbors (vector-ref graph node)])
                         (for/or ([neighbor : Integer neighbors])
                           (detect-cycle neighbor graph discovered processed)))))])
        (vector-set! processed node true)
        res)))
