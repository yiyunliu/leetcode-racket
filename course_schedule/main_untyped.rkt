#lang racket

(define (can-finish num-courses prereqs)
  (not (let ([graph (build-empty-graph num-courses)]
             [processed (make-vector num-courses #f)])
         (begin
           (for ([edge prereqs])
             (insert-node (first edge) (second edge) graph))
           (for/or ([i (in-range num-courses)])
             (let ([discovered (make-vector num-courses #f)])
               (detect-cycle i graph discovered processed)))))))

(define (build-empty-graph n)
  (make-vector n '()))

(define (insert-node x y g)
  (vector-set! g x (cons y (vector-ref g x))))

(define (detect-cycle node graph discovered processed)
  (if (vector-ref processed node)
      false
      (let ([res (or (vector-ref discovered node)
                     (begin
                       (vector-set! discovered node true)
                       (let ([neighbors (vector-ref graph node)])
                         (for/or ([neighbor neighbors])
                           (detect-cycle neighbor graph discovered processed)))))])
        (vector-set! processed node true)
        res)))
