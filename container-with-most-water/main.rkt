#lang racket
(define/contract (max-area _height)
  (-> (listof exact-integer?) exact-integer?)
  (define height (list->vector _height))
  (define len (vector-length height))
  (define (get i) (vector-ref height i))
  (define (area-between i j)
    (* (- j i) (min (get i) (get j))))
  (define (loop left right area-acc)
    (cond
      [(= left right) area-acc]
      [(< (get left) (get right))
       (define new-left
         (for/fold
           ([new-left (add1 left)])
           ([_ (in-cycle '(#f))])
           #:break (>= (get new-left) (get left))
           (add1 new-left)))
       (define new-area (area-between new-left right))
       (loop new-left right (max new-area area-acc))]
      [else
       (define new-right
         (for/fold
           ([new-right (sub1 right)])
           ([_ (in-cycle '(#f))])
           #:break (>= (get new-right) (get right))
           (sub1 new-right)))

       (define new-area (area-between left new-right))
       (loop left new-right (max new-area area-acc))]))
  (loop 0 (sub1 len) (area-between 0 (sub1 len))))
