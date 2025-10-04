#lang racket

(module M typed/racket
  (provide max-area)
  (: max-area (-> (Listof Integer) Integer))
  (define (max-area _heights)
    (define heights (list->vector _heights))
    (: get-height (-> Integer Integer))
    (define (get-height i) (vector-ref heights i))
    (let loop : Integer ([left : Integer 0] [right : Integer (sub1 (vector-length heights))] [acc : Integer 0])
      (cond
        [(>= left right) acc]
        [else
         (define left-height (get-height left))
         (define right-height (get-height right))
         (define-values (new-left new-right)
           (if (<= left-height right-height)
               (values (add1 left) right)
               (values left (sub1 right))))
         (loop new-left new-right (max acc (* (- right left) (min left-height right-height))))]))))

(require 'M)
