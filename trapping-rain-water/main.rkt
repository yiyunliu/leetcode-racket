#lang racket

(module M typed/racket
  (provide trap)
  (: trap (-> (Listof Integer) Integer))
  (define (trap bars)
    (let ([bars : (Vectorof Integer) (list->vector bars)])
      (: loop (-> Integer Natural Integer Natural Natural Natural))
      (define (loop left left-height right right-height acc)
        ;; left-height does not include bars[left]
        (cond
          [(> left right) acc]
          [(<= left-height right-height)
           (let ([curr-left-height (vector-ref bars left)])
             (loop (add1 left)
                   (max curr-left-height left-height)
                   right
                   right-height
                   (+ acc (max 0 (- left-height curr-left-height)))))]
          [else
           (let ([curr-right-height (vector-ref bars right)])
             (loop left
                   left-height
                   (sub1 right)
                   (max curr-right-height right-height)
                   (+ acc (max 0 (- right-height curr-right-height)))))]))
      (loop 0 0 (sub1 (vector-length bars)) 0 0))))


(require 'M)
