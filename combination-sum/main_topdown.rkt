#lang racket

(define (combination-sum _candidates target)
  (define candidates (list->vector _candidates))
  (let loop ([target target] [i 0] [picked '()] [acc '()])
    (cond
      [(= target 0) (cons picked acc)]
      [(or (>= i (vector-length candidates)) (negative? target)) acc]
      [else
       (define candidate (vector-ref candidates i))
       (let* ([acc (loop target (add1 i) picked acc)]
              [acc (loop (- target candidate) i (cons candidate picked) acc)])
         acc)])))
