#lang racket

(module M typed/racket
  (provide combination-sum4)

  (: combination-sum4 (-> (Listof Integer) Integer Integer))
  (define (combination-sum4 _nums target)

    ;; initialization
    (define nums (list->vector _nums))
    ;; cache[i] : the # of possible combinations to reach target i
    (: cache (Vectorof Integer))
    (define cache (make-vector (add1 target) 0))
    (vector-set! cache 0 1)

    ;; main loop
    (for ([i (in-range 1 (vector-length cache))])
      (for ([num nums])
        (define j (- i num))
        (unless (negative? j)
          (vector-set! cache i (+ (vector-ref cache i) (vector-ref cache j))))))
    (vector-ref cache target)))

(require 'M)
