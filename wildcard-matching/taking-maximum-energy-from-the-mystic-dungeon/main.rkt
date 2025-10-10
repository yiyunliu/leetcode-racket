#lang typed/racket

(: maximum-energy (-> (Listof Integer) Integer Integer))
(define (maximum-energy _energy k)
  (define energy (list->vector _energy))
  (define len (vector-length energy))
  (or (for/fold
      ([acc : (U Integer #f) #f])
      ([i (in-range k)])
        (let-values
            ([(acc _)
              (for/fold
              ([acc : (U Integer #f) acc]
               [curr : Integer 0])
              ([j (in-range (- len 1 i) -1 (- k))])
                (define num (vector-ref energy j))
                (cond
                  [(false? acc)
                   (values num num)]
                  [else
                   (define new-curr (+ num curr))
                   (values (max new-curr acc) new-curr)]))])
          acc)) 0))
