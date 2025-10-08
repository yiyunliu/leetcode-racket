#lang typed/racket

(: coin-change (-> (Listof Integer) Integer Integer))
(define (coin-change coins amount)
  (: cache (Vectorof (Promise (U Integer False))))
  (define cache (build-vector amount
                              (lambda ([x : Integer])
                                (delay
                                  (loop x)))))

  (: cache-wrap (-> Integer (U Integer False)))
  (define (cache-wrap amount)
    (force (vector-ref cache amount)))

  (: loop (-> Integer (U Integer False)))
  (define (loop amount)
    (cond
      [(= amount 0) 0]
      [else
       (for/fold
           ([acc : (U Integer False) #f])
           ([coin : Integer coins])
         (define new-amount (- amount coin))
         (cond
           [(negative? new-amount) acc]
           [else
            (define candidate (cache-wrap new-amount))
            (cond
              [(false? candidate) acc]
              [(false? acc) (add1 candidate)]
              [else (min (add1 candidate) acc)])]))]))
  (or (loop amount) -1))
