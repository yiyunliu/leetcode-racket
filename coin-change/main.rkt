#lang racket

(module M typed/racket
  (provide coin-change)
  (: coin-change (-> (Listof Integer) Integer Integer))
  (define (coin-change coins amount)
    (: cache (Vectorof (U False 'Impossible Integer)))
    (define cache (make-vector (+ amount 1) #f))
    (vector-set! cache 0 0)
    (for ([amount : Integer (in-range 1 (add1 amount))])
      (let ([result
             (for/fold ([acc : (U False Integer) #f])
                       ([coin coins])
               (let ([amount (- amount coin)])
                 (if (negative? amount)
                     acc
                     (let ([cnt (vector-ref cache amount)])
                       (cond
                         [(false? cnt) (error "Impossible")]
                         [(or (eqv? cnt 'Impossible) (negative? cnt)) acc]
                         [(and acc cnt) (min acc (add1 cnt))]
                         [cnt (add1 cnt)]
                         [else acc])))))])
        (vector-set! cache amount (or result 'Impossible))))
    (let ([result (vector-ref cache amount)])
      (case result
        ['Impossible -1]
        ['#f (error "impossible")]
        [else result]))))

(require 'M)
