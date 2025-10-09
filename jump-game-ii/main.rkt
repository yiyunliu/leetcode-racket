#lang typed/racket

(require racket/promise)

(: jump (-> (Listof Integer) Integer))
(define (jump _nums)
  (define nums (list->vector _nums))
  (define len (vector-length nums))

  (: cache (Vectorof (Promise (U Integer #f))))
  (define cache (build-vector
                 len
                 (lambda ([k : Integer])
                   (delay (go k)))))
  (: cache-ref (-> Integer (U Integer #f)))
  (define (cache-ref i)
    (force (vector-ref cache i)))

  (: go (-> Integer (U Integer #f)))
  (define (go i)
    (cond
      [(= i (- len 1)) 0]
      [else
       (define num (vector-ref nums i))
       (for/fold
           ([acc-min : (U Integer #f) #f])
           ([k (in-range (add1 i) (min len (+ 1 i num)))])
         (define num-k (cache-ref k))
         (cond
           [(false? num-k) acc-min]
           [(false? acc-min) (add1 num-k)]
           [else (min acc-min (add1 num-k))]))]))
  (or (go 0) -1))
