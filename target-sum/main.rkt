#lang typed/racket

(: find-target-sum-ways (-> (Listof Integer) Integer Integer))
(define (find-target-sum-ways nums target)
  (let ([nums (list->vector nums)])
    (: cache (HashTable (Pair Integer Integer) Integer))
    (define cache (make-hash))
    (: cache-ref (-> Integer Integer Integer))
    (define (cache-ref i j)
      (define p (cons i j))
      (if (hash-has-key? cache p)
          (hash-ref cache p)
          (let ([result (go i j)])
            (hash-set! cache p result)
            result)))
    (: go (-> Integer Integer Integer))
    (define (go i j)
      (cond
        [(zero? i) (if (= j 0) 1 0)]
        [else
         (define num (vector-ref nums (sub1 i)))
         (+ (cache-ref (sub1 i) (- j num))
            (cache-ref (sub1 i) (+ j num)))]))
    (go (vector-length nums) target)))
