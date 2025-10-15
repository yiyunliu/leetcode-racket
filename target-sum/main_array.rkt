#lang typed/racket

(: find-target-sum-ways (-> (Listof Integer) Integer Integer))
(define (find-target-sum-ways nums target)
  (let ([nums (list->vector nums)])
    (: len Integer)
    (define len (vector-length nums))
    (: total Integer)
    (define total (for/sum ([num nums]) num))
    (: cache (Vectorof (Vectorof (U Integer #f))))
    (define cache (build-vector
                   len
                   (lambda (_)
                     (ann (make-vector (add1 (* 2 total)) #f) (Vectorof (U Integer #f))))))

    (: target->index (-> Integer Integer))
    (define (target->index i)
      (+ (- total target) i))

    (: cache-ref (-> Integer Integer Integer))
    (define (cache-ref i _j)
      (define j (target->index _j))
      (define maybe-result (vector-ref (vector-ref cache i) j))
      (or maybe-result
          (let ([result (go i _j)])
            (vector-set! (vector-ref cache i) j result)
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
