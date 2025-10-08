#lang typed/racket
(require racket/promise)

(: combination-sum (-> (Listof Integer) Integer (Listof (Listof Integer))))
(define (combination-sum _candidates target)
  (: cache (Vectorof (Vectorof (Promise (Listof (Listof Integer))))))
  (define candidates (list->vector _candidates))
  (define len (vector-length candidates))
  (define cache
    (build-vector (add1 len)
                  (lambda ([i : Integer])
                    (build-vector (add1 target)
                                  (lambda ([j : Integer])
                                    (delay (go i j)))))))


  (: cache-wrap (-> Integer Integer (Listof (Listof Integer))))
  (define (cache-wrap start target)
    (force (vector-ref (vector-ref cache start) target)))


  (: go (-> Integer Integer (Listof (Listof Integer))))
  (define (go start target)
    (cond
      ([= target 0] '(()))
      ([>= start len] '())
      (else
       (define start-num (vector-ref candidates start))
       (define candidates-next (cache-wrap (add1 start) target))
       (cond
         [(> start-num target)
           candidates-next]
         [else
          (define candidates-start
            (map (lambda ([x : (Listof Integer)])
                   (cons start-num x))
                 (cache-wrap start (- target start-num))))
          (append candidates-next candidates-start)]))))
  (go 0 target))
