#lang racket

(define/contract (longest-consecutive nums)
  (-> (listof exact-integer?) exact-integer?)
  (define set-nums (apply set nums))
  (define-values
    (_ cnt)
    (for/fold
      ([visited (set)]
       [max-seq 0])
      ([n nums]
       #:unless (set-member? visited n))
      (define (dfs visited v cnt)
        (for/fold
            ([cnt cnt]
             [visited (set-add visited v)])
            ([next (list (add1 v) (sub1 v))]
             #:when (set-member? set-nums next)
             #:unless (set-member? visited next))
          (dfs visited next (add1 cnt))))
      (let-values ([(cnt visited) (dfs visited n 1)])
        (values visited (max max-seq cnt)))))
  cnt)
