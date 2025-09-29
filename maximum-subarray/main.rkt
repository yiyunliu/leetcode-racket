#lang racket
(module M typed/racket
  (provide max-sub-array)
  (: max-sub-array (-> (Listof Integer) Integer))
  (define (max-sub-array nums)
    (if (and (not (null? nums)) (null? (cdr nums)))
        (car nums)
        (let-values
            ([(_ max-sum)
              (for/fold ([max-ends-on-prev : Integer (car nums)]
                         [max-sum : Integer (car nums)])
                        ([n (cdr nums)])
                (define prev-curr (+ max-ends-on-prev n))
                (cond
                  [(< prev-curr n)
                   (values n (max max-sum n))]
                  [else
                   (values prev-curr (max max-sum prev-curr))]))])
          max-sum))))
(require 'M)
