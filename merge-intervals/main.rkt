#lang racket

(define/contract (merge intervals)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)))

  (if (null? intervals)
      intervals
      (let-values
          ([(start end acc-intervals)
            (let ([intervals (sort intervals < #:key car)])
              (for/fold
                  ([curr-start (first (car intervals))]
                   [curr-end (second (car intervals))]
                   [acc-intervals '() ])
                  ([interval (cdr intervals)])
                (match-define (list start end) interval)
                (cond
                  [(<= start curr-end)
                   (values curr-start (max curr-end end) acc-intervals)]
                  [else
                   (values start end (cons (list curr-start curr-end) acc-intervals))])))])
        (reverse (cons (list start end) acc-intervals)))))
