#lang racket


(define/contract (insert intervals new-interval)
  (-> (listof (listof exact-integer?)) (listof exact-integer?) (listof (listof exact-integer?)))

  (define (find-start intervals new-start)
    (for/fold ([before-intervals '()]
               [start-acc #f]
               [after-intervals intervals])
              ([interval intervals])
      #:break start-acc
      (match-let ([(list start end) interval])
        (cond
          [(and (<= start new-start) (<= new-start end))
           (values before-intervals start after-intervals)]
          [(< new-start start)
           (values before-intervals new-start after-intervals)]
          [else
           (values (cons interval before-intervals)  start-acc (cdr after-intervals))]))))


  (define (find-end intervals new-end)
    (for/fold ([end-acc #f]
               [after-intervals intervals])
              ([interval intervals])
      #:break end-acc
      (match-let ([(list start end) interval])
        (cond
          [(and (<= start new-end) (<= new-end end))
           (values end (cdr after-intervals))]
          [(< new-end start)
           (values new-end after-intervals)]
          [else
           (values end-acc (cdr after-intervals))]))))

  (define-values (before-intervals start-acc after-intervals)
    (find-start intervals (car new-interval)))
  (cond
    [start-acc
     (let-values ([(end-acc after-intervals) (find-end after-intervals (second new-interval))])
       (if end-acc
           (append (reverse before-intervals) (list (list start-acc end-acc)) after-intervals)
           (reverse (cons (list start-acc (second new-interval)) before-intervals))))]
    [else
     (reverse (cons new-interval before-intervals))]))
