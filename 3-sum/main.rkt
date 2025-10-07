#lang typed/racket


(: two-sum (-> (Vectorof Integer) Integer Integer Integer Integer
               (Setof (List Integer Integer Integer))
               (Setof (List Integer Integer Integer))))
(define (two-sum nums start end target num-before acc)
  (define (get [i : Integer]) (vector-ref nums i))
  (cond
    [(>= start end) acc]
    [else
     (define start-num (get start))
     (define end-num (get end))
     (define sum (+ start-num end-num))
     (cond
       [(= sum target) (two-sum nums (add1 start) end target num-before
                                (set-add acc (list num-before start-num end-num)))]
       [(< sum target) (two-sum nums (add1 start) end target num-before acc)]
       [else (two-sum nums start (sub1 end) target num-before acc)])]))

(: three-sum (-> (Listof Integer) (Listof (List Integer Integer Integer))))
(define (three-sum nums)
  (let* ([nums
          (let ([result (list->vector nums)])
            (vector-sort! result <)
            result)]
         [len (vector-length nums)])
    (set->list
     (for/fold
         ([acc : (Setof (List Integer Integer Integer)) (set)])
         ([(num idx)  (in-indexed nums)])
       (define new-target (- num))
       (two-sum nums (add1 idx) (sub1 len) new-target num acc)))))


(module+ test
  (require typed/rackunit)
  (check-equal? (three-sum '(-1 0 1 2 -1 -4)) '((-1 -1 2) (-1 0 1)) )
  (check-equal? (three-sum '(0 0 0)) '((0 0 0)))
  (check-equal? (three-sum '(0 1 1)) '())
  (check-equal? (three-sum '(0 0)) '()))
