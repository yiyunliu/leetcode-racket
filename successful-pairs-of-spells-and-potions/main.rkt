#lang typed/racket

(: successful-pairs (-> (Listof Integer) (Listof Integer)  Integer (Listof Integer) ))
(define (successful-pairs spells _potions target)
  (define potions (list->vector _potions))
  (define len (vector-length potions))
  (vector-sort! potions <)
  (: search (-> Integer Integer Integer (U Integer #f)))
  (define (search start end spell)
    (cond
      [(>= start end) #f]
      [else
       (define idx (+ start (quotient (- end start) 2)))
       (define elem (vector-ref potions idx))
       (cond
         [(< (* elem spell) target)
          (search (add1 idx) end spell)]
         [else (or (search start idx spell) idx)])]))

  (for/list
      ([spell spells])
    (define result (search 0 len spell))
    (if result
        (- len result)
        0)))
