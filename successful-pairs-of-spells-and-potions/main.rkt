#lang typed/racket

(: successful-pairs (-> (Listof Integer) (Listof Integer)  Integer (Listof Integer) ))
(define (successful-pairs _spells _potions target)
  (: spells (Vectorof (Pair Integer Integer)))
  (define spells (for/vector : (Vectorof (Pair Integer Integer))
                     ([(spell idx) (in-indexed _spells)])
                   (cons spell idx)))
  (define potions (list->vector _potions))
  (define len (vector-length potions))
  (vector-sort! potions <)
  ((inst vector-sort! (Pair Integer Integer) Integer) spells < #:key car)
  (: search (-> Integer Integer Integer (U Integer #f)))
  (define (search start end spell)
    (cond
      [(>= start end) #f]
      [else
       (define idx (+ start (quotient (- end start) 2)))
       (define elem (vector-ref potions idx))
       (cond
         [(< (* elem spell) target)
          (search (add1 start) end spell)]
         [else (or (search start idx spell) idx)])]))
  (define-values
    (_ acc) (for/fold
      ([prev-idx : Integer (sub1 len)]
       [acc : (Listof (Pair Integer Integer)) '()])
      ([spell-idx spells])
    (define idx (cdr spell-idx))
    (define spell (car spell-idx))
    (define result (search 0 (add1 prev-idx) spell))
    (define new-acc
      (cons (cons idx (if result (- len result) 0)) acc))
    (values (or result (sub1 len)) new-acc)))
  (: sorted-result (Listof (Pair Integer Integer)))
  (define sorted-result
    ((inst sort (Pair Integer Integer) Integer) acc < #:key car))
  (map (ann cdr (-> (Pair Integer Integer) Integer)) sorted-result))
