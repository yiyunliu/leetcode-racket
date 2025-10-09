#lang typed/racket

(require racket/unsafe/ops)

(: successful-pairs (-> (Listof Fixnum) (Listof Fixnum)  Fixnum (Listof Fixnum) ))
(define (successful-pairs spells _potions target)
  (define potions (list->vector _potions))
  (define len (vector-length potions))
  (vector-sort! potions <)
  (: search (-> Fixnum Fixnum Fixnum (U Fixnum #f)))
  (define (search start end spell)
    (cond
      [(unsafe-fx>= start end) #f]
      [else
       (define idx (unsafe-fx+ start (unsafe-fxquotient (unsafe-fx- end start) 2)))
       (define elem (vector-ref potions idx))
       (cond
         [(unsafe-fx< (unsafe-fx* elem spell) target)
          (search (unsafe-fx+ 1 idx) end spell)]
         [else (or (search start idx spell) idx)])]))

  (for/list
      ([spell : Fixnum spells])
    (define result (search 0 len spell))
    (if result
        (unsafe-fx- len result)
        0)))
