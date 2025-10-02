#lang typed/racket

(: search (-> (Pair Integer (Listof Integer)) Integer Integer))
(define (search _nums target)
  (define num-first (first _nums))
  (define nums (list->vector _nums))
  (: get (-> Integer Integer))
  (define (get i)
    (vector-ref nums i))
  (: first-frag? (-> Integer Boolean))
  (define (first-frag? n)
    (>= n num-first))
  (define target-first-frag?
    (first-frag? target))
  (: loop (-> Integer Integer (U False Integer)))
  (define (loop start end)
    (cond
      [(<= (- end start) 0) #f]
      [else
       (define idx (+ start (quotient (- end start) 2)))
       (define num (get idx))
       (define num-first-frag? (first-frag? num))
       (cond
         [(eq? num-first-frag? target-first-frag?)
          (cond
            [(< target num)
             (loop start idx)]
            [(> target num)
             (loop (add1 idx) end)]
            [else
             idx])]
         [target-first-frag?
          (loop start idx)]
         [else
          (loop (add1 idx) end)])]))
  (or (loop 0 (vector-length nums)) -1))
