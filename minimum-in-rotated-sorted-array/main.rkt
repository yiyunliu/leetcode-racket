#lang racket

(module M typed/racket
  (provide find-min)

  (: max-num Integer)
  (define max-num 5000)

  (: find-min (-> (Listof Integer) Integer))
  (define (find-min _nums)
    (define nums (list->vector _nums))
    (: get (-> Integer Integer))
    (define (get i) (vector-ref nums i))
    (cond
      [(<= (vector-length nums) 3)
       (for/fold ([acc max-num]) ([num nums]) (min num acc))]
      [else
       (define num-first (get 0))
       (define num-last (get (sub1 (vector-length nums))))
       (cond
         [(<= num-first num-last)  num-first]
         [else
          (: loop (-> Integer Integer Integer))
          (define (loop start end)
            (cond
              [(<= (- end start) 3) (for/fold ([acc max-num]) ([i (in-range start end)]) (min acc (get i)))]
              [else
               (define idx (+ start (quotient (- end start) 2)))
               (define num (get idx))
               (define num-left (get (sub1 idx)))
               (define num-right (get (add1 idx)))
               (cond
                 [(and (<= num num-left) (<= num num-right)) num]
                 [(< num-first num) (loop (add1 idx) end)]
                 [else (loop start idx)])]))
          (loop 0 (vector-length nums))])])))

(require 'M)
