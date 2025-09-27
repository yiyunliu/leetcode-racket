#lang racket

(module M typed/racket
  (provide search)

  (: search-rec (-> (Vectorof Integer) Integer Integer Integer (U Integer False)))
  (define (search-rec vec start end target)
    (if (>= start end)
        #f
        (let* ([idx (+ start (quotient (- end start) 2))]
               [num (vector-ref vec idx)])
          (cond
            [(> num target) (search-rec vec start idx target)]
            [(< num target) (search-rec vec (add1 idx) end target)]
            [else idx]))))

  (: search (-> (Listof Integer) Integer Integer))
  (define (search nums target)
    (let ([nums (list->vector nums)])
      (or (search-rec nums 0 (vector-length nums) target) -1))))

(require 'M)

(module+ test
  (require rackunit)
  (let ([xs '(-1 0 3 5 9 12)])
    (check-equal? (search xs 9) 4)
    (check-equal? (search xs 2) -1)))
