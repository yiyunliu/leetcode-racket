#lang racket

(module M typed/racket
  (provide minimum-total)

  (: minimum-total-rec (-> (Vectorof (Vectorof Integer)) Integer Integer Integer))
  (define (minimum-total-rec vec row col)
    (let ([size (vector-length vec)])
      (if (>= row size)
          0
          (let ([curr (vector-ref (vector-ref vec row) col)])
            (+ curr (min (minimum-total-rec vec (add1 row) col) (minimum-total-rec vec (add1 row) (add1 col))))))))

  (: minimum-total (-> (Listof (Listof Integer)) Integer))
  (define (minimum-total tr)
    (: triangle (Vectorof (Vectorof Integer)))
    (define triangle (list->vector (map (ann list->vector (-> (Listof Integer) (Vectorof Integer))) tr)))
    (define size (vector-length triangle))
    (: optimal (Vectorof Integer))
    (define optimal (vector-copy (vector-ref triangle (sub1 size))))
    (for ([row : Integer (in-range (- size 2) -1 -1)])
      (for ([col : Integer (in-range 0 (add1 row))])
        (let ([curr (vector-ref (vector-ref triangle row) col)])
          (vector-set! optimal col (+ curr (min (vector-ref optimal col) (vector-ref optimal (add1 col))))))))
    (vector-ref optimal 0)))

(require 'M)
(provide minimum-total)

(module+ test
  (require rackunit)
  (check-eqv? (minimum-total '((2) (3 4) (6 5 7) (4 1 8 3))) 11)
  (check-eqv? (minimum-total '((2) (3 4))) 5)
  (check-eqv? (minimum-total '((-10))) -10))
