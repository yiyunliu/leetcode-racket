#lang racket

(module M typed/racket
  (provide minimum-total)
  (define-struct (A) Slice ([vec : (Vectorof A)] [start : Integer] [end : Integer]))

  (: build-slice (All (A) (-> (Vectorof A) Integer Integer (Slice A))))
  (define (build-slice vec start end)
    (Slice vec start end))

  (: subslice (All (A) (-> (Slice A) Integer Integer (Slice A))))
  (define (subslice s start end)
    (Slice (Slice-vec s) (+ (Slice-start s) start) (+ (Slice-start s) end)))

  (: vector->slice (All (A) (-> (Vectorof A) (Slice A))))
  (define (vector->slice vec)
    (build-slice vec 0 (vector-length vec)))

  (: slice-ref (All (A) (-> (Slice A) Integer A)))
  (define (slice-ref s i)
    (vector-ref (Slice-vec s) (+ (Slice-start s) i)))

  (: slice-set! (All (A) (-> (Slice A) Integer A Void)))
  (define (slice-set! s i a)
    (vector-set! (Slice-vec s) (+ (Slice-start s) i) a))

  (: slice-size (All (A) (-> (Slice A) Integer)))
  (define (slice-size s)
    (- (Slice-end s) (Slice-start s)))

  (: slice-empty? (All (A) (-> (Slice A) Boolean)))
  (define (slice-empty? s)
    (>= (Slice-start s) (Slice-end s)))

  (: minimum-total-rec (-> (Vectorof (Vectorof Integer)) Integer Integer Integer))
  (define (minimum-total-rec vec row col)
    (let ([size (vector-length vec)])
      (if (>= row size)
          0
          (let ([curr (vector-ref (vector-ref vec row) col)])
            (+ curr (min (minimum-total-rec vec (add1 row) col) (minimum-total-rec vec (add1 row) (add1 col))))))))

  (: minimum-total (-> (Listof (Listof Integer)) Integer))
  (define (minimum-total triangle)
    (let ([triangle (list->vector (map (ann list->vector (-> (Listof Integer) (Vectorof Integer))) triangle))])
      (minimum-total-rec triangle 0 0))))

(require 'M)
(provide minimum-total)

(module+ test
  (require rackunit)
  (check-eqv? (minimum-total '((2) (3 4) (6 5 7) (4 1 8 3))) 11)
  (check-eqv? (minimum-total '((-10))) -10))
