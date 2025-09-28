#lang racket

(module M typed/racket
  (provide rob)
  (define-type (Matrix A) (Vectorof (Vectorof A)))

  (: build-matrix (All (A) (-> Integer Integer A (Matrix A))))
  (define (build-matrix m n a)
    (build-vector m (lambda (_) (make-vector n a))))

  (: matrix-ref (All (A) (-> (Matrix A) Integer Integer A)))
  (define (matrix-ref m row col)
    (vector-ref (vector-ref m row) col))

  (: matrix-set! (All (A) (-> (Matrix A) Integer Integer A Void)))
  (define (matrix-set! m row col a)
    (vector-set! (vector-ref m row) col a))

  (: rob (-> (Listof Integer) Integer))
  (define (rob _nums)
    (define nums (list->vector _nums))
    (define total-len (vector-length nums))

    (: cache (Matrix Integer))
    (define cache (build-matrix (add1 total-len) (add1 total-len) 0))

    (for ([len (in-range 1 (add1 total-len))])
      (for ([start (in-range 0 (add1 (- total-len len)))])
        (define end (+ start len))
        (define max-without-start (matrix-ref cache (add1 start) end))

        (define new-start (+ start 2))
        (define start-elem (vector-ref nums start))
        (define max-with-start
          (if (> new-start end)
              start-elem
              (+ start-elem (matrix-ref cache new-start end))))

        (matrix-set! cache start end (max max-with-start max-without-start))))

    (matrix-ref cache 0 total-len)))
(require 'M)
