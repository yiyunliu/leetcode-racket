#lang racket

(define (longest-palindrome s)
  (if (eqv? (string-length s) 0)
      s
      (let ([longest-that-ends-at (make-vector (string-length s) 1)]
            [count 1])
        (for ([idx (in-range 1 (string-length s))])
          (let ([ch (string-ref s idx)]
                [len (vector-ref longest-that-ends-at (sub1 idx))])
            (let ([idx0 (- idx len 1)])
              (set! count (if (eqv? ch (string-ref s (sub1 idx)))
                              (add1 count)
                              1))
              (vector-set! longest-that-ends-at idx count)
              (when (and (natural? idx0) (eqv? ch (string-ref s idx0)))
                (vector-set! longest-that-ends-at idx (+ len 2))))))
        (let-values
            ([(len idx)
              (for/fold
            ([acc  0]
             [accidx -1])
            ([x  longest-that-ends-at]
             [idx (in-naturals)])
                (if (<= x acc)
                    (values acc accidx)
                    (values x idx)))])
          (substring s (- (add1 idx) len) (add1 idx))))))
