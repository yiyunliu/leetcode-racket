#lang typed/racket

(: longest-palindrome (-> String String))
(define (longest-palindrome s)
  (if (eqv? (string-length s) 0)
      s
      (let ([longest-that-ends-at : (Vectorof Integer) (make-vector (string-length s) 1)])
        (for ([idx : Natural (in-range 1 (string-length s))])
          (let ([ch (string-ref s idx)]
                [len (vector-ref longest-that-ends-at (sub1 idx))])
            (let ([idx0 (- idx len 1)])
              (when (eqv? ch (string-ref s (sub1 idx)))
                (vector-set! longest-that-ends-at idx 2))
              (when (and (natural? idx0) (eqv? ch (string-ref s idx0)))
                (vector-set! longest-that-ends-at idx (+ len 2))))))
        (let-values
            ([(len idx)
              (for/fold
            ([acc : Integer 0]
             [accidx : Integer -1])
            ([x : Integer longest-that-ends-at]
             [idx : Natural (in-naturals)])
                (if (<= x acc)
                    (values acc accidx)
                    (values x idx)))])
          (substring s (- (add1 idx) len) (add1 idx))))))
