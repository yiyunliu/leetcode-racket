#lang racket
(define/contract (num-decodings s)
  (-> string? exact-integer?)
  (define str-len (string-length s))
  (define max-len-1 (build-vector str-len (lambda (idx) (if (eq? (string-ref s idx) #\0) 0 1))))
  (define max-len-2 (make-vector (add1 str-len) 1))
  (for ([code-len (in-inclusive-range 2 str-len)])
    (for ([(ch idx) (in-indexed s)])
      #:break (< str-len (+ idx code-len))
      (define one-char-ways
        (case ch
          [(#\0) 0]
          [else
           (vector-ref max-len-1 (add1 idx))]))
      (define two-char-ways
        (* (vector-ref max-len-2 (+ 2 idx))
           (case ch
             [(#\1) 1]
             [(#\2) (if (<= (char->integer (string-ref s (add1 idx))) (char->integer #\6) )
                        1
                        0)]
             [else 0])))
      (vector-set! max-len-2 idx (+ one-char-ways two-char-ways)))
    (define tmp max-len-1)
    (set! max-len-1 max-len-2)
    (set! max-len-2 tmp))
  (vector-ref max-len-1 0))
