#lang racket

(define (longest-valid-parentheses s)
  (calculate-longest-rec 0 0 0 (longest-valid-rec s)))

(define (left-paren? ch)
  (eqv? #\( ch))

(define (longest-valid-rec str)
  (let ([max-idxs (make-vector (string-length str) #f)])
    (for/fold
          ([stack '()])
          ([idx (in-naturals)]
           [ch str])
      (cond
        [(left-paren? ch)
         (cons idx stack)]
        [(null? stack) stack]
        [else
         (vector-set! max-idxs (car stack) (add1 idx))
         (cdr stack)]))
    max-idxs))

(define (calculate-longest-rec gmax curr-acc n idxs)
  (if (< n (vector-length idxs))
      (let ([maybe-next (vector-ref idxs n)])
        (if maybe-next
            (calculate-longest-rec gmax (+ curr-acc (- maybe-next n)) maybe-next idxs)
            (calculate-longest-rec (max gmax curr-acc) 0 (add1 n) idxs)))
      (max curr-acc gmax)))
