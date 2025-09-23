#lang typed/racket

(: longest-valid-parentheses (-> String Integer))
(define (longest-valid-parentheses s)
  (calculate-longest-rec 0 0 0 (longest-valid-rec s)))

(define-type Stack (Listof Natural))

(: left-paren? (-> Char Boolean))
(define (left-paren? ch)
  (eqv? #\( ch))

(: right-paren? (-> Char Boolean))
(define (right-paren? ch)
  (eqv? #\) ch))

(: longest-valid-rec (-> String (Vectorof (U False Integer))))
(define (longest-valid-rec str)
  (let ([max-idxs : (Vectorof (U False Integer)) (make-vector (string-length str) #f)])
    (for/fold
          ([stack : Stack '()])
          ([idx : Natural (in-naturals)]
           [ch : Char str])
      (cond
        [(left-paren? ch)
         (cons idx stack)]
        [(null? stack) stack]
        [else
         (vector-set! max-idxs (car stack) (add1 idx))
         (cdr stack)]))
    max-idxs))

(: calculate-longest-rec (-> Integer Integer Integer (Vectorof (U False Integer)) Integer))
(define (calculate-longest-rec gmax curr-acc n idxs)
  (if (< n (vector-length idxs))
      (let ([maybe-next (vector-ref idxs n)])
        (if maybe-next
            (calculate-longest-rec gmax (+ curr-acc (- maybe-next n)) maybe-next idxs)
            (calculate-longest-rec (max gmax curr-acc) 0 (add1 n) idxs)))
      (max curr-acc gmax)))
