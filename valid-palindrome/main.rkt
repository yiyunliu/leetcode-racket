#lang racket

(define (alphanumeric? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)))

(define/contract (is-palindrome s)
  (-> string? boolean?)
  (define (get i) (string-ref s i))
  (let ([len (string-length s)])
    (define-values (_i _j acc)
      (for/fold
        ([i 0]
         [j (sub1 len)]
         [acc #t])
        ([_ (in-cycle '(#f))])
        #:break (or (false? acc) (>= i j))
        (cond
          [(not (alphanumeric? (get i))) (values (add1 i) j acc)]
          [(not (alphanumeric? (get j))) (values i (sub1 j) acc)]
          [else (values (add1 i) (sub1 j) (eqv? (char-foldcase (get i)) (char-foldcase (get j))))])))
    acc))
