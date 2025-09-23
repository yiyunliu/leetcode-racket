#lang racket

(define (update-one stat)
  (case stat
    ['Neither 'First]
    [else 'Bad]))

(define (update-two stat)
  (case stat
    ['First 'Both]
    [else 'Bad]))

(define (count-words l0 l1)
  (let ([table (make-hash)])
    (for ([s l0])
      (hash-ref! table s (lambda () 'Neither))
      (hash-update! table s update-one))
    (for ([s l1])
      (hash-ref! table s (lambda () 'Neither))
      (hash-update! table s update-two))
    (for/fold
        ([acc 0])
        ([(_ v) table])
      (if (eq? v 'Both)
          (add1 acc)
          acc))))
