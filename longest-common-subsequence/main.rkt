#lang typed/racket
(require math/array)

(: longest-common-subsequence (-> String String Integer))
(define (longest-common-subsequence text0 text1)
  (define len0 (string-length text0))
  (define len1 (string-length text1))

  (: cache (Mutable-Array Integer))
  (define cache (array->mutable-array
                 (build-array
                  (vector (add1 len0) (add1 len1))
                  (lambda (_) 0))))
  (for* ([i (in-range 1 (add1 len0))]
         [j (in-range 1 (add1 len1))])
    (define char-i (string-ref text0 (sub1 i)))
    (define char-j (string-ref text1 (sub1 j)))
    (let ([result
           (cond
             [(eqv? char-i char-j)
              (add1 (array-ref cache (vector (sub1 i) (sub1 j))))]
             [else
              (max
               (array-ref cache (vector (sub1 i) j))
               (array-ref cache (vector i (sub1 j))))])])
      (array-set! cache (vector i j) result)))
  (array-ref cache (vector len0 len1)))
