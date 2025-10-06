#lang typed/racket

(define-syntax-rule (swap! x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(: longest-common-subsequence (-> String String Integer))
(define (longest-common-subsequence text0 text1)
  (define len0 (string-length text0))
  (define len1 (string-length text1))
  (when (> len0 len1)
    (swap! len0 len1)
    (swap! text0 text1))

  (: cache (Vector Integer))
  (define cache (make-vector len0 0))
  (: cache-tmp)
  (define cache-tmp (make-vector len0 0))
  (for* ([i (in-range len1)])
    (for/fold
        ([prev : Integer 0])
        ([j (in-range len0)])
      (define char-i (string-ref text1 i))
      (define char-j (string-ref text0 j))
      (define new-prev
        (cond
          [(eqv? char-i char-j)
           (add1 (vector-ref cache j) prev)]
          [else
           ]))

)
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
