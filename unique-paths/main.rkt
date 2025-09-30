#lang racket

;; solution[i][j] = solution[i-1][j] + solution[i][j-1]

(define/contract (unique-paths m n)
  (-> exact-integer? exact-integer? exact-integer?)
  (cond
    [(or (<= m 1) (<= n 1)) 1]
    [else
     (let-values ([(n m) (if (< n m) (values n m) (values m n))])
       (define cache (make-vector n 1))
       (for ([_ (in-range (sub1 m))])
         (for ([i (in-range 1 (vector-length cache))])
           (vector-set! cache i (+ (vector-ref cache (sub1 i)) (vector-ref cache i)))))
       (vector-ref cache (sub1 (vector-length cache))))]))
