#lang racket

(define-struct Matrix (data height width) #:transparent)

(define (matrix-ref m i j)
  (vector-ref (vector-ref (Matrix-data m) i) j))

(define (matrix-set! m i j a)
  (vector-set! (vector-ref (Matrix-data m) i) j a))

(define (build-matrix i j a)
  (Matrix (build-vector i (λ (_) (build-vector j (λ (_) a)))) i j))

(define (in-matrix? m coord)
  (match-define (Matrix _ height width) m)
  (match-define (cons x y) coord)
  (and (<= 0 x) (< x height) (<= 0 y) (< y width)))

(define (get-neighbors m coord)
  (match-define (cons x y) coord)
  (define coords (list (cons (add1 x) y)
                       (cons x (add1 y))
                       (cons x (sub1 y))
                       (cons (sub1 x) y)))
  (filter (lambda (c) (and (in-matrix? m c)
                           (= (matrix-ref m (car c) (cdr c)) 1))) coords))

;; no error checking
;; assumes that all sublists in ls have the same length
(define (2dlist->matrix ls)
  (define data (list->vector (map (lambda (chars) (list->vector (map (lambda (ch) (- (char->integer ch) (char->integer #\0))) chars))) ls)))
  (Matrix data (vector-length data) (if (not (vector-empty? data))
                                        (vector-length (vector-ref data 0))
                                        0)))
(define/contract (num-islands _grid)
  (-> (listof (listof char?)) exact-integer?)

  (define grid (2dlist->matrix _grid))
  (define height (Matrix-height grid))
  (define width (Matrix-width grid))

  ;; data structures for dfs
  (define discovered (build-matrix height width #f))
  (define (discovered? v)
    (matrix-ref discovered (car v) (cdr v)))
  (define (discovered! v)
    (matrix-set! discovered (car v) (cdr v) #t))

  ;; invariant:
  ;; - v has not been discovered
  ;; -grid[v] is 1
  (define (loop v)
    (discovered! v)
    (for ([next-v (get-neighbors grid v)]
          #:unless (or (discovered? next-v)
                       (= (matrix-ref grid (car next-v) (cdr next-v)) 0)))
      (loop next-v)))

  (for/fold
      ([acc 0])
      ([i (in-range height)])
    (for/fold
        ([acc acc])
        ([j (in-range width)]
          #:unless (or (discovered? (cons i j))
                       (= (matrix-ref grid i j) 0)))
      (loop (cons i j))
      (add1 acc))))
