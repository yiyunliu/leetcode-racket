#lang racket

;; type State = 'right | 'down | 'left | 'up
;; next-state : State -> State
(define (next-state st)
  (case st
    ['right 'down]
    ['down 'left]
    ['left 'up]
    ['up 'right]))

(define (move-coord st coord)
  (define x (car coord))
  (define y (cdr coord))
  (case st
    ['right (cons x (add1 y))]
    ['down (cons (add1 x) y)]
    ['left (cons x (sub1 y))]
    ['up (cons (sub1 x) y)]))

(define (matrix-ref matrix coord)
  (vector-ref (vector-ref matrix (car coord)) (cdr coord)))

(define (matrix-set! matrix coord a)
  (vector-set! (vector-ref matrix (car coord)) (cdr coord) a))

(define (explored? bmatrix coord)
  (matrix-ref bmatrix coord))

(define (explored! bmatrix coord)
  (matrix-set! bmatrix coord #t))

(define (bad-coord? bmatrix coord)
  (define height (vector-length bmatrix))
  (define width (vector-length (vector-ref bmatrix 0)))
  (define x (car coord))
  (define y (cdr coord))
  (or (negative? x) (negative? y) (>= x height) (>= y width) (explored? bmatrix coord)))

(define (next-coord bmatrix st coord)
  (define coord0 (move-coord st coord))
  (if (bad-coord? bmatrix coord0)
      (next-coord bmatrix (next-state st) coord)
      (values st coord0)))

(define/contract (spiral-order _matrix)
  (-> (listof (listof exact-integer?)) (listof exact-integer?))
  (define matrix (list->vector (map list->vector _matrix)))
  (define height (vector-length matrix))
  (define width (vector-length (vector-ref matrix 0)))
  (define bmatrix (build-vector height (lambda (_) (make-vector width #f))))

  (define-values
    (coord _ result)
    (for/fold
      ([coord (cons 0 0)]
       [st 'right]
       [result '()])
      ([_ (in-range (sub1 (* width height)))])
      (explored! bmatrix coord)
      (define-values (new-st new-coord) (next-coord bmatrix st coord))
      (define new-result (cons (matrix-ref matrix coord) result))
      (values new-coord new-st new-result)))
  (reverse (cons (matrix-ref matrix coord) result)))
