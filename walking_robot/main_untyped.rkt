#lang racket
(require racket/set)

(define (parse-command i)
  (cond
    ([eqv? -2 i] 'TurnLeft)
    ([eqv? -1 i] 'TurnRight)
    (else i)))


(define (turn-left orient)
  (case orient
    ['North 'West]
    ['South 'East]
    ['West 'South]
    ['East 'North]))

(define (turn-right orient)
  (case orient
    ['North 'East]
    ['South 'West]
    ['West 'North]
    ['East 'South]))

(define (turn-orientation turn orient)
  ((case turn
     ['TurnLeft turn-left]
     ['TurnRight turn-right]) orient))

(define (move-one coord orient)
  (let ([x (car coord)]
        [y (cdr coord)])
    (case orient
      ['North (cons x (add1 y))]
      ['East (cons (add1 x) y)]
      ['South (cons x (sub1 y))]
      ['West (cons (sub1 x) y)])))

(define (move-one-maybe obs coord orient)
  (let ([new-coord (move-one coord orient)])
    (if (set-member? obs new-coord)
        #f
        new-coord)))

(define (move-robot obs coord orient n)
  (if (positive? n)
      (let ([new-coord (move-one-maybe obs coord orient)])
        (if new-coord
            (move-robot obs new-coord orient (sub1 n))
            coord))
      coord))

(define-struct State (coord orient))


(define (process-command obs state command)
  (if (integer? command)
      (struct-copy State state
                   [coord (move-robot obs (State-coord state) (State-orient state) command)])
      (struct-copy State state
                   [orient (turn-orientation command (State-orient state))])))

(define (calculate-distance state)
  (let* ([coord (State-coord state)]
         [x (car coord)]
         [y (cdr coord)])
    (+ (* x x) (* y y))))

(define (build-obstacles obses)
  (for/fold
        ([tbl (set)])
        ([obs obses])
    (let ([x (first obs)]
          [y (second obs)])
      (set-add tbl (cons x y)))))

(define (robot-sim commands obstacles)
  (let ([obstacles (build-obstacles obstacles)])
    (let-values ([(max-dist _)
                  (for/fold
                      ([max-dist 0]
                       [state (State (cons 0 0) 'North)])
                      ([command commands])
                    (let* ([command (parse-command command)]
                           [new-state (process-command obstacles state command)])
                      (if (number? command)
                          (values (max max-dist (calculate-distance new-state)) new-state)
                          (values max-dist new-state))))])
      max-dist)))
