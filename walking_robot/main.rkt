#lang typed/racket
(require racket/set)

(define-type Coord (Pair Integer Integer))

(define-type Obstacles (Setof (Pair Integer Integer)))

(: parse-command (-> Integer Command))
(define (parse-command i)
  (cond
    ([eqv? -2 i] 'TurnLeft)
    ([eqv? -1 i] 'TurnRight)
    (else i)))

(define-type Orientation
  (U 'North 'South 'West 'East))

(define-type Command
  (U 'TurnLeft 'TurnRight Integer))

(: turn-left (-> Orientation Orientation))
(define (turn-left orient)
  (case orient
    ['North 'West]
    ['South 'East]
    ['West 'South]
    ['East 'North]))

(: turn-right (-> Orientation Orientation))
(define (turn-right orient)
  (case orient
    ['North 'East]
    ['South 'West]
    ['West 'North]
    ['East 'South]))

(: turn-orientation (-> (U 'TurnLeft 'TurnRight) Orientation Orientation))
(define (turn-orientation turn orient)
  ((case turn
     ['TurnLeft turn-left]
     ['TurnRight turn-right]) orient))

(: move-one (-> Coord Orientation Coord))
(define (move-one coord orient)
  (let ([x (car coord)]
        [y (cdr coord)])
    (case orient
      ['North (cons x (add1 y))]
      ['East (cons (add1 x) y)]
      ['South (cons x (sub1 y))]
      ['West (cons (sub1 x) y)])))

(: move-one-maybe (-> Obstacles Coord Orientation (U False Coord)))
(define (move-one-maybe obs coord orient)
  (let ([new-coord (move-one coord orient)])
    (if (set-member? obs new-coord)
        #f
        new-coord)))

(: move-robot (-> Obstacles Coord Orientation Integer Coord))
(define (move-robot obs coord orient n)
  (if (positive? n)
      (let ([new-coord (move-one-maybe obs coord orient)])
        (if new-coord
            (move-robot obs new-coord orient (sub1 n))
            coord))
      coord))

(define-struct State ([coord : Coord] [orient : Orientation]))


(: process-command (-> Obstacles State Command State))
(define (process-command obs state command)
  (if (integer? command)
      (struct-copy State state
                   [coord (move-robot obs (State-coord state) (State-orient state) command)])
      (struct-copy State state
                   [orient (turn-orientation command (State-orient state))])))

(: calculate-distance (-> State Integer))
(define (calculate-distance state)
  (let* ([coord (State-coord state)]
         [x (car coord)]
         [y (cdr coord)])
    (+ (* x x) (* y y))))

(: build-obstacles (-> (Listof (List Integer Integer)) Obstacles))
(define (build-obstacles obses)
  (for/fold
        ([tbl : Obstacles (set)])
        ([obs : (List Integer Integer) obses])
    (let ([x (first obs)]
          [y (second obs)])
      (set-add tbl (cons x y)))))

(: robot-sim (-> (Listof Integer) (Listof (List Integer Integer)) Natural))
(define (robot-sim commands obstacles)
  (let ([obstacles (build-obstacles obstacles)])
    (let-values ([(max-dist _)
                  (for/fold
                      ([max-dist : Natural 0]
                       [state : State (State (cons 0 0) 'North)])
                      ([command : Integer commands])
                    (let* ([command (parse-command command)]
                           [new-state (process-command obstacles state command)])
                      (if (number? command)
                          (values (max max-dist (calculate-distance new-state)) new-state)
                          (values max-dist new-state))))])
      max-dist)))
