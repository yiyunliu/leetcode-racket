#lang racket

(module M typed/racket
  (define-type Coord (Pair Integer Integer))

  (define-struct State ([discovered : (Setof Coord)] [word : (Listof Char)] (coord : Coord)) #:transparent)

  (require/typed data/queue
    [#:opaque StateQueue queue?]
    [make-queue (-> StateQueue)]
    [enqueue! (-> StateQueue State Void)]
    [dequeue! (-> StateQueue State)]
    [queue-empty? (-> StateQueue Boolean)])

  (: matrix-ref (-> (Vectorof (Vectorof Char)) Coord Char))
  (define (matrix-ref matrix coord)
    (match-define (cons x y) coord)
    (vector-ref (vector-ref matrix x) y))

  (define-type (Matrix A) (Vectorof (Vectorof A)))


  (: coord-valid? (-> (Matrix Char) Coord Boolean))
  (define (coord-valid? m coord)
    (define height (vector-length m))
    (define width (vector-length (vector-ref m 0)))
    (match-define (cons x y) coord)
    (and (natural? x) (natural? y) (< x height) (< y width)))

  (: coord->state (-> Coord (Listof Char) State))
  (define (coord->state coord word)
    (State (set coord) word coord))

  (: next-state (-> (Matrix Char) State (U (Listof State) 'done)))
  (define (next-state matrix state)
    (match-define (State discovered word coord) state)
    (cond
      [(null? word) 'done]
      [else
       (match-define (cons char new-word) word)
       (cond
         ([not (eq? char (matrix-ref matrix coord))] '())
         ([null? (cdr word)] 'done)
         (else
          (match-define (cons x y) coord)
          (let* ([coords (list
                          (cons (add1 x) y)
                          (cons x (add1 y))
                          (cons (sub1 x) y)
                          (cons x (sub1 y)))]
                 [coords : (Listof Coord) (filter (lambda ([x : Coord])
                                                    (and (coord-valid? matrix x)
                                                         (not (set-member? discovered x)))) coords)])
            (ann (for/list ([coord coords])
                   (State (set-add discovered coord) new-word coord)) (Listof State)))))]))


  (: exist (-> (Listof (Listof Char)) String Boolean))
  (define (exist _board _word)
    (: word (Listof Char))
    (define word (string->list _word))
    (: board (Vectorof (Vectorof Char)))
    (define board
      (list->vector (map (lambda ([x : (Listof Char)]) (list->vector x)) _board)))
    (define height (vector-length board))
    (define width (vector-length (vector-ref board 0)))
    (: queue StateQueue)
    (define queue (make-queue))

    (for* ([i (in-range height)]
           [j (in-range width)])
      (enqueue! queue (coord->state (cons i j) word)))

    (: loop (-> Boolean))
    (define (loop)
      (cond
        [(queue-empty? queue)
         #f]
        [else
         (define st (dequeue! queue))
         (define maybe-sts
           (next-state board st))
         (cond
           [(list? maybe-sts)
            (for ([st : State maybe-sts])
              (enqueue! queue st))
            (loop)]
           [(symbol? maybe-sts) #t])]))
    (loop)))

(require 'M)
