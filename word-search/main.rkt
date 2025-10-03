#lang racket

(module M typed/racket
  (provide exist)
  (define-type Coord (Pair Integer Integer))

  (define-struct State ([discovered : (Setof Coord)] [word : (Listof Char)] (coord : Coord)) #:transparent)

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
    ;; This is stupid
    (define word (reverse (string->list _word)))
    (: board (Vectorof (Vectorof Char)))
    (define board
      (list->vector (map (lambda ([x : (Listof Char)]) (list->vector x)) _board)))
    (define height (vector-length board))
    (define width (vector-length (vector-ref board 0)))

    (: stack (Listof State))
    (define stack
      (ann (for*/list : (Listof State)
               ([i : Integer (in-range height)]
                [j : Integer (in-range width)])
        (ann (coord->state (ann (cons i j) Coord) word) State)) (Listof State)))

    (: loop (-> (Listof State) Boolean))
    (define (loop stack)
      (cond
        [(null? stack)
         #f]
        [else
         (define st (first stack))
         (define new-stack (rest stack))
         (define maybe-sts
           (next-state board st))
         (cond
           [(list? maybe-sts)
            (loop (for/fold
                      ([new-stack : (Listof State) new-stack])
                      ([st : State maybe-sts])
                    (cons st new-stack)))
            ]
           [(symbol? maybe-sts) #t])]))
    (loop stack)))

(require 'M)
