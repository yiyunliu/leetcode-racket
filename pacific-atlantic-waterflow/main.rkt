#lang typed/racket

(module M typed/racket
  (provide pacific-atlantic)
  (define-type Matrix (Vectorof (Vectorof Integer)))

  (define-type Coord (Pair Integer Integer))

  (: get-height (-> Matrix Coord Integer))
  (define (get-height matrix coord)
    (vector-ref (vector-ref matrix (car coord)) (cdr coord)))

  ;; assumes that oob coords are sea
  (: sea-coord? (-> Matrix Coord Boolean))
  (define (sea-coord? matrix coord)
    (define row (car coord))
    (define col (cdr coord))
    (define height (vector-length matrix))
    (define width (vector-length (vector-ref matrix 0)))
    (or (negative? row) (negative? col) (>= row height) (>= col width)))


  (: get-flowable (-> Matrix Coord (Listof Coord)))
  (define (get-flowable matrix coord)
    (let ([row (car coord)]
          [col (cdr coord)])
      (filter (lambda ([x : Coord])
                (and (not (sea-coord? matrix x)) (<= (get-height matrix coord) (get-height matrix x))))
              `((,(add1 row) . ,col) (,row . ,(add1 col)) (,(sub1 row) . ,col) (,row . ,(sub1 col))))))


  (: mark-flowable (-> (Listof Coord)
                       Matrix
                       (Vectorof (Vectorof Boolean))
                       (Vectorof (Vectorof Integer))
                       Void))
  (define (mark-flowable sea-vertices matrix discovered flow-table)
    (: discovered? (-> Coord Boolean))
    (define (discovered? [vertex : Coord])
      (vector-ref (vector-ref discovered (car vertex)) (cdr vertex)))

    (define (discovered! [vertex : Coord])
      (vector-set! (vector-ref discovered (car vertex)) (cdr vertex) #t))

    (define (incr! [vertex : Coord])
      (vector-set! (vector-ref flow-table (car vertex)) (cdr vertex)
                   (add1 (vector-ref (vector-ref flow-table (car vertex)) (cdr vertex)))))

    (for ([vertex sea-vertices]
          #:unless (discovered? vertex))
      (: dfs (-> Coord Void))
      (define (dfs src)
        (incr! src)
        (discovered! src)
        ;; (print discovered)
        ;; (print (get-flowable matrix src))
        (for ([tgt (get-flowable matrix src)]
              #:unless (discovered? tgt))
          (dfs tgt)))
      (dfs vertex)))


  (: list->matrix (-> (Listof (Listof Integer)) (Vectorof (Vectorof Integer))))
  (define (list->matrix xs)
    (list->vector (map (lambda ([x : (Listof Integer)]) (list->vector x)) xs)))

  (: matrix->list (-> (Vectorof (Vectorof Integer)) (Listof (Listof Integer))))
  (define (matrix->list xs)
    (vector->list (vector-map (lambda ([x : (Vectorof Integer)]) (vector->list x)) xs)))

  (: pacific-atlantic (-> (Listof (Listof Integer)) (Listof (Listof Integer))))
  (define (pacific-atlantic _heights)
    (define heights (list->matrix _heights))
    (define height (vector-length heights))
    (define width (vector-length (vector-ref heights 0)))
    (: discovered (Vectorof (Vectorof Boolean)))
    (define discovered
      (build-vector height (lambda (_) (ann (make-vector width #f) (Vectorof Boolean)))))
    (: flow-table (Vectorof (Vectorof Integer)))
    (define flow-table
      (build-vector height (lambda (_) (make-vector width 0))))
    (: pacific-vertices (Listof Coord))
    (define pacific-vertices
      (append
       (ann (for/list ([x : Integer (in-range height)])
              (cons x 0)) (Listof Coord))
       (ann (for/list ([x : Integer (in-range 1 width)])
              (cons 0 x)) (Listof Coord))))
    (: atlantic-vertices (Listof Coord))
    (define atlantic-vertices
      (append
       (ann (for/list ([x : Integer (in-range height)])
              (cons x (- width 1))) (Listof Coord))
       (ann (for/list ([x : Integer (in-range 0 (sub1 width))])
              (cons (- height 1) x)) (Listof Coord))))
    ;; sea-vertices matrix discovered flow-table
    (mark-flowable pacific-vertices heights discovered flow-table)
    (for ([v discovered])
      (vector-fill! v #f))
    ;; (print atlantic-vertices)
    (mark-flowable atlantic-vertices heights discovered flow-table)
    ;; (print discovered)
    (for/fold ([acc : (Listof (Listof Integer)) '()])
              ([row : Integer (in-range height)])
      (for/fold ([acc : (Listof (Listof Integer)) acc])
                ([col : Integer (in-range width)])
                (if (eqv? 2 (vector-ref (vector-ref flow-table row) col))
                    (cons (list row col) acc)
                    acc)))))

(require 'M)
