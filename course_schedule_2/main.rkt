#lang racket

(module M typed/racket
  (provide find-order)

  (define-type Vertex Integer)
  (define-type Graph (Vectorof (Listof Integer)))

  (: initialize-empty-graph (Integer -> Graph))
  (define (initialize-empty-graph n)
    (make-vector n '()))

  (: insert-edge! (Graph Integer Integer -> Void))
  (define (insert-edge! graph src tgt)
    (vector-set! graph src (cons tgt (vector-ref graph src))))

  ;; invariant: vertex is not discovered
  ;; processed is a subset of discovered (processed[i] ==> discovered[i])
  (: top-sort-rec (Graph Integer (Vectorof Boolean) (Vectorof Boolean) (Listof Integer) ->
                         (U False (Listof Integer))))
  (define (top-sort-rec graph src discovered processed acc)
    (vector-set! discovered src #t)
    (let ([acc
           (for/fold
             ([acc : (U False (Listof Integer)) acc])
             ([tgt (vector-ref graph src)])
             #:break (false? acc)
             (cond
               [(vector-ref processed tgt)
                (values acc)]
               [(vector-ref discovered tgt)
                (values #f)]
               [else
                (top-sort-rec graph tgt discovered processed acc)]))])
      (vector-set! processed src #t)
      (and acc (cons src acc))))

  (: top-sort (Graph -> (U False (Listof Integer))))
  (define (top-sort graph)
    (: size Index)
    (define size (vector-length graph))
    (let ([processed : (Vectorof Boolean) (make-vector size #f)]
          [discovered : (Vectorof Boolean) (make-vector size #f)])
      (for/fold
          ([acc : (U False (Listof Integer)) '()])
          ([src : Integer (in-range 0 size)]
           #:unless (vector-ref processed src))
           #:break (false? acc)
           (top-sort-rec graph src discovered processed acc))))



  (: find-order (Integer (Listof (List Integer Integer)) -> (Listof Integer)))
  (define (find-order num-courses prereqs)
    (: graph Graph)
    (define graph (initialize-empty-graph num-courses))
    (for ([prereq prereqs])
      (insert-edge! graph (second prereq) (first prereq)))
    (or (top-sort graph) '())))

(require 'M)
