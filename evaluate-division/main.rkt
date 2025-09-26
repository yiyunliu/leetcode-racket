#lang racket

(module M typed/racket
  (provide calc-equation)
  (define-struct (A) Queue
    ([front : (Listof A)] (back : (Listof A)) (length : Integer)))

  (: make-queue (All (a) (-> a * (Queue a))))
  (define (make-queue . xs)
    (Queue xs '() (length xs)))

  (: queue-empty? (All (a) (-> (Queue a) Boolean)))
  (define (queue-empty? queue)
    (zero? (Queue-length queue)))

  (: queue (All (a) (-> (Listof a) (Listof a) Integer (Queue a) )))
  (define (queue front back len)
    (if (null? front)
        (Queue (reverse back) '() len)
        (Queue front back len)))

  (: queue-snoc (All (a) (-> (Queue a) a (Queue a))))
  (define (queue-snoc q a)
    (queue (Queue-front q)
           (cons a (Queue-back q))
           (add1 (Queue-length q))))

  (: queue-head (All (a) (-> (Queue a) a)))
  (define (queue-head q)
    (let ([front (Queue-front q)])
      (if (null? front)
          (error "Queue is empty!")
          (car front))))

  (: queue-pop (All (a) (-> (Queue a) (Values a (Queue a)))))
  (define (queue-pop q)
    (let ([front (Queue-front q)]
          [back (Queue-back q)]
          (len (Queue-length q)))
      (if (null? front)
          (error "Queue is empty!")
          (values (car front) (queue (cdr front) back (sub1 len))))))


  (define-type Vertex String)
  (define-struct EdgeNode ([ratio : Number] [target : Vertex]))
  (define-type Graph (HashTable Vertex (Listof EdgeNode)))

  (: insert-edge! (-> Graph Vertex Vertex Number Void))
  (define (insert-edge! graph src target ratio)
    (let ([new-edge (EdgeNode ratio target)])
      (hash-update! graph src
                    (lambda ([x : (Listof EdgeNode)]) (cons new-edge x))
                    (lambda () (list new-edge)))))

  (: build-graph (-> (Listof (List String String)) (Listof Number) Graph))
  (define (build-graph src-targets ratios)
    (let ([graph : Graph (make-hash)])
      (for ([src-target src-targets]
            [ratio ratios])
        (let ([src (first src-target)]
              [target (second src-target)])
          (insert-edge! graph src target ratio)
          (insert-edge! graph target src (/ 1.0 ratio))))
      graph))
  
  

  (: find-ratio (-> Graph Vertex Vertex Number))
  (define (find-ratio graph src orig-target)
    (let ([processed : (Setof String) (set)]
          [discovered : (Setof String) (set)]
          [queue : (Queue (Pairof Number String)) (make-queue (cons 1.0 src))])
      (set! discovered (set-add discovered src))
      (: loop (-> (U Number #f)))
      (define (loop)
        (if (queue-empty? queue)
            #f
            (let-values (([acc-vertex tl-queue] (queue-pop queue)))
              (set! queue tl-queue)
              (let ([vertex (cdr acc-vertex)]
                    [acc (car acc-vertex)])
                (if (equal? vertex orig-target)
                    acc
                    (let ([edges (hash-ref graph vertex)])
                      (for ([edge edges])
                        (let [(target (EdgeNode-target edge))
                              (ratio (EdgeNode-ratio edge))]
                          (unless (and (set-member? discovered target))
                            (set! discovered (set-add discovered target))
                            (set! queue (queue-snoc queue (cons (* acc ratio) target))))))
                      (set! processed (set-add processed vertex))
                      (loop)))))))
      (or (loop) -1.0)))

  (: calc-equation (-> (Listof (List String String)) (Listof Number) (Listof (List String String)) (Listof Number)))
  (define (calc-equation equations ratios queries)
    (let ([graph (build-graph equations ratios)])
      (for/list ([query queries])
        (let ([src (first query)]
              [target (second query)])
          (cond
            [(not (and (hash-has-key? graph src) (hash-has-key? graph target))) -1.0]
            [(equal? src target) 1.0]
            [else (find-ratio graph (first query) (second query))]))))))

(require 'M)
