#lang typed/racket

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
