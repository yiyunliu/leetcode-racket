#lang typed/racket

(require racket/promise)

(: maximum-total-damage (-> (Listof Integer) Integer))
(define (maximum-total-damage spells)
  (define power->counts
    (for/fold
        ([ht : (Immutable-HashTable Integer Integer) (hash)])
        ([spell spells])
      (hash-update ht spell (lambda ([x : Integer]) (add1 x)) (lambda () 0))))

  (define sorted-spells
    (let ([result
           (for/vector : (Vectorof (Pair Integer Integer))
                       ([(k v) power->counts])
             (cons k v))])
      ((inst vector-sort! (Pair Integer Integer) Integer) result < #:key car)
      result))

  (define len (vector-length sorted-spells))

  (: cache (Vectorof (Promise Integer)))
  ;; begin, end
  (define cache (build-vector
                 (add1 len)
                 (lambda ([idx : Integer])
                   (delay (go idx)))))

  (: cache-ref (-> Integer Integer))
  (define (cache-ref i)
    (force (vector-ref cache i)))


  (: go (-> Integer Integer))
  (define (go i)
    (cond
      ;; i = 0
      [(zero? i) 0]
      ;; i > 0
      [else
       (match-define (cons power cnt) (vector-ref sorted-spells (sub1 i)))
       (define total-power (* power cnt))
       (define max-without-i (cache-ref (sub1 i)))
       (define max-prev
         (for/fold
             ([acc : (U Integer #f) #f])
             ([j : Integer (in-range (- i 1) 0 -1)])
           #:break acc
           (if (< (+ 2 (car (vector-ref sorted-spells (sub1 j)))) power)
               (begin
                 (cache-ref j))
               acc)))
       (define max-with-i (+ total-power (or max-prev 0)))
       (max max-without-i max-with-i)]))
  (go len))
