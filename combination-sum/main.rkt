#lang typed/racket

(define-type MultiSet (HashTable Integer Integer))
(define-type Combinations (Setof MultiSet))

(: multiset-add (-> MultiSet Integer MultiSet))
(define (multiset-add mset i)
  (hash-update mset i add1 (lambda () 0)))

(: empty-set Combinations)
(define empty-set (set))
(: empty-multiset MultiSet)
(define empty-multiset (hash))


(: multiset->list (-> MultiSet (Listof Integer)))
(define (multiset->list mult)
  (for/fold ([acc : (Listof Integer) '()])
            ([([k : Integer] [v : Integer]) mult])
    (append (make-list v k) acc)))

(: combination-sum (-> (Listof Integer) Integer (Listof (Listof Integer))))
(define (combination-sum _candidates target)
  (define candidates (list->vector _candidates))
  (: cache (Vectorof Combinations))
  (define cache (make-vector (add1 target) (set empty-multiset)))
  (for ([i (in-range 1 (add1 target))])
    (vector-set! cache i
                 (for/fold
                     ([combs : Combinations empty-set])
                     ([candidate : Integer candidates])
                   (define diff (- i candidate))
                   (cond
                     [(negative? diff) combs]
                     [else
                      (for/fold
                          ([combs : Combinations combs])
                          ([comb (vector-ref cache diff)])
                        (set-add combs (multiset-add comb candidate)))]))))
  (for/list ([combination (vector-ref cache target)])
    (multiset->list combination)))
