#lang typed/racket

(define-type Price Integer)
(define-type Count Integer)
(define-type Map (HashTable Price Count))

(require/typed data/heap
  [#:opaque Heap heap?]
  [make-heap (-> (-> Price Price Boolean) Heap)]
  [heap-add! (-> Heap Price * Void)]
  [heap-min (-> Heap Price)]
  [heap-remove-min! (-> Heap Void)]
  [heap->vector (-> Heap (Vectorof Price))])

(define-struct Backlog ([cnt : Map] [heap : Heap]) #:transparent)

(: sell-order? (-> (U 0 1) Boolean))
(define (sell-order? u)
  (eqv? u 1))

(: backlog-initialize (-> (U 0 1) Backlog))
(define (backlog-initialize type)
  (Backlog (make-hash)
           (if (sell-order? type)
               (make-heap <)
               (make-heap >))))

(: backlog-insert! (-> Backlog Price Count Void))
(define (backlog-insert! backlog price amount)
  (define cnt (Backlog-cnt backlog))
  (define heap (Backlog-heap backlog))
  (unless (hash-has-key? cnt price)
    (heap-add! heap price))
  (hash-update! cnt price
                (lambda ([x : Count]) (+ x amount))
                (lambda () 0)))

(: backlog-empty? (-> Backlog Boolean))
(define (backlog-empty? backlog)
  (hash-empty? (Backlog-cnt backlog)))

(: backlog-top (-> Backlog (Values Price Count)))
(define (backlog-top backlog)
  (define cnt-map (Backlog-cnt backlog))
  (define heap (Backlog-heap backlog))
  (let ([price (heap-min heap)])
    ;; (printf "\nmin price:~a\n" price)
    (values price (hash-ref cnt-map price))))

;; Assumes that there are that many elements to remove
(: backlog-remove! (-> Backlog Count Void))
(define (backlog-remove! backlog n)
  (let-values ([(price count) (backlog-top backlog)])
    (let ([new-count (- count n)])
      (if (zero? new-count)
          (begin
            (hash-remove! (Backlog-cnt backlog) price)
            (heap-remove-min! (Backlog-heap backlog)))
          (hash-set! (Backlog-cnt backlog) price new-count)))))

(: process-buy-order! (-> Backlog Backlog Price Count Void))
(define (process-buy-order! buy-log sell-log price count)
  (unless (zero? count)
      (if (backlog-empty? sell-log)
          (backlog-insert! buy-log price count)
          (let-values
              ([(sprice scount) (backlog-top sell-log)])
            (if (<= sprice price)
                (begin
                  ;; (print "here")
                  (let ([sell-count (min count scount)])
                    (backlog-remove! sell-log sell-count)
                    (process-buy-order! buy-log sell-log price
                                        (- count sell-count))))
                (backlog-insert! buy-log price count))))))


(: process-sell-order! (-> Backlog Backlog Price Count Void))
(define (process-sell-order! buy-log sell-log price count)
  (unless (zero? count)
      (if (backlog-empty? buy-log)
          (backlog-insert! sell-log price count)
          (let-values
              ([(bprice bcount) (backlog-top buy-log)])
            (if (<= price bprice)
                (begin
                  (let ([buy-count (min count bcount)])
                    (backlog-remove! buy-log buy-count)
                    (process-sell-order! buy-log sell-log price
                                        (- count buy-count))))
                (backlog-insert! sell-log price count))))))


(: get-number-of-backlog-orders (-> (Listof (List Price Count (U 0 1) )) Integer))
(define (get-number-of-backlog-orders orders)
  (let ([buy-log (backlog-initialize 0)]
        [sell-log (backlog-initialize 1)])
    (for ([order orders])
      (let ([type (third order)]
            [count (second order)]
            [price (first order)])
        ((if (eqv? type 1) process-sell-order! process-buy-order!)
         buy-log
         sell-log
         price
         count)))
    (remainder (+
     (for/fold ([acc : Integer 0])
               ([(_ v) (Backlog-cnt buy-log)])
       (+ v acc))
     (for/fold ([acc : Integer 0])
               ([(_ v) (Backlog-cnt sell-log)])
       (+ v acc))) (+ 7 (expt 10 9)))))
