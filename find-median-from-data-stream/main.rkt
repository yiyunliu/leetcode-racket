#lang racket

(require data/heap)

(struct two-heaps (max-heap min-heap)  #:transparent)

(define median-finder%
  (class object%
    (super-new)

    (init-field)
    (define th (make-two-heaps))

    ; add-num : exact-integer? -> void?
    (define/public (add-num num)
      (two-heaps-add! th num))
    ; find-median : -> flonum?
    (define/public (find-median)
      (two-heaps-median th))))

(define (make-two-heaps)
  (two-heaps (make-heap >) (make-heap <)))

(define (two-heaps-count th)
  (match-define (two-heaps max-heap min-heap) th)
  (+ (heap-count max-heap) (heap-count min-heap)))

(define (two-heaps-add! th num)
  (match-define (two-heaps max-heap min-heap) th)
  (define th-count (two-heaps-count th))
  (cond
    [(zero? th-count)
     (heap-add! min-heap num)]
    [(= 1 th-count)
     (define right-min (heap-min min-heap))
     (if (< num right-min)
         (heap-add! max-heap num)
         (begin
           (heap-remove-min! min-heap)
           (heap-add! min-heap num)
           (heap-add! max-heap right-min)))]
    [else
     (define right-min (heap-min min-heap))
     (define left-max (heap-min max-heap))
     (cond
       [(and (even? th-count) (>= num left-max))
        (heap-add! min-heap num)]
       [(and (odd? th-count) (<= num right-min))
        (heap-add! max-heap num)]
       [(even? th-count)
        (heap-add! min-heap left-max)
        (heap-remove-min! max-heap)
        (heap-add! max-heap num)]
       [else
        (heap-add! max-heap right-min)
        (heap-remove-min! min-heap)
        (heap-add! min-heap num)])]))

(define (two-heaps-median th)
  (match-define (two-heaps max-heap min-heap) th)
  (cond
    [(odd? (two-heaps-count th))
     (heap-min min-heap)]
    [else
     (/ (+ (heap-min max-heap) (heap-min min-heap)) 2)]))

(module+ test
  (require rackunit)


  (test-case
      "Two heap properly picks out medians (2)"
    (for ([xs (permutations '(2 4))])
      (define th (make-two-heaps))
      (for ([i xs])
        (two-heaps-add! th i))
      (check-equal? (two-heaps-median th) 3)))

  (test-case
      "Two heap properly picks out medians (3)"
    (for ([xs (permutations '(2 3 4))])
      (define th (make-two-heaps))
      (for ([i xs])
        (two-heaps-add! th i))
      (check-equal? (two-heaps-median th) 3)))

  (test-case
      "Two heap properly picks out medians (4)"
    (for ([xs (permutations '(2 4 6 9))])
      (define th (make-two-heaps))
      (for ([i xs])
        (two-heaps-add! th i))
      (check-equal? (two-heaps-median th) 5)))


  (test-case
      "Two heap properly picks out medians (5)"
    (for ([xs (permutations '(2 4 6 9 10))])
      (define th (make-two-heaps))
      (for ([i xs])
        (two-heaps-add! th i))
      (check-equal? (two-heaps-median th) 6))))
