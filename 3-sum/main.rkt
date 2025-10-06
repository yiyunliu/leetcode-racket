#lang typed/racket

(: two-sum (-> (Vectorof Integer) Integer Integer (Listof Integer) (Listof Integer)))


;; (define (two-sums num))

;; (define/contract (three-sum nums)
;;   (-> (listof exact-integer?) (listof (listof exact-integer?)))
;;   (define vnums (list->vector nums))
;;   (vector-sort! vnums <)
;;   (define len (vector-length vnums))
;;   (define result
;;     (for*/set
;;         ([i (in-range (- len 2))]
;;          [j (in-range (add1 i) (sub1 len))]
;;          [k (in-range (add1 j) len )])
;;       (list (vector-ref vnums i) (vector-ref vnums j) (vector-ref vnums k))))
;;   (filter
;;    (lambda (l)
;;      (match-define (list i j k) l)
;;      (zero? (+ i j k)))
;;    (set->list result)))
