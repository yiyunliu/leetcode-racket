#lang racket

(module M typed/racket
  (provide max-profit)

  (: max-profit (-> (Pair Integer (Listof Integer)) Integer))

  (define (max-profit prices)
    (define-values (min-price max-profit)
      (for/fold
        ([min-price : Integer (car prices)]
         [max-profit : Integer 0])
        ([price (cdr prices)])
        (define new-min-price (min min-price price))
        (define profit (max 0 (- price min-price)))
        (values new-min-price (max profit max-profit))))
    max-profit))

(require 'M)
