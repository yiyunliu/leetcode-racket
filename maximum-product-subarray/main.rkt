#lang racket

(module M typed/racket
  (provide max-product)
  (: max-product (-> (Listof Integer) Integer))
  (define (max-product nums)
    (define-values (prod-all prod-since-neg found-first-neg max-acc)
      (for/fold
      ([prod-all : (U Integer #f) #f]
       [prod-since-neg : Integer 1]
       [found-first-neg : Boolean #f]
       [max-acc : (U Integer #f) #f])
      ([num nums])
        (cond
          [(negative? num)
           (define new-prod-all (if prod-all (* num prod-all) num))
           (define new-prod-since-neg
             (if found-first-neg (* prod-since-neg num) 1))
           (values new-prod-all
                   new-prod-since-neg
                   #t
                   (if max-acc
                       (max new-prod-all (if found-first-neg new-prod-since-neg new-prod-all) max-acc)
                       (max new-prod-all (if found-first-neg new-prod-since-neg new-prod-all))))]
          [(positive? num)
           (define new-prod-all (if prod-all (* num prod-all) num))
           (define new-prod-since-neg (if prod-since-neg (* prod-since-neg num) 1))
           (define new-max (if max-acc
                               (max new-prod-all (if found-first-neg new-prod-since-neg new-prod-all) max-acc)
                               (max new-prod-all (if found-first-neg new-prod-since-neg new-prod-all))))
           (values new-prod-all new-prod-since-neg found-first-neg new-max)]
          [else
           (values #f 1 #f (if max-acc (max max-acc 0) 0))])))
    (or max-acc 0)))
(require 'M)
