(module M typed/racket
  (provide product-except-self)
  (: product-except-self (-> (Listof Integer) (Listof Integer)))
  (define (product-except-self _nums)
    (define nums (list->vector _nums))
    (define nums-len (vector-length nums))

    (: prefix-prod (Vectorof Integer))
    (define prefix-prod
      (make-vector (add1 nums-len) 1))

    (: suffix-prod (Vectorof Integer))
    (define suffix-prod
      (make-vector (add1 nums-len) 1))

    (for ([i (in-range 0 nums-len)])
      (vector-set! prefix-prod
                   (add1 i)
                   (* (vector-ref prefix-prod i) (vector-ref nums i)))
      (define inv-i (- nums-len i 1))
      (vector-set! suffix-prod
                   inv-i
                   (* (vector-ref suffix-prod (add1 inv-i)) (vector-ref nums inv-i))))
    (for/list ([i (in-range 0 nums-len)])
      (* (vector-ref prefix-prod i) (vector-ref suffix-prod (add1 i))))))

(require 'M)
