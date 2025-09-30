#lang racket

(module M typed/racket
  (provide reverse)
  (define-type Sign (U '- '+))

  (: list-reverse (All (A) (-> (Listof A) (Listof A))))
  (define (list-reverse xs)
    (for/fold ([acc '()])
              ([x xs])
      (cons x acc)))

  (: number->list (-> Integer (Values Sign (Listof Integer))))
  (define (number->list n)
    (: sign Sign)
    (define sign
      (if (negative? n)
          '-
          '+))
    
    (: loop (-> Integer (Listof Integer) (Listof Integer)))
    (define (loop n xs)
      (if (= n 0)
          xs
          (let-values ([(n rem) (quotient/remainder n 10)])
            (loop n (cons rem xs)))))
    (values sign (list-reverse (loop (if (eqv? '- sign) (- n) n) '()))))


  (: eval-sign (-> Sign Integer Integer))
  (define (eval-sign sign x)
    (case sign
      ['- (- x)]
      ['+ x]))

  (: overflow? (-> Integer Integer Boolean))
  (define (overflow? acc digit)
    (cond
      [(< acc 214748364) #f]
      [(= acc 214748364) (> digit 7)]
      [else #t]))

  (: reverse (-> Integer Integer))
  (define (reverse x)
    (let/ec k : Integer
      (let-values ([(sign digits) (number->list x)])
        (eval-sign sign
                   (for/fold ([acc : Integer 0])
                             ([digit digits])
                     (when (overflow? acc digit)
                       (k 0))
                     (+ (* 10 acc) digit)))))))
