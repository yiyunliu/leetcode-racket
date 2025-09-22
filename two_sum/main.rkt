#lang typed/racket
(require racket/fixnum)
(: two-sum (-> (Listof Integer) Integer (List Integer Integer)))
(define (two-sum nums target)
  (let ([prev : (HashTable Integer Integer) (make-hash)])
    (let ([result
           (for/fold ([result : (U (List Integer Integer) Null) '()])
                     ([num : Integer nums]
                      [i : Integer (in-naturals)])
             #:break (not (null? result))
             (let ([complement (hash-ref prev (- target num) #f)])
               (if complement
                   (list complement i)
                   (begin
                     (hash-set! prev num i)
                     result))))])
      (if (null? result)
          (error "Impossible")
          result))))
