#lang racket

(require racket/fixnum)

(define (char->index ch)
  (- (char->integer ch) (char->integer #\a)))

(define (make-countvec)
  (make-fxvector 26 0))

(define (countvec-fill! vec)
  (for ([i (in-range 26)])
    (fxvector-set! vec i 0)))

(define (string->countvec vec s)
  (for ([ch s])
    (define idx (char->index ch))
    (fxvector-set! vec idx
                   (add1 (fxvector-ref vec idx))))
  vec)

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define/contract (remove-anagrams words)
  (-> (listof string?) (listof string?))
  (cond
    [(null? words) '()]
    [else
     (define head (first words))
     (define tail (rest words))
     (define vec0 (string->countvec (make-countvec) head))
     (define vec1 (make-countvec))
     (define acc
       (for/fold
         ([acc (list head)])
         ([word tail])
         (cond
           [(equal? vec0 (string->countvec vec1 word))
            (countvec-fill! vec1)
            acc]
           [else
            (swap vec0 vec1)
            (countvec-fill! vec1)
            (cons word acc)])))
     (reverse acc)]))
