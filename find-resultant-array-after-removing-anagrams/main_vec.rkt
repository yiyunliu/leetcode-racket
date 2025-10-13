#lang racket

(require racket/fixnum)

(define (char->index ch)
  (- (char->integer ch) (char->integer #\a)))

(define (make-countvec)
  (make-fxvector 26 0))

(define (string->countvec s)
  (define vec (make-countvec))
  (for ([ch s])
    (define idx (char->index ch))
    (fxvector-set! vec idx
                   (add1 (fxvector-ref vec idx))))
  vec)

(define (anagram? s0 s1)
  (equal? (string->countvec s0) (string->countvec s1)))

(define/contract (remove-anagrams words)
  (-> (listof string?) (listof string?))
  (cond
    [(null? words) '()]
    [else
     (define head (first words))
     (define tail (rest words))
     (define-values (_ acc)
       (for/fold
         ([prev head]
          [acc (list head)])
         ([word tail])
         (cond
           [(anagram? prev word)
            (values prev acc)]
           [else
            (values word (cons word acc))])))
     (reverse acc)]))
