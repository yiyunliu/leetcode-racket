#lang racket

(define (anagram? s0 s1)
  (define ht
    (for/fold
        ([ht (hash)])
        ([ch s0])
      (hash-update ht ch add1 0)))
  (let-values
      ([(ht acc)
        (for/fold
             ([ht ht]
              [acc #t])
             ([ch s1])
          #:break (false? acc)
          (cond
            [(hash-has-key? ht ch)
             (define cnt (hash-ref ht ch))
             (cond
               [(= cnt 1) (values (hash-remove ht ch) acc)]
               [else (values (hash-set ht ch (sub1 cnt)) acc)])]
            [else (values ht #f)]))])
    (and acc (hash-empty? ht))))

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
