#lang racket

(define-struct Slice (str len))

(define (string->slice str)
  (Slice str (string-length str)))


(define (longest-common-prefix lst)
  (define (common-slice slice str)
    (let ([slice-str (Slice-str slice)])
      (let ([new-count
             (for/fold
                 ([cnt 0])
                 ([idx (in-range 0 (Slice-len slice))]
                  [ch  str])
               #:break (not (eqv? (string-ref slice-str idx) ch))
               (add1 cnt))])
        (if (zero? new-count)
            #f
            (Slice slice-str new-count)))))
  (if (null? lst)
      ""
      (let ([result 
             (for/fold
               ([acc (string->slice (first lst))])
               ([str (rest lst)])
               #:break (not acc)
               (common-slice acc str))])
        (if result
            (substring (Slice-str result) 0 (Slice-len result))
            ""))))
