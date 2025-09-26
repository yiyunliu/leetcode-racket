#lang racket

(module M typed/racket
  (provide longest-common-prefix)
  
  (define-struct Slice ([str : String] [len : Integer]))

  (: string->slice (-> String Slice))
  (define (string->slice str)
    (Slice str (string-length str)))


  (: longest-common-prefix (-> (Listof String) String))
  (define (longest-common-prefix lst)
    (: common-slice (-> Slice String (U Slice False)))
    (define (common-slice slice str)
      (let ([slice-str (Slice-str slice)])
        (let ([new-count
               (for/fold
                 ([cnt : Integer 0])
                 ([idx : Integer (in-range 0 (Slice-len slice))]
                  [ch : Char str])
                 #:break (not (eqv? (string-ref slice-str idx) ch))
                 (add1 cnt))])
          (if (zero? new-count)
              #f
              (Slice slice-str new-count)))))
    (if (null? lst)
        ""
        (let ([result 
               (for/fold
               ([acc : (U Slice False) (string->slice (first lst))])
               ([str : String (rest lst)])
                 #:break (not acc)
                 (common-slice acc str))])
          (if result
              (substring (Slice-str result) 0 (Slice-len result))
              ""))))
  )

(require 'M)
