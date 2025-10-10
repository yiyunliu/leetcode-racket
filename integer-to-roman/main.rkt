#lang typed/racket

(: int-to-roman (-> Integer String))
(define (int-to-roman num)
  (define os (open-output-string))
  (for/fold
      ([num : Integer num])
      ([f roman-vec])
    (define-values (rem digits) (f num))
    (display digits os)
    rem)
  (get-output-string os))

(: num-i (-> Integer (Values Integer String)))
(define num-i
  (lambda (quot)
    (values 0
            (cond
              [(<= quot 3) (make-string quot #\I)]
              [(= quot 4) "IV"]
              [else (error "Impossible")]))))


(: num-v (-> Integer (Values Integer String)))
(define (num-v num)
  (cond
    [(= 9 num) (values 0 "IX")]
    [else
     (define-values (quot rem) (quotient/remainder num 5))
     (values rem (make-string quot #\V))]))

(: num-x (-> Integer (Values Integer String)))
(define (num-x num)
  (cond
    [(<= 40 num) (values (- num 40) "XL")]
    [else
     (define-values (quot rem) (quotient/remainder num 10))
     (values rem (make-string quot #\X))]))

(: num-l (-> Integer (Values Integer String)))
(define (num-l num)
  (cond
    [(<= 90 num) (values (- num 90) "XC")]
    [else
     (define-values (quot rem) (quotient/remainder num 50))
     (values rem (make-string quot #\L))]))

(: num-c (-> Integer (Values Integer String)))
(define (num-c num)
  (cond
    [(<= 400 num) (values (- num 400) "CD")]
    [else
     (define-values (quot rem) (quotient/remainder num 100))
     (values rem (make-string quot #\C))]))

(: num-d (-> Integer (Values Integer String)))
(define num-d
  (lambda (num)
    (cond
      [(<= 900 num)
       (values (- num 900) "CM")]
      [(<= 500 num)
       (values (- num 500) "D")]
      [else
       (values num "")])))

(: num-m (-> Integer (Values Integer String)))
(define num-m
  (lambda (num)
    (define-values (quot rem) (quotient/remainder num 1000))
    (values rem (make-string quot #\M))))

(: roman-vec (Vectorof (-> Integer (Values Integer String))))
(define roman-vec (vector num-m num-d num-c num-l num-x num-v num-i))
