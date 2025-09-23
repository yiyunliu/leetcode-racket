#lang racket

(define (expand-palindrome l r s acc)
  (if (and (>= l 0) (< r (string-length s)) (eqv? (string-ref s l) (string-ref s r)))
      (expand-palindrome (sub1 l) (add1 r) s (+ acc 1))
      acc))

(define (longest-palindrome s)
  (if (<= (string-length s) 1)
      s
      (let-values ([(omaxidx omaxlen)
                    (for/fold
      ([maxidx -1]
       [maxlen -1])
      ([idx (in-range 0 (string-length s))])
                      (let ([newlen (expand-palindrome (sub1 idx) (add1 idx) s 0)])
                        (if (< maxlen newlen)
                            (values idx newlen)
                            (values maxidx maxlen))))]
                   [(emaxidx emaxlen)
                    (for/fold
                      ([maxidx -1]
                       [maxlen -1])
                      ([idx (in-range 0 (sub1 (string-length s)))])
                      (let ([newlen (expand-palindrome idx (add1 idx) s 0)])
                        (if (< maxlen newlen)
                            (values idx newlen)
                            (values maxidx maxlen))))])
        (if (>= omaxlen emaxlen)
            (substring s (- omaxidx omaxlen) (+ omaxidx omaxlen 1))
            (substring s (- (add1 emaxidx) emaxlen) (+ emaxidx emaxlen 1))))))
