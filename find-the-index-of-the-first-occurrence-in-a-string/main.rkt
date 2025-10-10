#lang typed/racket

(: str-str (-> String String Integer))
(define (str-str haystack needle)
  (define needle-len (string-length needle))
  (or (for/or : (U Integer False)
              ([i (in-range 0 (add1 (- (string-length haystack) needle-len)))])
        (and (for/and : Boolean
                      ([j (in-range needle-len)])
               (char=? (string-ref haystack (+ i j))
                       (string-ref needle j))) i)) -1))
