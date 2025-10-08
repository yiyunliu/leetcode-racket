#lang typed/racket

(: star? (-> Char Boolean))
(define (star? ch)
  (char=? ch #\*))

(: one? (-> Char Boolean))
(define (one? ch)
  (char=? ch #\?))

(: is-match (-> String String Boolean))
(define (is-match s p)
  (define slen (string-length s))
  (define plen (string-length p))

  (: cache (Vectorof (Vectorof (Promise Boolean))))
  (define cache
    (build-vector
     (add1 slen)
     (lambda ([i : Integer])
       (build-vector
        (add1 plen)
        (lambda ([j : Integer])
          (delay (go i j)))))))

  (: cache-ref (-> Integer Integer Boolean))
  (define (cache-ref i j)
    (force (vector-ref (vector-ref cache i) j)))

  (: go (-> Integer Integer Boolean))
  (define (go i j)
    (cond
      [(= j 0)
       (zero? i)]
      [(= i 0)
       (and (star? (string-ref p (sub1 j)))  (cache-ref i (sub1 j)))]
      [else
       (define pat (string-ref p (sub1 j)))
       (define ch (string-ref s (sub1 i)))
       (cond
         [(one? pat)
          (cache-ref (sub1 i) (sub1 j))]
         [(star? pat) (or (cache-ref (sub1 i) j)
                         (cache-ref i (sub1 j)))]
         [else
          (and (char=? pat ch) (cache-ref (sub1 i) (sub1 j)))])]))

  (go slen plen))


(module+ test
  (require typed/rackunit)
  (check-false (is-match "aa" "a"))
  (check-true (is-match "aa" "aa"))
  (check-true (is-match "a" "?"))
  (check-true (is-match "abc" "???"))
  (check-false (is-match "abc" "????"))
  (check-false (is-match "abc" "??"))
  (check-false (is-match "aaaa" "*b"))
  (check-true (is-match "aaaa" "*"))
  (check-true (is-match "aaaa" "a*a")))
