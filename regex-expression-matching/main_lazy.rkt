#lang typed/racket

(struct Star ([char : Char]) #:transparent)

(: wild-card? (-> Char Boolean))
(define (wild-card? ch)
  (char=? ch #\.))

(: parse-regex (-> String (Listof (U Char Star))))
(define (parse-regex regex)
  (define end (string-length regex))
  (reverse (let loop : (Listof (U Char Star))
                ([start : Integer 0]
                 [acc : (Listof (U Char Star)) '() ])
             (cond
               [(= start end) acc]
               [(= (add1 start) end) (cons (string-ref regex start) acc)]
               [(char=? (string-ref regex (add1 start)) #\*)
                (loop (+ 2 start) (cons (Star (string-ref regex start)) acc))]
               [else
                (loop (add1 start) (cons (string-ref regex start) acc))]))))

(: is-match (-> String String Boolean))
(define (is-match s _regex)
  (define len (string-length s))
  (: regex (Vectorof (U Char Star)))
  (define regex (list->vector (parse-regex _regex)))
  (define rlen (vector-length regex))
  (: cache (Vectorof (Vectorof (Promise Boolean))))
  (define cache (build-vector
                 (add1 len)
                 (lambda ([i : Integer])
                   (build-vector
                    (add1 rlen)
                    (lambda ([j : Integer])
                      (delay (go i j)))))))

  (: cache-ref (-> Integer Integer Boolean))
  (define (cache-ref i j)
    (force (vector-ref (vector-ref cache i) j)))

  (: char-match? (-> Char Char Boolean))
  (define (char-match? r ch)
    (or (wild-card? r) (char=? r ch)))
  
  (: go (-> Integer Integer Boolean))
  (define (go i j)
    (cond
      [(= j 0)
       (zero? i)]
      [(= i 0)
       (and (Star? (vector-ref regex (sub1 j))) (cache-ref 0 (sub1 j)))]
      [else
       (define r (vector-ref regex (sub1 j)))
       (define ch (string-ref s (sub1 i)))
       (cond
         [(char? r) (and (char-match? r ch) (cache-ref (sub1 i) (sub1 j)))]
         [else (let ([r (Star-char r)])
                 (or (cache-ref i (sub1 j))
                     (and (char-match? r ch) (cache-ref (sub1 i) j))))])]))

  (go len rlen))


(module+ test
  (require typed/rackunit)
  (check-false (is-match "aa" "a"))
  (check-true (is-match "aa" "aa"))
  (check-true (is-match "ab" ".*"))
  (check-true (is-match "abcbbbddda" "abcbb*.*"))
  (check-false (is-match "abcbbbddda" "abcbb*.*b"))
  (check-false (is-match "a" "ab*a")))
