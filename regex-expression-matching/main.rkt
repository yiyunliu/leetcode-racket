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
(define (is-match s regex)
  (define len (string-length s))
  (let loop : Boolean ([regex (parse-regex regex)] [start : Integer 0])
    (cond
      [(null? regex) (= start len)]
      [else
       (let ([r (car regex)]
             [regex (cdr regex)])
         (cond
           [(and (char? r) (= start len))
            #f]
           [(char? r)
            (and (or (wild-card? r) (char=? (string-ref s start) r))
                 (loop regex (add1 start)))]
           [else
            (let ([r (Star-char r)])
              (or (loop (cons r (cons (Star r) regex))
                        start)
                  (loop regex start)))]))])))
