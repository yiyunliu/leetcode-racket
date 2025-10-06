#lang typed/racket

(define-type Trie (U TrieNode False))
(define-struct TrieNode ([is-word? : Boolean] [children : (Vectorof Trie)]) #:mutable)

(: alphabet-size Index)
(define alphabet-size 26)

(: make-trie (-> TrieNode))
(define (make-trie)
  (TrieNode #f (make-vector alphabet-size #f)))

(: char->index (-> Char Integer))
(define (char->index ch)
  (- (char->integer ch) (char->integer #\a)))

(: trie-insert! (-> TrieNode String Void))
(define (trie-insert! trie str)
  (set-TrieNode-is-word?!
   (for/fold
       ([trie : TrieNode trie])
       ([_ch : Char str])
     (define ch (char->index _ch))
     (define nodes (TrieNode-children trie))
     (define new-trie
       (or (vector-ref nodes ch)
           (let ([ret (make-trie)])
             (vector-set! nodes ch ret)
             ret)))
     (values new-trie))
   #t))


(: trie-member? (-> TrieNode String Boolean))
(define (trie-member? trie str)
  (define len (string-length str))
  (let loop : Boolean ([trie : TrieNode trie] [idx : Integer 0])
    (cond
      [(= idx len)
       (TrieNode-is-word? trie)]
      [else
       (define ch (string-ref str idx))
       (cond
         [(eqv? #\. ch)
          (for/and : Boolean
                   ([trie (TrieNode-children trie)]
                    #:unless (false? trie))
            (loop trie (add1 idx)))]
         [else
          (define new-trie (vector-ref (TrieNode-children trie) (char->index ch)))
          (and new-trie (loop new-trie (add1 idx)))])])))


(module+ test
  (require typed/rackunit)
  (let ([trie (make-trie)])
    (for ([str '("hello" "hella" "pink" "hej")])
      (trie-insert! trie str))
    (check-true (trie-member? trie "hello"))
    (check-true (trie-member? trie "hella"))
    (check-true (trie-member? trie "he..a"))
    (check-false (trie-member? trie "pint"))
    (check-true (trie-member? trie "pin."))
    (check-false (trie-member? trie "hell"))))
