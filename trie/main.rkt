#lang typed/racket

(define-type Trie
  (U TrieNode False))

(define-struct TrieNode
  ([is-word : Boolean]  [children :  (Vectorof Trie)] ) #:mutable)

(: alphabet-size Integer)
(define alphabet-size 26)

(: empty-trie-vector (-> (Vectorof Trie)))
(define (empty-trie-vector)
  (make-vector alphabet-size #f))

(: empty-trie (-> TrieNode))
(define (empty-trie)
  (TrieNode #f (empty-trie-vector)))

(: char->index (-> Char Integer))
(define (char->index ch)
  (- (char->integer ch) 97))

(: insert-trie-node-rec! (-> String Integer TrieNode Void))
(define (insert-trie-node-rec! str idx trie)
  (if (< idx (string-length str))
      (let* ([ch (char->index (string-ref str idx))]
             [maybe-trie (vector-ref (TrieNode-children trie) ch)])
        (if maybe-trie
            (insert-trie-node-rec! str (add1 idx) maybe-trie)
            (let ([subtrie (empty-trie)])
              (vector-set! (TrieNode-children trie) ch subtrie)
              (insert-trie-node-rec! str (add1 idx) subtrie))))
      (set-TrieNode-is-word! trie #t)))

(: insert-trie-node! (-> TrieNode String Void))
(define (insert-trie-node! trie str)
  (insert-trie-node-rec! str 0 trie))


(: trie-prefix-rec? (-> TrieNode String Natural Boolean))
(define (trie-prefix-rec? trie str idx)
  (or (eqv? idx (string-length str))
      (let ([ch (char->index (string-ref str idx))])
        (let ([maybe-trie (vector-ref (TrieNode-children trie) ch)])
          (and maybe-trie (trie-prefix-rec? maybe-trie str (add1 idx)))))))

(: trie-prefix? (-> TrieNode String Boolean))
(define (trie-prefix? trie str)
  (trie-prefix-rec? trie str 0))


(: trie-member-rec? (-> TrieNode String Natural Boolean))
(define (trie-member-rec? trie str idx)
  (if (eqv? idx (string-length str))
      (TrieNode-is-word trie)
      (let ([ch (char->index (string-ref str idx))])
        (let ([maybe-trie (vector-ref (TrieNode-children trie) ch)])
          (and maybe-trie (trie-member-rec? maybe-trie str (add1 idx)))))))

(: trie-member? (-> TrieNode String Boolean))
(define (trie-member? trie str)
  (trie-member-rec? trie str 0))

(module+ test
  (require typed/rackunit)
  (let ([trie (empty-trie)])
    (insert-trie-node! trie "aaaaaab")
    (insert-trie-node! trie "aaaaaa")
    (insert-trie-node! trie "hello")
    (insert-trie-node! trie "aaaaaac")
    (insert-trie-node! trie "aaaa")
    (insert-trie-node! trie "hela")
    (insert-trie-node! trie "hej")
    (insert-trie-node! trie "kale")
    (insert-trie-node! trie "helloo")
    (insert-trie-node! trie "help")
    (check-true (trie-prefix? trie "a"))
    (check-true (trie-prefix? trie "aaaaaa"))
    (check-false (trie-prefix? trie "aaaaaaa"))
    (check-true (trie-prefix? trie "aaaaaa"))
    (check-false (trie-prefix? trie "kales"))
    (check-false (trie-prefix? trie "hed"))
    (check-true (trie-prefix? trie "hel"))
    (check-true (trie-prefix? trie "hell"))
    (check-true (trie-member? trie "hello"))
    (check-true (trie-member? trie "helloo"))
    (check-false (trie-member? trie "hell")))
  (let ([trie (empty-trie)])
    (insert-trie-node! trie "a")
    (check-true (trie-prefix? trie "a"))
    (check-false (trie-prefix? trie "aa"))))
