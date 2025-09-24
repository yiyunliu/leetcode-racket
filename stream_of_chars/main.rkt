#lang typed/racket

(define-type Trie
  (U TrieNode False))

(define-type TrieNode
  (Vectorof Trie))

(: alphabet-size Integer)
(define alphabet-size 24)

(: empty-trie-node (-> TrieNode))
(define (empty-trie-node)
  (make-vector alphabet-size #f))

(: singleton-node (-> Integer TrieNode))
(define (singleton-node i)
  (let ([vec (empty-trie-node)])
    (vector-set! vec i (empty-trie-node))
    vec))

(: char->index (-> Char Integer))
(define (char->index ch)
  (- (char->integer ch) 97))

(: insert-trie-node! (-> String Integer TrieNode Void))
(define (insert-trie-node! str idx trie)
  (when (< idx (string-length str))
      (let* ([ch (char->index (string-ref str idx))]
             [maybe-trie (vector-ref trie ch)])
        (if maybe-trie
            (insert-trie-node! str (add1 idx) maybe-trie)
            (let ([subtrie (singleton-node ch)])
              (vector-set! trie ch subtrie)
              (insert-trie-node! str (add1 idx) subtrie))))))


(: trie-prefix-rec? (-> TrieNode (Listof Char) Boolean))
(define (trie-prefix-rec? trie str)
  (or (null? str)
      (let ([ch (char->index (car str))]
            [str (cdr str)])
        (let ([maybe-trie (vector-ref trie ch)])
          (and maybe-trie (trie-prefix-rec? maybe-trie str))))))
