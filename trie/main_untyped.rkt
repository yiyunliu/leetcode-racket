#lang racket

(define-struct TrieNode
  (is-word  children ) #:mutable)

(define alphabet-size 26)

(define (empty-trie-vector)
  (make-vector alphabet-size #f))

(define (empty-trie)
  (TrieNode #f (empty-trie-vector)))

(define (char->index ch)
  (- (char->integer ch) 97))

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

(define (insert-trie-node! trie str)
  (insert-trie-node-rec! str 0 trie))


(define (trie-prefix-rec? trie str idx)
  (or (eqv? idx (string-length str))
      (let ([ch (char->index (string-ref str idx))])
        (let ([maybe-trie (vector-ref (TrieNode-children trie) ch)])
          (and maybe-trie (trie-prefix-rec? maybe-trie str (add1 idx)))))))

(define (trie-prefix? trie str)
  (trie-prefix-rec? trie str 0))


(define (trie-member-rec? trie str idx)
  (if (eqv? idx (string-length str))
      (TrieNode-is-word trie)
      (let ([ch (char->index (string-ref str idx))])
        (let ([maybe-trie (vector-ref (TrieNode-children trie) ch)])
          (and maybe-trie (trie-member-rec? maybe-trie str (add1 idx)))))))

(define (trie-member? trie str)
  (trie-member-rec? trie str 0))


(define trie%
  (class object%
    (super-new)
    
    (init-field)
    (define trie (empty-trie))
    ; insert : string? -> void?
    (define/public (insert word)
      (insert-trie-node! trie word))
    ; search : string? -> boolean?
    (define/public (search word)
      (trie-member? trie word))
    ; starts-with : string? -> boolean?
    (define/public (starts-with prefix)
      (trie-prefix? trie prefix))))
