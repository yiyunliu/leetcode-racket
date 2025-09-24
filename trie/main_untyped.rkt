#lang racket

(define alphabet-size 24)

(define (empty-trie-node)
  (make-vector alphabet-size #f))

(define (singleton-node i)
  (let ([vec (empty-trie-node)])
    (vector-set! vec i (empty-trie-node))
    vec))

(define (char->index ch)
  (- (char->integer ch) 97))

(define (insert-trie-node! str idx trie)
  (when (< idx (string-length str))
      (let* ([ch (char->index (string-ref str idx))]
             [maybe-trie (vector-ref trie ch)])
        (if maybe-trie
            (insert-trie-node! str (add1 idx) maybe-trie)
            (let ([subtrie (singleton-node ch)])
              (vector-set! trie ch subtrie)
              (insert-trie-node! str (add1 idx) subtrie))))))


(define (trie-prefix-rec? trie str)
  (or (null? str)
      (let ([ch (char->index (car str))]
            [str (cdr str)])
        (let ([maybe-trie (vector-ref trie ch)])
          (and maybe-trie (trie-prefix-rec? maybe-trie str))))))


(define trie%
  (class object%
    (super-new)
    
    (init-field)
    (define trie (empty-tree-node))
    ; insert : string? -> void?
    (define/public (insert word)
      (insert-trie-node! word 0 trie))
    ; search : string? -> boolean?
    (define/public (search word)
      )
    ; starts-with : string? -> boolean?
    (define/public (starts-with prefix)
      )))
