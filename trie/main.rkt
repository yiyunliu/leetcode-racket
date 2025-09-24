#lang typed/racket

(define-type Trie
  (U TrieNode False))

(define-struct TrieNode
  ([word? : Boolean]  [children :  (Vectorof Trie)] ) #:mutable)

(: alphabet-size Integer)
(define alphabet-size 24)

(: empty-trie-node (-> TrieNode))
(define (empty-trie-node)
  (TrieNode #f (make-vector alphabet-size #f)))

(: singleton-node (-> Boolean Integer TrieNode))
(define (singleton-node b i)
  (let ([vec (empty-trie-node)])
    (vector-set! vec i (empty-trie-node))
    (TrieNode b vec)))

(: char->index (-> Char Integer))
(define (char->index ch)
  (- (char->integer ch) 97))

(: insert-trie-node! (-> String Integer TrieNode Void))
(define (insert-trie-node! str idx trie)
  (when (< idx (string-length str))
      (let* ([ch (char->index (string-ref str idx))]
             [maybe-trie (vector-ref trie ch)])
        (if maybe-trie
            (let ([children (TrieNode-children maybe-tree)])
              (if (eq? idx (sub1 (string-length str)))
                  ()
                  (insert-trie-node! str (add1 idx) children)))
            (let ([subtrie (singleton-node ch)])
              (vector-set! trie ch subtrie)
              (insert-trie-node! str (add1 idx) subtrie))))))


(: trie-prefix-rec? (-> TrieNode String Natural Boolean))
(define (trie-prefix-rec? trie str idx)
  (or (< idx (string-length str))
      (let ([ch (char->index (string-ref str idx))])
        (let ([maybe-trie (vector-ref trie ch)])
          (and maybe-trie (trie-prefix-rec? maybe-trie str (add1 idx)))))))


(: trie-prefix? (-> TrieNode String Boolean))
(define (trie-prefix? trie str)
  (trie-prefix-rec? trie str 0))


;; (: trie-member-rec? (-> TrieNode String Natural Boolean))
;; (define (trie-member-rec? trie str idx)
;;   )
