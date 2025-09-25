#lang racket

(module M typed/racket
  (provide empty-trie insert-trie-node! update-query-state)

  (define-type Trie
    (U TrieNode False))

  (define-type QueryState (Listof TrieNode))

  (define-struct TrieNode
    ([is-word : Boolean]  [children :  (Vectorof Trie)] ) #:mutable)

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


  (: update-query-state (-> QueryState Char (Values QueryState Boolean)))
  (define (update-query-state tries ch)
    (let ([ch (char->index ch)])
      (for/fold
          ([acc-tries : QueryState '()]
           [acc-bool : Boolean #f])
          ([trie tries])
        (let ([result (trie-query-char trie ch)])
          (if result
              (values (cons (car result) acc-tries) (or acc-bool (cdr result)))
              (values acc-tries (or result acc-bool)))))))

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

  (: trie-query-char (-> TrieNode Integer (U False (Pairof TrieNode Boolean))))
  (define (trie-query-char trie ch)
    (let ([maybe-child (vector-ref (TrieNode-children trie) ch)])
      (if maybe-child
          (cons maybe-child (TrieNode-is-word maybe-child))
          #f))))

(require 'M)

(define stream-checker%
  (class object%
    (super-new)
    
    ; words : (listof string?)
    (init-field
      words)

    (define trie
      (let ([ret (empty-trie)])
        (for ([word words])
          (insert-trie-node! ret word))
        ret))

    (define query-state (list trie))

    ; query : char? -> boolean?
    (define/public (query letter)
      (let-values ([(new-state ret) (update-query-state query-state letter)])
        (set! query-state (cons trie new-state))
        ret))))
