#lang racket
(module M typed/racket
  (provide word-break)
  (define-struct Trie
    ([is-word? : Boolean] [children : (Vectorof (U Trie False))])
    #:mutable #:transparent)

  (: alphabet-size Natural)
  (define alphabet-size 26)

  (: char->index (-> Char Integer))
  (define (char->index ch)
    (- (char->integer ch) (char->integer #\a)))

  (: make-trie (-> Trie))
  (define (make-trie)
    (Trie #f (make-vector alphabet-size #f)))

  (: trie-query (-> Trie Char (U #f Trie)))
  (define (trie-query trie _ch)
    (define ch (char->index _ch))
    (vector-ref (Trie-children trie) ch))

  (: trie-insert-ch (-> Trie Integer Trie))
  (define (trie-insert-ch trie ch)
    (define child (vector-ref (Trie-children trie) ch))
    (or child (let ([new-node (make-trie)])
                (vector-set! (Trie-children trie) ch new-node)
                new-node)))

  (: trie-insert (-> Trie String Void))
  (define (trie-insert trie str)
    (define new-trie
      (for/fold ([trie : Trie trie])
                ([ch str])
        (let ([ch (char->index ch)])
          (trie-insert-ch trie ch))))
    (set-Trie-is-word?! new-trie #t)
    (void))

  (: word-break (-> String (Listof String) Boolean))
  (define (word-break s words)
    (define trie (make-trie))
    (for ([word words])
      (trie-insert trie word))
    (define tries
      (for/fold
      ([acc : (Listof Trie) (list trie)])
      ([ch : Char s])
        (define-values (new-acc matched?)
          (for/fold
            ([new-acc : (Listof Trie) '()]
             [matched? : Boolean #f])
            ([trie acc])
            (define maybe-trie (trie-query trie ch))
            (if maybe-trie
                (values
                 (cons maybe-trie new-acc)
                 (or matched? (Trie-is-word? maybe-trie)))
                (values new-acc matched?))))
        (if matched? (cons trie new-acc) new-acc)))
    (for/or ([trie tries])
      (Trie-is-word? trie))))

(require 'M)
