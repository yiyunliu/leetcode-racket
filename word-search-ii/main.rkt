#lang typed/racket

(define-struct (A)
  Matrix ([num-rows : Integer]
          [num-cols : Integer]
          [data : (Vectorof A)]))

(: matrix-ref (All (A) (-> (Matrix A) Integer Integer A)))
(define (matrix-ref m i j)
  (match-define (Matrix _ num-cols data) m)
  (vector-ref data (+ j (* i num-cols))))

(: matrix-set! (All (A) (-> (Matrix A) Integer Integer A Void)))
(define (matrix-set! m i j a)
  (match-define (Matrix _ num-cols data) m)
  (vector-set! data (+ j (* i num-cols)) a))

(: list->matrix (All (A) (-> Integer Integer (Listof A) (Matrix A))))
(define (list->matrix num-rows num-cols data)
  (Matrix num-rows num-cols (list->vector data)))

(: build-matrix (All (A) (-> Integer Integer A (Matrix A))))
(define (build-matrix num-rows num-cols init-val)
  (Matrix num-rows num-cols (make-vector (* num-rows num-cols) init-val)))


(define-type Trie (U TrieNode #f))
(define-struct TrieNode ([is-word? : Boolean] [children : (Vectorof Trie)]) #:mutable #:transparent)

(: make-trie (-> TrieNode))
(define (make-trie)
  (TrieNode #f (make-vector 26 #f)))

(: char->index (-> Char Integer))
(define (char->index ch)
  (- (char->integer ch) (char->integer #\a)))


(: trie-insert! (-> TrieNode String Void))
(define (trie-insert! trie str)
  (define result
    (for/fold
      ([trie : TrieNode trie])
      ([_ch : Char str])
      (define ch (char->index _ch))
      (define children (TrieNode-children trie))
      (or (vector-ref children ch)
          (let ([new-trie (make-trie)])
            (vector-set! children ch new-trie)
            new-trie))))
  (set-TrieNode-is-word?! result #t))

(: trie-query-char (-> TrieNode Char Trie))
(define (trie-query-char trie ch)
  (vector-ref (TrieNode-children trie) (char->index ch)))


(: find-words (-> (Pair (Listof Char) (Listof (Listof Char))) (Listof String) (Listof String)))
(define (find-words _board words)
  (define num-rows (length _board))
  (define num-cols (length (car _board)))
  (define board (list->matrix num-rows num-cols (append* _board)))

  (: discovered (Matrix Boolean))
  (define discovered (build-matrix num-rows num-cols #f))

  (: discovered? (-> Integer Integer Boolean))
  (define (discovered? row col)
    (matrix-ref discovered row col))

  (: discovered! (-> Integer Integer Void))
  (define (discovered! row col)
    (matrix-set! discovered row col #t))

  (: undo-discovered! (-> Integer Integer Void))
  (define (undo-discovered! row col)
    (matrix-set! discovered row col #f))


  (: coord-valid? (-> Integer Integer Boolean))
  (define (coord-valid? row col)
    (and (natural? row) (natural? col) (< row num-rows) (< col num-cols)))

  (: next-coords (-> Integer Integer (Listof (Pair Integer Integer))))
  (define (next-coords row col)
    (filter
     (lambda ([coord : (Pair Integer Integer)])
       (match-define (cons row col) coord)
       (and (coord-valid? row col) (not (discovered? row col))))
     (list (cons (add1 row) col) (cons (sub1 row) col) (cons row (add1 col)) (cons row (sub1 col)))))

  (: trie TrieNode)
  (define trie
    (let ([trie (make-trie)])
      (for ([word words])
        (trie-insert! trie word))
      trie))

  (set->list
   (for*/fold : (Setof String)
              ([acc : (Setof String) (set)])
              ([row (in-range num-rows)]
               [col (in-range num-cols)]
               #:unless (discovered? row col))
     (let loop : (Setof String)
          ([acc : (Setof String) acc]
           [str : (Listof Char) '()]
           [trie : TrieNode trie]
           [row : Integer row]
           [col : Integer col])
       (discovered! row col)

       (define new-acc
         (if (TrieNode-is-word? trie)
             (set-add acc (list->string (reverse str)))
             acc))

       (define ch (matrix-ref board row col))
       (define new-trie-maybe (trie-query-char trie ch))
       (cond
         [(false? new-trie-maybe)
          (undo-discovered! row col)
          new-acc]
         [else
          (define new-str (cons ch str))
          (define result
            (for/fold
             ([acc : (Setof String) new-acc])
             ([coord : (Pair Integer Integer) (next-coords row col)])
              (loop acc new-str new-trie-maybe (car coord) (cdr coord))))
          (undo-discovered! row col)
          result])))))

(module+ test
  (require typed/rackunit)
  (: board (Pair (Listof Char) (Listof (Listof Char))))
  (define board
    '((#\o #\a #\a #\n)
      (#\e #\t #\a #\e)
      (#\i #\h #\k #\r)
      (#\i #\f #\l #\v)))
  (: words (Listof String))
  (define words
    '("eat" "pea" "oath" "rain"))

  (check-equal? (list->set (find-words board words)) (set "eat" "oath"))

  (check-equal? (list->set (find-words board '("an" "if" "take"))) (set "an" "if"))

  (check-equal? (list->set (find-words board '("oeiiflvrk" "ana"))) (set "oeiiflvrk"))
  (check-equal? (list->set (find-words board '("oateihkaa" "ana"))) (set "oateihkaa"))
  (check-equal? (list->set (find-words board '())) (set))

  (define board-small
    '((#\a #\b) (#\c #\d)))

  (define words-small
    '("abab"))

  (check-equal?
   (list->set (find-words board-small words-small)) (set)))
