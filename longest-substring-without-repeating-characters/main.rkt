#lang typed/racket

(define-type Table (Vectorof (U -1 Natural)))
(: make-ascii-table (-> Table))
(define (make-ascii-table)
  ((inst make-vector (U -1 Natural)) 128 -1))

(: find-char-and-set! (-> Table Char Natural (U -1 Natural)))
(define (find-char-and-set! tbl ch idx)
  (let* ([ch-num (char->integer ch)]
         [maybe-idx (vector-ref tbl ch-num)])
    (vector-set! tbl ch-num idx)
    maybe-idx))


(: update-max (-> Integer Natural Natural Integer))
(define (update-max acc start idx)
  (max (- idx start) acc))


(: find-longest-rec (-> String Natural Natural Table Integer Integer))
(define (find-longest-rec str start idx tbl acc)
  (if (>= idx (string-length str))
      (update-max acc start idx)
      (let* ([ch (string-ref str idx)]
             [maybe-dup-idx (find-char-and-set! tbl ch idx)])
        (if (>= maybe-dup-idx start)
            (find-longest-rec str (add1 maybe-dup-idx) (add1 idx) tbl (update-max acc start idx))
            (find-longest-rec str start (add1 idx) tbl acc)
))))

(: length-of-longest-substring (-> String Integer))
(define (length-of-longest-substring s)
  (find-longest-rec s 0 0 (make-ascii-table) 0))
