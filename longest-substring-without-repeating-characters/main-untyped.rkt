#lang racket

(require racket/unsafe/ops)

(define (make-ascii-table)
  (make-vector 128 -1))

(define (find-char-and-set! tbl ch idx)
  (let* ([ch-num (char->integer ch)]
         [maybe-idx (vector-ref tbl ch-num)])
    (vector-set! tbl ch-num idx)
    maybe-idx))


(define (update-max acc start idx)
  (max (unsafe-fx- idx start) acc))


(define (find-longest-rec str start idx tbl acc)
  (if (>= idx (string-length str))
      (update-max acc start idx)
      (let* ([ch (string-ref str idx)]
             [maybe-dup-idx (find-char-and-set! tbl ch idx)])
        (if (unsafe-fx>= maybe-dup-idx start)
            (find-longest-rec str (unsafe-fx+ 1 maybe-dup-idx) (unsafe-fx+ 1 idx) tbl (update-max acc start idx))
            (find-longest-rec str start (unsafe-fx+ 1 idx) tbl acc)
))))

(define (length-of-longest-substring s)
  (find-longest-rec s 0 0 (make-ascii-table) 0))
