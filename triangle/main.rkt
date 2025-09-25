#lang racket

(module M typed/racket
  (provide minimum-total)
  (define-struct (A) Slice ([vec : (Vectorof A)] [start : Integer] [end : Integer]))

  (: build-slice (All (A) (-> (Vectorof A) Integer Integer (Slice A))))
  (define (build-slice vec start end)
    (Slice vec start end))

  (: subslice (All (A) (-> (Slice A) Integer Integer (Slice A))))
  (define (subslice s start end)
    (Slice (Slice-vec s) (+ (Slice-start s) start) (+ (Slice-start s) end)))

  (: vector->slice (All (A) (-> (Vectorof A) (Slice A))))
  (define (vector->slice vec)
    (build-slice vec 0 (vector-length vec)))

  (: slice-ref (All (A) (-> (Slice A) Integer A)))
  (define (slice-ref s i)
    (vector-ref (Slice-vec s) (+ (Slice-start s) i)))

  (: slice-set! (All (A) (-> (Slice A) Integer A Void)))
  (define (slice-set! s i a)
    (vector-set! (Slice-vec s) (+ (Slice-start s) i) a))

  (: slice-size (All (A) (-> (Slice A) Integer)))
  (define (slice-size s)
    (- (Slice-end s) (Slice-start s)))

  (: slice-empty? (All (A) (-> (Slice A) Boolean)))
  (define (slice-empty? s)
    (>= (Slice-start s) (Slice-end s)))

  

  )

(require 'M)
(provide minimum-total)
