#lang racket

(module M typed/racket
  (provide climb-stairs)
  (: climb-stairs (-> Integer Integer))
  (define (climb-stairs n)
    (cond
      [(zero? n) 1]
      [(eqv? n 1) 1]
      [else
       (let-values ([(_ ret)
                     (for/fold
                         ([curr : Integer 1]
                          [next : Integer 2])
                         ([_ (in-range 0 (- n 2))])
                       (values next (+ curr next)))])
         ret)])))
(require 'M)
