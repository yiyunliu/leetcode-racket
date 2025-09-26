#lang racket

(module M typed/racket
  (provide add-strings)
  
  (: string->digits (-> String (Listof Integer)))
  (define (string->digits str)
    (reverse
     (map (lambda ([x : Char]) (- (char->integer x) (char->integer #\0)))
          (string->list str))))

  (: add-digits (-> (Listof Integer) (Listof Integer) (Listof Integer)))
  (define (add-digits digits1 digits2)
    (: loop (-> Integer (Listof Integer) (Listof Integer) (Listof Integer) (Listof Integer)))
    (define (loop carry digits1 digits2 acc)
      (let-values ([(digit1 digits1) (if (null? digits1) (values #f '()) (values (car digits1) (cdr digits1)))]
                   [(digit2 digits2) (if (null? digits2) (values #f '()) (values (car digits2) (cdr digits2)))])
        (if (and (zero? carry) (not digit1) (not digit2))
            acc
            (let* ([digit (+ (or digit1 0) (or digit2 0) carry)])
              (let-values ([(quot rem) (quotient/remainder digit 10)])
                (loop quot digits1 digits2 (cons rem acc)))))))
    (loop 0 digits1 digits2 '()))


  (: add-strings (-> String String String))
  (define (add-strings num1 num2)
    (list->string
     (map (lambda ([x : Integer]) (integer->char (+ x (char->integer #\0))))
          (add-digits (string->digits num1) (string->digits num2))))))

(require 'M)
