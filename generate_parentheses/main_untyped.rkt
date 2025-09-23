#lang racket

(define (generate-parenthesis i)
  (map list->string (generate-parenthesis-rec i 0 0)))

(define (generate-parenthesis-rec i o-count c-count)
  (if (eqv? o-count i)
      (list (make-list (- i c-count) #\) ))
      (let ([oparens (map (lambda (str) (cons #\( str))
                          (generate-parenthesis-rec i (add1 o-count) c-count))])
        (if (> o-count c-count)
            (append oparens (map (lambda (str) (cons #\) str))
                                 (generate-parenthesis-rec i o-count (add1 c-count))))
            oparens))))
