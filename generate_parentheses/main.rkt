#lang typed/racket

(: generate-parenthesis (-> Natural (Listof String)))
(define (generate-parenthesis i)
  (map list->string (generate-parenthesis-rec i 0 0)))

(: generate-parenthesis-rec (-> Natural Natural Natural (Listof (Listof Char))))
(define (generate-parenthesis-rec i o-count c-count)
  (if (eqv? o-count i)
      (list (make-list (- i c-count) #\) ))
      (let ([oparens (map (lambda ([str : (Listof Char)]) (cons #\( str))
                          (generate-parenthesis-rec i (add1 o-count) c-count))])
        (if (> o-count c-count)
            (append oparens (map (lambda ([str : (Listof Char)]) (cons #\) str))
                                 (generate-parenthesis-rec i o-count (add1 c-count))))
            oparens))))
