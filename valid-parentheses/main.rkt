#lang racket

(module M typed/racket
  (provide is-valid)
  (define-type
    LParens (U #\( #\{ #\[))

  (define lparens? (make-predicate LParens))

  (define-type
    RParens (U #\) #\} #\] ))

  (define rparens? (make-predicate RParens))


  (: closing-parens (-> LParens RParens))
  (define (closing-parens l)
    (case l
      ['#\(  #\) ]
      ['#\{  #\} ]
      ['#\[  #\] ]
      [else (error "impossible")]))

  (: is-valid (-> String Boolean))
  (define (is-valid s)
    (define result
      (for/fold
      ([stack : (U (Listof LParens) #f) '()])
      ([ch : Char s])
        #:break (false? stack)
        (cond
          [(lparens? ch) (cons ch stack)]
          [(rparens? ch)
           (if (null? stack)
               #f
               (and (eqv? (closing-parens (car stack)) ch)
                    (cdr stack)))]
          [else (error "invalid inputs")])))
    (null? result)))

(require 'M)
