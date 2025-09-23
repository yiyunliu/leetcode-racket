#lang racket

(define-struct Sing () #:transparent)
(define-struct Paren (e) #:transparent)
(define-struct Seq (e0 e1) #:transparent)

(define (gen-paren i)
  (if (eqv? i 1)
      (list (Sing))
      (let ([parens (gen-paren (sub1 i))])
        (append-map (lambda (e) (list (Seq (Sing) e) (Seq e (Sing)) (Paren e) )) parens))))

(define (render-paren-rec e out)
  (void (cond
          [(Sing? e)
           (write-string "()" out)]
          [(Paren? e)
           (write-string "(" out)
           (render-paren-rec (Paren-e e) out)
           (write-string ")" out)]
          [(Seq? e)
           (render-paren-rec (Seq-e0 e) out)
           (render-paren-rec (Seq-e1 e) out)])))

(define (render-paren e)
  (let ([out (open-output-string)])
    (render-paren-rec e out)
    (get-output-string out)))

(define (generate-parenthesis i)
  (sort (remove-duplicates (map render-paren (gen-paren i))) string<=?))
