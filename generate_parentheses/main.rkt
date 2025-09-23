#lang typed/racket

(define-type Exp
  (U Paren Seq Sing))
(define-struct Sing () #:transparent)
(define-struct Paren ([e : Exp]) #:transparent)
(define-struct Seq ([e0 : Exp] [e1 : Exp]) #:transparent)

(: gen-paren (-> Integer (Listof Exp)))
(define (gen-paren i)
  (if (eqv? i 1)
      (list (Sing))
      (let ([parens (gen-paren (sub1 i))])
        ((inst append-map Exp Exp) (lambda ([e : Exp]) (list (Seq (Sing) e) (Seq e (Sing)) (Paren e) )) parens))))

(: render-paren-rec (-> Exp Output-Port Void))
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

(: render-paren (-> Exp String))
(define (render-paren e)
  (let ([out (open-output-string)])
    (render-paren-rec e out)
    (get-output-string out)))

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
