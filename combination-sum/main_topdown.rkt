#lang racket

(define (combination->result comb)
  (for/fold ([acc '()])
            ([can-cnt comb])
    (match-define (cons can cnt) can-cnt)
    (append (make-list cnt can) acc)))

(define (combination-sum _candidates target)
  (define candidates (list->vector _candidates))
  (define result
    (let loop ([target target] [i 0])
      (cond
        [(= target 0) '(())]
        [(>= i (vector-length candidates)) '()]
        [else
         (define candidate (vector-ref candidates i))
         (let inner-loop ([n 0] [target target] [acc '()])
           (cond
             [(< target (* n candidate))
              acc]
             [else
              (let ([combs (loop (- target (* n candidate)) (add1 i))])
                (inner-loop (add1 n) target
                            (append
                             (map
                              (lambda (comb) (cons (cons candidate n) comb))
                              combs)
                             acc)))]))])))
  (map combination->result result))
