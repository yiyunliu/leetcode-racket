#lang racket

(define/contract (min-remove-to-make-valid s)
  (-> string? string?)
  (define keep (make-vector (string-length s) #t))
  (define open-to-delete
    (for/fold
        ([open-parens '()])
        ([(ch idx) (in-indexed s)])
      (cond
        [(char=? ch #\( )
         (cons idx open-parens)]
        [(char=? ch #\) )
         (cond
           [(null? open-parens)
            (vector-set! keep idx #f)
            open-parens]
           [else
            (cdr open-parens)])]
        [else
         open-parens])))
  (for ([idx open-to-delete])
    (vector-set! keep idx #f))
  (list->string
   (for/list
       ([(ch idx) (in-indexed s)]
        #:when (vector-ref keep idx))
     ch)))
