#lang typed/racket

(: print-special-row (-> String Integer Integer Output-Port Void))
(define (print-special-row s offset num-rows out)
  (let* ([space (- num-rows 2)]
         [gap (* 2 (- num-rows 1))]
         [sep (make-string space #\ )])
    (for ([i : Integer (in-range offset (string-length s) gap)])
      (when (> i offset)
        (display sep out))
      (display (string-ref s i) out))))

(: convert (-> String Natural String))
(define (convert s num-rows)
  (if (eq? num-rows 1)
      s
      (let ([out (open-output-string)]
            [num-space (- num-rows 2)]
            [gap (* 2 (- num-rows 1))])
        (print-special-row s 0 num-rows out)
        (for ([row : Natural (in-range 1 (sub1 num-rows))])
          (display #\newline out)
          (let* ([comp : Integer (- gap row)]
                 [space-snd (make-string (sub1 row) #\ )]
                 [space-fst (make-string (- num-space 1 (sub1 row)) #\ )])
            (for ([i : Integer (in-range row (string-length s) gap)])
              (when (> i row)
                (display space-snd out))
              (display (string-ref s i) out)
              (let ([comp-idx (+ (- i row) comp)])
                ;; (display "comp-idx:")
                ;; (print comp-idx)
                ;; (display "\ni:")
                ;; (print i)
                (when (< comp-idx (string-length s))
                  (display space-fst out)
                  (display (string-ref s comp-idx) out))))))
        (display #\newline out)
        (print-special-row s (sub1 num-rows) num-rows out)
        (get-output-string out))))
