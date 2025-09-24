#lang racket

(define (print-special-row s offset num-rows out)
  (let ([gap (* 2 (- num-rows 1))])
    (for ([i (in-range offset (string-length s) gap)])
      (display (string-ref s i) out))))

(define (convert s num-rows)
  (if (eq? num-rows 1)
      s
      (let ([out (open-output-string)]
            [gap (* 2 (- num-rows 1))])
        (print-special-row s 0 num-rows out)
        (for ([row (in-range 1 (sub1 num-rows))])
          (let* ([comp (- gap row)])
            (for ([i (in-range row (string-length s) gap)])
              (display (string-ref s i) out)
              (let ([comp-idx (+ (- i row) comp)])
                (when (< comp-idx (string-length s))
                  (display (string-ref s comp-idx) out))))))
        (print-special-row s (sub1 num-rows) num-rows out)
        (get-output-string out))))
