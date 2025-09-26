#lang racket

(module M typed/racket
  (provide empty-buffer insert-at delete-at
           move-cursor-left move-cursor-right
           read-at)
  (require racket/treelist)

  (define-type Buffer (TreeListof Char))

  (: empty-buffer Buffer)
  (define empty-buffer
    (treelist))

  (: insert-at (-> Buffer Integer String (Values Buffer Integer)))
  (define (insert-at buffer cursor str)
    (for/fold ([buffer : Buffer buffer]
               [cursor : Integer cursor])
              ([ch : Char str])
      (let ([new-cursor (add1 cursor)])
        (if (index? cursor)
            (values (treelist-insert buffer cursor ch) new-cursor)
            (error "index out of bound")))))

  (: delete-at (-> Buffer Integer Integer (Values Buffer Integer)))
  (define (delete-at buffer cursor cnt)
    (let ([del-count (min cursor cnt)])
      (values
       (for/fold ([buffer : Buffer buffer])
                 ([i (in-range 0 del-count)])
         (let ([new-cursor (- cursor i 1)])
           (if (index? new-cursor)
               (treelist-delete buffer new-cursor)
               (error "index out of bound"))))
       (- cursor del-count))))

  (: read-at (-> Buffer Integer String))
  (define (read-at buffer cursor)
    (let ([start (max 0 (- cursor 10))])
      (build-string (- cursor start)
                    (lambda ([x : Integer])
                      (let ([y (+ start x)])
                        (if (index? y)
                            (treelist-ref buffer y)
                            (error "index out of bound")))))))

  (: move-cursor-right (-> Buffer Integer Integer Integer))
  (define (move-cursor-right buffer cursor offset)
    (let ([len (treelist-length buffer)])
      (min len (+ cursor offset))))


  (: move-cursor-left (-> Integer Integer Integer))
  (define (move-cursor-left cursor offset)
    (max 0 (- cursor offset))))

(require 'M)


(define text-editor%
  (class object%
    (super-new)
    
    (init-field)

    (define buffer empty-buffer)
    (define cursor 0)
    
    ; add-text : string? -> void?
    (define/public (add-text text)
      (let-values ([(new-buffer new-cursor) (insert-at buffer cursor text)])
        (set! buffer new-buffer)
        (set! cursor new-cursor)))

    ; delete-text : exact-integer? -> exact-integer?
    (define/public (delete-text k)
      (let-values ([(new-buffer new-cursor) (delete-at buffer cursor k)])
        (let ([ret (- cursor new-cursor)])
          (set! buffer new-buffer)
          (set! cursor new-cursor)
          ret)))

    ; cursor-left : exact-integer? -> string?
    (define/public (cursor-left k)
      (set! cursor (move-cursor-left cursor k))
      (read-at buffer cursor))

    ; cursor-right : exact-integer? -> string?
    (define/public (cursor-right k)
      (set! cursor (move-cursor-right buffer cursor k))
      (read-at buffer cursor))))

;; Your text-editor% object will be instantiated and called as such:
;; (define obj (new text-editor%))
;; (send obj add-text text)
;; (define param_2 (send obj delete-text k))
;; (define param_3 (send obj cursor-left k))
;; (define param_4 (send obj cursor-right k))
