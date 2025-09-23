#lang typed/racket

(define-type WordStat (U 'Neither 'Bad 'First 'Both))
(define-type Table (Mutable-HashTable String WordStat))

(: update-one (-> WordStat WordStat))
(define (update-one stat)
  (case stat
    ['Neither 'First]
    [else 'Bad]))

(: update-two (-> WordStat WordStat))
(define (update-two stat)
  (case stat
    ['First 'Both]
    [else 'Bad]))

(: count-words (-> (Listof String) (Listof String) Natural))
(define (count-words l0 l1)
  (let ([table : Table (make-hash)])
    (for ([s : String l0])
      (hash-ref! table s (lambda () 'Neither))
      (hash-update! table s update-one))
    (for ([s : String l1])
      (hash-ref! table s (lambda () 'Neither))
      (hash-update! table s update-two))
    (for/fold
        ([acc : Natural 0])
        ([v : WordStat (hash-values table)])
      (if (eq? v 'Both)
          (add1 acc)
          acc))))
