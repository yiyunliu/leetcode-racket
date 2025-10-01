#lang typed/racket

(module M typed/racket
  (provide group-anagrams)
  (: group-anagrams (-> (Listof String) (Listof (Listof String))))
  (define (group-anagrams strs)

    (: ht (HashTable String (Listof String)))
    (define ht (make-hash))

    (for
    ([str : String strs])

      (define sorted-str (sort-string str))

      (hash-update! ht sorted-str
                    ;; prepend the new str to the list containing its anagrams
                    (lambda ([x : (Listof String)]) (cons str x))
                    ;; entry doesn't exist
                    (lambda () '())))

    (for/list ([(_ v) ht]) v))

  ;; (sort "bac") ~> "abc"
  (: sort-string (-> String String))
  (define (sort-string str)
    (list->string (sort (string->list str) char<?)))
  )

(require 'M)
