; NOT COMPLETE
#lang racket
(require dyoo-while-loop)

(define (is-in-list list value)
 (cond
  [(empty? list) false]
  [(equal? (first list) value) true]
  [else (is-in-list (rest list) value)]))

(define (bfs tree)
    (define A '("a"))
    (define explored '())
    (define v 0)
    (while (not (empty? A))
        (set! v (first A))
        (display A) (display "\n")
        (set! A (rest A))
        (if (not(is-in-list explored v))
            (and 
                (set! explored (cons v explored))
                (set! A (append A (hash-ref tree v)))
            )
            (break)
        )
    )
)
(define alphabet '("a" "b" "c" 'd 'e 'f 'g 'h 'i 'j 'k 'l))

(define (run-bfs adj_list)
    (define t (make-hash))
    (define i 0)
    (while (< i (length adj_list))
        (hash-set! t (list-ref alphabet i) (list-ref adj_list i))
        (set! i (add1 i))
    )
    (bfs t)
)

(define adj_list1 (list (list "b" "c") (list "a" "c") (list "a" "b")))

(run-bfs adj_list1)
