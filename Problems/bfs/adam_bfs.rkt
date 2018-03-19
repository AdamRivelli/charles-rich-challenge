#lang racket
(require graph)
(define-struct node (name nodes) #:transparent #:mutable)
(define-values (a b c d)
      (shared ([a (make-node "a" (list b c d))]
               [b (make-node "b" (list a c d))]
               [c (make-node "c" (list a b d))]
               [d (make-node "d" (list a b c))])
              (values a b c d)))

(define-values (g h j k l)
      (shared ([g (make-node "g" (list h k))]
               [h (make-node "h" (list l))]
               [j (make-node "j" (list g))]
               [k (make-node "k" (list g))]
               [l (make-node "l" (list g k j))])
              (values g h j k l)))


(define (run-bfs queue visited)
  (cond
    [(empty? queue)]
    [(member (first queue) visited) (run-bfs (rest queue) visited)]
    [else
     (begin
       (writeln (node-name (first queue)))
       (define new-queue (append queue (node-nodes (first queue))))
       (run-bfs (rest new-queue) (cons (first queue) visited)))]))

(run-bfs (list a) empty)
(run-bfs (list g) empty)
