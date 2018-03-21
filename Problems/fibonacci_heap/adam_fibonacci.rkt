#lang racket

(define-struct node (value marked? parent children) #:transparent #:mutable)
(define-values (a b c d e)
      (shared ([a (make-node 0 #f empty (list b))]
               [b (make-node 1 #f a empty)]
               [c (make-node 2 #f empty empty)]
               [d (make-node 3 #f empty (list e))]
               [e (make-node 4 #f d empty)])
              (values a b c d e)))
(define h1 (list a c d))
;; a fheap is a list of nodes

(define (find-minimum fheap)
  (first (sort fheap #:key node-value <)))

(define (merge fheap1 fheap2)
  (sort (append fheap1 fheap2) #:key node-value <))

(define (insert fheap new-node)
  (merge fheap (list new-node)))

(define (delete-min fheap)
  (consolidate (append (node-children (first fheap)) (rest fheap))))

(define (size tree)
  (define children (node-children tree))
  (cond [(empty? children) 1]
        [else (+ 1 (foldl max (size (first children)) (map size children)))]))

(define (consolidate fheap)
  (cond [(no-dups (map size fheap)) fheap]
        [(member (size (first fheap)) (map size (rest fheap)))
         (define fsize (size (first fheap)))
         (consolidate
          (cons (heap-merge (first fheap) (findf (lambda (x) (= (size x) fsize) (rest fheap)))
                (remove fsize (rest fheap) (lambda (x y) (= x (size y)))))))]
        [else (cons (first fheap) (consolidate (rest fheap)))]))

(define (heap-merge heap1 heap2)
  (cond [(<= (node-value heap1) (node-value heap2))
         (struct-copy node heap1
                      [children (cons
                                 (struct-copy node heap2
                                              [parent heap1])
                                              (node-children heap1))])]
        [else (heap-merge heap2 heap1)]))

(define (no-dups a-list)
  (cond [(empty? a-list) #t]
        [else (and (not (member (first a-list) (rest a-list)))
                   (no-dups (rest a-list)))]))

(define (preorder tree)
  (cond [(empty? (node-children tree)) (displayln (node-value tree))]
        [else
         (displayln (node-value tree))
         (map preorder (node-children tree))]))

(not (no-dups (list 1 2 3 4 3)))
(no-dups (list 1 2 3 4))
(heap-merge a d)