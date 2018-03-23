#lang racket
(require racket/draw)

(define-struct state (x y angle))

(define (repl-X) "F+[[X]-X]-F[-FX]+X")
(define (repl-F) "FF")
(define (repl-char c)
  (cond [(equal? c #\F) (repl-F)]
        [(equal? c #\X) (repl-X)]
        [else (string c)]))

(define plant-unit 3)
(define (get-angle)
  (+ 15 (random 20)))

(define (calc-end start)
  (define rad-angle (degrees->radians (state-angle start)))
  (make-state (+ (* plant-unit (cos rad-angle)) (state-x start))
              (+ (* plant-unit (sin rad-angle)) (state-y start))
              (state-angle start)))

(define (plant-step cur-step)
  (apply string-append (map repl-char (string->list cur-step))))

(define (nth-step n start next-proc)
  (cond [(= n 0) start]
        [else (nth-step (- n 1) (next-proc start) next-proc)]))

(define target (make-bitmap 10000 10000)) 
(define dc (new bitmap-dc% [bitmap target]))

(define (draw-step step stack)
  (_draw-step (string->list step) stack (make-state 5000 5000 270) target))

(define (_draw-step step stack start target)
  (cond [(empty? step) ]
        [else
         (cond [(equal? #\X (first step))
                (_draw-step (rest step) stack start target)]
               [(equal? #\F (first step)) 
                (define end (calc-end start))
                (send dc draw-line (state-x start) (state-y start)
                                   (state-x end) (state-y end))
                (_draw-step (rest step) stack end target)]
               [(equal? #\[ (first step))
                (_draw-step (rest step) (cons start stack) start target)]
               [(equal? #\] (first step))
                (_draw-step (rest step) (rest stack) (first stack) target)]
               [(equal? #\+ (first step))
                (_draw-step (rest step)
                            stack
                            (make-state
                             (state-x start)
                             (state-y start)
                             (- (state-angle start) (get-angle)))
                            target)]
               [(equal? #\- (first step))
                (_draw-step (rest step)
                            stack
                            (make-state
                             (state-x start)
                             (state-y start)
                             (+ (state-angle start) (get-angle)))
                            target)]
               )]))

(draw-step (nth-step 6 "X" plant-step) empty)
(send target save-file "adam_tree.png" 'png)
