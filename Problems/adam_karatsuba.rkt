#!/usr/bin/racket
#lang racket

(define (mag num)
  (cond [(< num 10) 1]
        [else (+ 1 (mag (quotient num 10)))]))

(define (getL num) 
  (quotient num (expt 10 (quotient (mag num) 2))))

(define (getR num)
  (modulo num (expt 10 (quotient (mag num) 2))))


(define (mult x y)
  (let ([xL (getL x)]
        [xR (getR x)]
        [yL (getL y)]
        [yR (getR y)])
           (cond [(not (or (<= (mag xR) 3) (<= (mag yR) 3)))
              (let
                ([A (mult (+ xL xR) (+ yL yR))]
                 [B (mult xL yL)]
                 [C (mult xR yR)])
                (+ (* B (expt 10 (mag x)))
                   (* A (expt 10 (/ (mag x) 2)))
                   C))]
                 [else (* x y)])))

(display (mult 10 10))

(define (test-mult x y)
  (= (* x y) (mult x y)))