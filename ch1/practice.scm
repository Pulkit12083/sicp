#lang scheme

(define (improve-sqrt-guess x guess)
  (/ (+ guess (/ x guess)) 2))

(define (good-enough? x guess precision)
  (>= precision (abs (- x (* guess guess)))))     

(define (sqrt x (precision 0.1) (guess 1.0))
  (if (good-enough? x guess precision)
      guess
      (sqrt x precision (improve-sqrt-guess x guess))))