#lang scheme
#|
Newtonian cube root: Improve guess by:
((x/(y^2))+2y)/3)
|#
(define (improve-cbrt-guess x guess)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? x guess precision)
  (>= precision (abs (- x (* guess guess)))))     

(define (cbrt-iter x precision guess (old-guess 0))
  (if (<= (abs (- guess old-guess)) precision)
      guess
      (cbrt-iter x precision (improve-cbrt-guess x guess) guess)))

(define (cbrt x (precision 0.001) (guess 0.5))
  (cbrt-iter x precision guess))