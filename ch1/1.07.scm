#lang scheme
#|
For small input values, our precision might be too large
to allow for a good enough guess.
For large input values, we might fall into an infinite loop
since our guess doesn't improve over iterations due to
its trailing floating point value being truncated due to
how lisp stores large float values (apparently) (4.2345e10)

The following is an implementation that improves the guess
and halts when the change in the guess is small enough
|#
(define (improve-sqrt-guess x guess)
  (/ (+ guess (/ x guess)) 2))

(define (good-enough? x guess precision)
  (>= precision (abs (- x (* guess guess)))))     

(define (sqrt-iter x precision guess (old-guess 0))
  (if (<= (abs (- guess old-guess)) precision)
      guess
      (sqrt-iter x precision (improve-sqrt-guess x guess) guess)))

(define (sqrt x (precision 0.1) (guess 1.0))
  (sqrt-iter x precision guess))