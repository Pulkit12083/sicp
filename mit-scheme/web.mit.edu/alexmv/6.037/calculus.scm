#lang racket


"Part 1: Numerical integration"

"Problem 1: Integrating any function"

(define (integral func num-steps x1 x2)
    'your-code-here)

;; Test cases:

;; With only one step, the integral of y = x^2 from 3 to 5
;; should be 3^2 * 2 = 18
(integral (lambda (x) (expt x 2)) 1 3 5)
;; With two steps, we should get 3^2 + 4^2 = 25
(integral (lambda (x) (expt x 2)) 2 3 5)

"Problem 2: Area of a unit circle"

(define (approx-pi num-steps)
    'your-code-here)
(approx-pi 1)   ;; Should be 4
(approx-pi 2)   ;; Hopefully lower than 4
(approx-pi 600) ;; Right to the first two decimal places?

"Problem 3: Integrating with pieces of any shape"

(define (rectangle func x1 x2)
    'your-code-here)

(define (trapezoid func x1 x2)
    'your-code-here)

(define (integral-with piece func num-steps x1 x2)
    'your-code-here)

;; Write your own test cases.  Start with checking that calling
;; (integral-with rectangle ...) is the same as calling (integral ...)
;; Then check that (integral-with trapezoid ...) produces better answers
;; for a given num-steps than the same (integral-with rectangle ...)


"Problem 4: Better approximation of pi"

(define (better-pi num-steps)
    'your-code-here)

;; How many digits does (better-pi 600) get correct, compared to
;; the earlier (approx-pi 600) ?


"Part 2: Symbolic differentiation"

(define (deriv-constant wrt constant)
    0)


"Problem 5: Derivative of a variable"

(define (deriv-variable wrt var)
    'your-code-here)


"Problem 6: Calling the right function"

(define (derivative wrt expr)
    (cond
        ; your code here
        (else (error "Don't know how to differentiate" expr))))


"Problem 7: Derivative of a sum"

(define (deriv-sum wrt expr)
    'your-code-here)


"Problem 8: Derivative of a product"

(define (deriv-product wrt expr)
    'your-code-here)


"Problem 9: Additional testing"

; Additional test cases for 'derivative' go here.
