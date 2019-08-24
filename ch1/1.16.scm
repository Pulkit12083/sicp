#lang scheme

;fast exponent function
;linearly iterative process
(define (fast-exp x n)
  (define (fast-exp-iter x n prod)
    (define is-even? (= (modulo n 2) 0))
    (cond ((= n 0) prod)
          ((= n 1) (* x prod))
          (else (if is-even?
                    (fast-exp-iter (* x x) (/ n 2) prod)
                    (fast-exp-iter x (- n 1) (* prod x))))))
  (fast-exp-iter x n 1))