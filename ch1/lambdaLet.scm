
;; (iterative sum)
(define (sumIter term a next b)
  (define (iter term a next b runningSum)
    (if (> a b) runningSum
	(iter term (next a) next b (+ runningsum (term a)))))
  (iter term a next b 0))

;; book iterative sum

(define (sumB term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

;; Integrals : )
;; For approximating pi/8:
;; Integ(a,b)(f) = [f(a+dx/2) + f(a+dx+dx/2) + f(a+2dx+dx/2)...]dx
;; Express procedurally nao:

(define (integral f a b dx)
  (* dx (sumB f
	      (+ a (/ dx 2.0))
	      (lambda (val) (+ val dx))
	      b)))

(integral cube 0 1 0.01)

;; Simpson's Rule

(define (simpIntegral f a b n)
  (define (sIntegInc x) (+ x (/ (- b a) n)))
  (define (coefficient x) (/ (- x a) (/ (- b a) n)))
  (define (fTerm x)
    (cond ((= (coefficient x) 0) (f x))
	  ((= (coefficient x) n) (f x))
	  ((= (remainder (coefficient x) 2) 1) (* 4.0 (f x)))
	  (else (* 2.0 (f x)))))
  (/ (* (- b a) (sum fTerm a sIntegInc b)) (* 3 n)))

(simpIntegral cube 0 1 100)



