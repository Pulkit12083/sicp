;; functions as return values

(define (fixedPoint func guess precision)
  (let ((fixedVal (func guess)))
    (cond ((< (abs (- fixedVal guess)) precision) guess)
	  (else (fixedPoint func fixedVal precision)))))

(fixedPoint cos 1.0 tolerance)
(fixedPoint (lambda (x) (+ 1 (/ 1.0 x))) 1.6 tolerance)

(define (fixedPointDamped func guess precision)
  (define (average a b) (/ (+ a b) 2))
  (let ((fixedVal (func guess)))
    (cond ((< (abs (- fixedVal guess)) precision) guess)
	  (else (fixedPoint func (average fixedVal guess) precision)))))

(fixedPoint (lambda (x) (/ (log 1000) (log x))) 1.2 tolerance)


(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
;; a procedure that accepts a function f and returns a function that takes a value and retusn the average of that value and the func of that value

((average-damp square) 10)

(define (sqrt x)
  (fixedPoint (average-damp (lambda (y) (/ x y))) 1.0 0.01))

(define (cube-root x)
  (fixedPoint (average-damp (lambda (y) (/ x (square y)))) 1.0 0.01))

(cube-root 27)
(sqrt 5)


;;Newton's method
;; f(x) = x - g(x)/Dg(x)
;; Dg(x) = (g(x+dx) - g(x))/dx

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixedPoint (newton-transform g) guess 0.001))

(define (sqroot x)
  (newton-method (lambda (y) (- (square y) x)) 1.0))

(sqroot 5)

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-damp f)
  (lambda (x) (- x (/ ((deriv f) x)

(define (fixed-point-of-transform g transform guess precision)
  (fixed-point (transform g) guess precision))
