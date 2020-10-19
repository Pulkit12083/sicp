;;1.40
;; define cubic that uses newtons-method in the form (newtons-method (cubic a b c 1))
;; to approximate zeros of the cubic x^3 + ax^2 + bx + c



;;Newton's method
;; f(x) = x - g(x)/Dg(x)
;; Dg(x) = (g(x+dx) - g(x))/dx

(define (fixedPoint func guess precision)
  (let ((fixedVal (func guess)))
    (cond ((< (abs (- fixedVal guess)) precision) guess)
	  (else (fixedPoint func fixedVal precision)))))
(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixedPoint (newton-transform g) guess 0.0001))

(define (sqroot x)
  (newton-method (lambda (y) (- (square y) x)) 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Approximate zeros of cubic x^3 +ax^2 +bc+ c
;; use the fixed point
;; 1.40

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 4 3 8) 1)
((cubic 4 3 8) -3.76734)



