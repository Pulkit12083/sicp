;;1.44


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



;; 1.41

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))
(((double (double double)) inc) 5) ;; 21


;; 1.42 implement composition

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6) ;;49


;; 1.43
;; nth application of a function f

(define (repeated-application f n)
  (lambda (x)
    (if (= n 1) (f x)
	(f ((repeated-application f (- n 1)) x)))))

(define (repeated f n)
  (lambda (x)
    (if (= n 1) (f x)
	((compose f (repeated f (- n 1))) x))))

((repeated-application square 2) 5)
((repeated square 2) 2)

((repeated inc 7) 4)
;; 1.44
;; smoothing

(define dx 0.00001)

(define (smoothFunc f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ (x dx)))) 3)))

(define (n-fold-smoothing f n)
  (repeated (smoothFunc f) n))

;;verify..

;; 1.45
(define (fixedPoint func guess precision)
  (let ((fixedVal (func guess)))
    (cond ((< (abs (- fixedVal guess)) precision) guess)
	  (else (fixedPoint func fixedVal precision)))))


(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (quadroot x)
  (fixedPoint (repeated (average-damp (lambda (y) (/ x (* y y y)))) 2) 1.0 0.0001))

(quadroot 256)

(define (fast-exp x n)
  (define (fast-exp-iter x n prod)
    (define is-even? (= (modulo n 2) 0))
    (cond ((= n 0) prod)
          ((= n 1) (* x prod))
          (else (if is-even?
                    (fast-exp-iter (* x x) (/ n 2) prod)
                    (fast-exp-iter x (- n 1) (* prod x))))))
  (fast-exp-iter x n 1))

;;1.45
(define (nThRoot x n test)
  (fixedPoint (repeated (average-damp (lambda (y) (/ x (fast-exp y (- n 1))))) (floor (logN n 2))  ) 1.0 0.00001))

(define (logN x y)
  (/ (log x) (log y)))

(logN 32 2)
(nThRoot 16 4 2) ;;5->4

