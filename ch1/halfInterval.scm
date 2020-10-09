;; find a zero of a function given a positive and a negative value
(define tolerance 0.00001)

(define (halfIntervalSearch func negPoint posPoint precision)
  (define (average a b) (/ (+ a b) 2.0))
  (let ((midPoint (average posPoint negPoint)))
    (let ((midValue (func midPoint)))
      (define (closeEnough?) (< (abs midValue) precision))
      (cond ((closeEnough?) midPoint)
	    ((< midValue 0) (halfIntervalSearch func midPoint posPoint precision))
	    ((> midValue 0) (halfIntervalSearch func negPoint midPoint precision))
	    (else midPoint)))))

(define (halfInterval func a b precision)
  (let ((nPoint (func a))
	(pPoint (func b)))
    (cond ((and (<= nPoint 0) (>= pPoint 0)) (halfIntervalSearch func a b precision))
	  ((and (>= nPoint 0) (<= pPoint 0)) (halfIntervalSearch func b a precision))
	  (else (error "incorrect values; numeric arguments do not have opposite signs")))))


;;test for x^3 - 2^x -3 between 1 and 0
(halfInterval (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0 0.001) ;;1.8933..


;; fixedpoint

(define (fixedPoint func guess precision)
  (let ((fixedVal (func guess)))
    (cond ((< (abs (- fixedVal guess)) precision) guess)
	  (else (fixedPoint func fixedVal precision)))))

(fixedPoint cos 1.0 tolerance)

;; 1.35
;; 1 + 1/x

(fixedPoint (lambda (x) (+ 1 (/ 1.0 x))) 1.6 tolerance)

(define (fixedPointDamped func guess precision)
  (define (average a b) (/ (+ a b) 2))
  (let ((fixedVal (func guess)))
    (cond ((< (abs (- fixedVal guess)) precision) guess)
	  (else (fixedPoint func (average fixedVal guess) precision)))))

(fixedPoint (lambda (x) (/ (log 1000) (log x))) 1.2 tolerance)

;; f (n d k) = N1/(D1 + (N2/(D2+ N3(D4..+Nk/Dk))))

;; reversing k indices for ease tbh
;; works only for procedures that reverrse n d
(define (contfracrev n d k)
  (cond ((= k 1) 0)
	(else (/ (n k) (+ (d k) (contfrac n d (- k 1)))))))

(define (contfracrec n d k)
  (define (iter i)
    (cond ((= i k) (/ (n i) (d i)))
	  (else (/ (n i) (+ (d i) (iter (+ i 1)))))))
  (iter 1))

(define (contfraciter n d k)
  (define (iter i result)
    (cond ((= i 0) result)
	  (else (iter (- i 1) (/ (n i) (+ (d i) result))))))
  (iter k 0))
	   
(contfracrec (lambda (i) 1.0)
	  (lambda (i) 1.0)
	  50)
