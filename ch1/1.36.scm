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

;; 1.36

(fixedPoint (lambda (x) (/ (log 1000) (log x))) 1.2 tolerance)

