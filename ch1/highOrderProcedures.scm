;; comparing high order procedures with naive procedures

;; I. naive implementations that don't use procedural abstractions

(define (nRangedSum a b)
  (if (> a b) 0
      (+ a (nRangedSum (+ a 1) b))))

(define (nRangedCube a b)
  (if (> a b) 0
      (+ (* a a a) (nRangedCube (+ a 1) b))))

;;Leibniz's summation for approximating pi/8:

(define (nPiSum a b)
  (if (> a b) 0
      (+ (/ 1.0 (* a (+ a 2))) (nPiSum (+ a 4) b))))

(nPisum 1 100) ;; .390199..

#| common pattern: Summation!
(define (<procedureName> a b)
  (if (<comparator> a b) <terminalValue>
      (+ (term a) (<procedureName> (next a) b))))
|#

;; term and next are procedures that are arguments to this higher order procedure
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))

;; redefine above methods now

(define (identity x) x)
(define (increment x) (+ x 1))
(define (rangedSum a b)
  (sum identity a increment b))
(define (cube x) (* x x x))
(define (rangedCube a b)
  (sum cube a increment b))

(define (piSum a b)
  (define (piTerm v) (/ 1.0 (* v (+ v 2))))
  (define (piInc v) (+ v 4))
  (sum piTerm a piInc b))

(piSum 1 100)

;; Integrals : )
;; For approximating pi/8:
;; Integ(a,b)(f) = [f(a+dx/2) + f(a+dx+dx/2) + f(a+2dx+dx/2)...]dx
;; Express procedurally nao:

(define (integral f a b dx)
  (define (integInc val) (+ val dx))
  (* dx (sum f (+ a (/ dx 2.0)) integInc b)))

(integral cube 0 1 0.01)

;; Simpson's Rule

(define (simpIntegral f a b n)
  (define (sIntegInc x) (+ x (/ (- b a) n)))
  (define (coefficient x) (/ (- x a) (/ (- b a) n)))
  (define (fTerm x)
    (cond ((= (coefficient x) 0) (f x))
	  ((= (coefficient x) n) (f x))
	  ((= (remainder (coefficient x) 2) 1) (* 4 (f x)))
	  (else (* 2.0 (f x)))))
  (/ (* (- b a) (sum fTerm a sIntegInc b)) (* 3 n)))

(simpIntegral cube 0 1 100)
