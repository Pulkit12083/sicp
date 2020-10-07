;; filtered-accumulate!

;; build generate accumulate with combine
(define (identity x) x)
(define (incByOne x) (+ x 1))

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
	  (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
	  (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (filtered-accumulate-iter combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
	result
	(if (filter a)
	    (iter (next a) (combiner (term a) result))
	    (iter (next a) result))))
  (iter a null-value))

(define (even? x) (= (remainder x 2) 0))

(filtered-accumulate-iter addit 0 identity 1 incByOne 10 even?) ;; 2+4+6+8+10 = 30


;; implement sum of primes within a range


(define (sum-squared-primes a b)
  (filtered-accumulate-iter addit 0 square a incByOne b prime?))

(sum-squared-primes 1 7) ;;87

;; implement product of coprimes (product of all i<n st GCD(i,n)=1



(define (product-coprimes n)
  (define (gcd a b)
    (if (= b 0) a
	(gcd b (remainder a b))))
  (define (co-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate-iter mult 1 identity 1 incByOne (- n 1) co-prime?))

(product-coprimes 5);24

(define (next ctr)
  (if (= ctr 2) 3 (+ ctr 2)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((and (not (= base (- m 1))) (= (remainder (square base) m) 1)) 0)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermatsTest p val)
    (= (expmod val (- p 1) p) 1))

(define (isPrimeGuess p counter)
  (cond ((= counter p) true)
	((fermatsTest p counter) (isPrimeGuess p (next counter)))
	(else false)))

(define (millerRabinTest p)
  (if (= p 1) #f (isPrimeGuess p 2)))

(define (prime? p)
  (millerRabinTest p))

(prime? 3)


;;recursive accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;;iterative accumulate
(define (accumulateIter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (mult a b) (* a b))
(define (rangedProduct a b)
  (accumulate mult 1 identity a incByOne b))

(define (factorial n)
  (rangedProduct 1 n))

(factorial 5)

(define (addit a b) (+ a b))

(define (rangedSum a b)
  (accumulateIter addit 0 identity a incByOne b))

(rangedSum 1 4)

(define (accumulate 
;; recursive procedure for product
(define (productRec term a next b)
  (if (> a b) 1 (* (term a) (productRec term (next a) next b))))

(define (incVal x) (+ x 2))

(define (productItr term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (* result (term a)))))
  (iter a 1))

;; approximating pi/4
(define (pi4IntegRec n)
  (define (inc x) (+ x 2.0))
  (/ (* 8 (/ (productRec square 4.0 inc (* 2 n)) (productRec square 3.0 inc (- (* 2 n) 1)))) (* 2 n)))


(pi4IntegRec 15)
