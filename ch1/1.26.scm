;; 1.26
;; applicative order evaluation ensures that both arguments to (* (expmod base (/ exp 2)) (expmod base (/ exp 2)) ) are evaluated separately.
;; therefore the result of one half computation is not being used for other half which is computed again. the problem is split in two and both are solved.
;; linear time is obvious.

;;first compute (a^n)%n for (a,n)

;;naive
hello what
(define (even? val) (= (remainder val 2) 0))

(define (expmodn a n)
  (define (faster-exp base power)
    (define (exp-iter b n product)
      (cond ((= n 0) product)
	    ((= n 1) (* b product))
	    ((even? n) (exp-iter (* b b) (/ n 2) product))
	    (else (exp-iter b (- n 1) (* product b)))))
    (exp-iter base power 1))
  (remainder (faster-exp a n) n))

(expmodn 4 7)


;; (ab)%m = ((a%m)(b%m))%m
;; avoid numbers much larger than m

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermatsTest p)
  (define (check val)
    (= (expmod val p p) val))
  (check (+ 1 (random (- p 1)))))

(define (fast-prime? p times)
  (cond ((= times 0) true)
	((fermatsTest p) (fast-prime? p (- times 1)))
	(else false)))


(define (next test-divisor)
  (if (= test-divisor 2) 3 (+ test-divisor 2)))

(define (smallest-divisor n)
  (define (find-divisor test)
    (cond ((> (square test) n) n)
	  ((= (remainder n test) 0) test)
	  (else (find-divisor (next test)))))
  (find-divisor 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))


(define (start-prime-test n start-time)
  (if (fast-prime? n 10000)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  true)


(define (search-for-primes start-range end-range)
  (cond ((>= start-range end-range) 0) 
	((= (remainder start-range 2) 0) (search-for-primes (+ 1 start-range) end-range))
	((timed-prime-test start-range) start-range)
	((search-for-primes (+ 2 start-range) end-range))))
  
(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 1000 10000)) 10000)) 10000)
#|
(+ 1 test-divisor)
1009 *** 0.
1013 *** 0.
1019 *** 0.

(next test-divisor)
1009 *** 0.
1013 *** 0.
1019 *** 0.
|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 10000 100000)) 100000)) 100000)
#|
(+ 1 test-divisor)
10007 *** 0.
10009 *** 0.
10037 *** 0.

(next test-divisor)
10007 *** 0.
10009 *** 0.
10037 *** 0.
|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 100000 1000000)) 1000000)) 1000000)
#|
(+ 1 test-divisor)
100003 *** 0.
100019 *** 0.
100043 *** 0.

(next test-divisor)
100003 *** 0.
100019 *** 1.0000000000005116e-2
100043 *** 0.
|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 1000000 10000000)) 10000000)) 10000000)
#|
(+ 1 test-divisor)
1000003 *** 0.
1000033 *** 0.
1000037 *** 0.

(next test-divisor)
1000003 *** 0.
1000033 *** 1.0000000000005116e-2
1000037 *** 0.
|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 10000000 100000000)) 100000000)) 100000000)
#|
(+ 1 test-divisor)
10000019 *** 0.
10000079 *** 1.0000000000005116e-2
10000103 *** 0.

(next test-divisor)
10000019 *** 1.0000000000005116e-2
10000079 *** 0.
10000103 *** 9.999999999990905e-3
|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 100000000 1000000000)) 1000000000)) 1000000000)
#|
(+ 1 test-divisor)
100000007 *** .00999999999999801
100000037 *** .00999999999999801
100000039 *** 1.0000000000005116e-2

(next test-divisor)
100000007 *** 9.999999999990905e-3
100000037 *** 1.0000000000005116e-2
100000039 *** 1.0000000000005116e-2
|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 1000000000 10000000000)) 10000000000)) 10000000000)
#|
(+ 1 test-divisor)
1000000007 *** .05000000000000426
1000000009 *** .01999999999999602
1000000021 *** 3.0000000000001137e-2
(next test-divisor)
1000000007 *** .04999999999999716
1000000009 *** .01999999999999602
1000000021 *** .01999999999999602

|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 10000000000 100000000000)) 100000000000)) 100000000000)
#|
(+ 1 test-divisor)
10000000019 *** 6.0000000000002274e-2
10000000033 *** .07000000000000028
10000000061 *** .07000000000000028

(next test-divisor)
10000000019 *** .07000000000000739
10000000033 *** .06999999999999318
10000000061 *** 6.0000000000002274e-2
|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 100000000000 1000000000000)) 1000000000000)) 1000000000000)
#|
(+ 1 test-divisor)
100000000003 *** .21999999999999886
100000000019 *** .21000000000000085
100000000057 *** .21000000000000085

(next test-divisor)
100000000003 *** .23000000000000398
100000000019 *** .20999999999999375
100000000057 *** .21999999999999886

|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 1000000000000 10000000000000)) 10000000000000)) 10000000000000)
#|
(+ 1 test-divisor)
1000000000039 *** .6700000000000017
1000000000061 *** .6700000000000017
1000000000063 *** .6700000000000017

(next test-divisor)
1000000000039 *** .6699999999999875
1000000000061 *** .6700000000000017
1000000000063 *** .6700000000000017
|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 10000000000000 100000000000000)) 100000000000000)) 100000000000000)
#|
(+ 1 test-divisor)
10000000000037 *** 2.1199999999999974
10000000000051 *** 2.1199999999999974
10000000000099 *** 2.1099999999999994

(next test-divisor)
10000000000037 *** 2.1200000000000045
10000000000051 *** 2.1199999999999903
10000000000099 *** 2.1200000000000045
|#

(search-for-primes (+ 1 (search-for-primes (+ 1 (search-for-primes 100000000000000 1000000000000000)) 1000000000000000)) 1000000000000000)
#|
(+ 1 test-divisor)
100000000000031 *** 6.670000000000002
100000000000067 *** 6.680000000000007
100000000000097 *** 6.679999999999993
2
(next test-divisor)
100000000000031 *** 6.700000000000003
100000000000067 *** 6.709999999999994
100000000000097 *** 6.679999999999993
|#

;;expectation or O(sqrt(n)) confirmed
;;expectation for time halving on skipping even numbers not confirmed
;; reason: likely because (next) computation replaces the remainder computation for even n
;; logn growth for fast-prime confirmed. 
