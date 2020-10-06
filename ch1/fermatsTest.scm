;;first compute (a^n)%n for (a,n)

;;naive
(define (expmodn a n)
  (define (faster-exp base power)
    (define (even? val) (= (remainder val 2) 0))
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

(define (isPrimeGuess p times)
  (cond ((= times 0) true)
	((fermatsTest p) (isPrimeGuess p (- times 1)))
	(else false)))
(define (smallest-divisor n)
  (define (find-divisor test)
    (cond ((> (square test) n) n)
	  ((= (remainder n test) 0) test)
	  (else (find-divisor (+ test 1)))))
  (find-divisor 2))

(smallest-divisor 199)   ;;199
(smallest-divisor 1999)  ;;1999
(smallest-divisor 19999) ;;7
