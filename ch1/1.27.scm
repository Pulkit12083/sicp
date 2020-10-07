;; Carmichael Numbers (a few of them at least)
;; 561, 1105, 1729, 2465, 2821, 6601

;; Ex 1.28
;;miller rabin test for primality

;; modify expmod to signal when it has found a non trivial square congruent to modulo 1

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
  (isPrimeGuess p 2))


(millerRabinTest 561)  ;;#f
(millerRabinTest 1105) ;;#f
(millerRabinTest 1729) ;;#f
(millerRabinTest 2465) ;;#f
(millerRabinTest 2821) ;;#f
(millerRabinTest 6601) ;;#f
(millerRabinTest 7)    ;;#t
(millerRabinTest 3)    ;;#t
;;cannot be fooled


