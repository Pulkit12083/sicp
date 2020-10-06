;; Ex 1.27
;; Carmichael Numbers (a few of them at least)
;; 561, 1105, 1729, 2465, 2821, 6601

;; try fermat's test on each value a below n

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermatsTest p val)
    (= (expmod val p p) val))

(define (isPrimeGuess p counter)
  (cond ((= counter p) true)
	((fermatsTest p counter) (isPrimeGuess p (+ counter 1)))
	(else false)))

(define (fermatsTestStrict p)
  (isPrimeGuess p 2))

(fermatsTestStrict 561)  ;;#t
(fermatsTestStrict 1105) ;;#t
(fermatsTestStrict 1729) ;;#t
(fermatsTestStrict 2465) ;;#t
(fermatsTestStrict 2821) ;;#t
(fermatsTestStrict 6601) ;;#t
