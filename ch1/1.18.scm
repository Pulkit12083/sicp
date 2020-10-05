#| improve multiplication
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
|#

(define (double number) (+ number number))
(define (halve number) (/ number 2))
(define (even? num) (= (remainder num 2) 0))

(define (fast-mult a b)
  (cond ((= b 0) 0)
	((= b 1) a)
	((even? b) (fast-mult (double a) (halve b)))
	(else (+ a (fast-mult a (- b 1))))))

(define (faster-multi a b)
  (define (mult-iter a b product)
    (cond ((= b 0) 0)
	  ((= b 1) (+ product a))
	  ((even? b) (mult-iter (double a) (halve b) product))
	  (else (mult-iter a (- b 1) (+ product a)))))
  (mult-iter a b 0))

(faster-multi 4 9)


