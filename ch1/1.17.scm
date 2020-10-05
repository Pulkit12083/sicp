#| improve multiplication
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
|#

(define (double number) (+ number number))
(define (halve number) (/ number 2))

(define (fast-mult a b)
  (define (even? num) (= (remainder num 2) 0))
  (cond ((= b 0) 0)
	((= b 1) a)
	((even? b) (fast-mult (double a) (halve b)))
	(else (+ a (fast-mult a (- b 1))))))

(fast-mult 24 7)



