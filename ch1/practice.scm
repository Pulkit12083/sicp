
(define (fast-exp b n)
  (define (even? val) (= (remainder val 2) 0))
  (cond ((= n 0) 1)
	((even? n) (square (fast-exp b (/ n 2))))
	(else (* b (fast-exp b (- n 1))))))

(fast-exp 2 3)

(define (faster-exp base power)
  (define (even? val) (= (remainder val 2) 0))
  (define (exp-iter b n product)
    (cond ((= n 0) product)
	  ((= n 1) (* b product))
	  ((even? n) (exp-iter (* b b) (/ n 2) product))
	  (else (exp-iter b (- n 1) (* product b)))))
  (exp-iter base power 1))

(define (test-faster-exp b n)
  (= (fast-exp b n) (faster-exp b n)))

(test-faster-exp 13 4)



