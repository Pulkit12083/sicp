(define (square n) (* n n))

(define (sum-squares n1 n2) (+ (square n1) (square n2)))

(define (larger-sum-squares n1 n2 n3)
  (cond ((and (<= n1 n2) (<= n1 n3)) (sum-squares n2 n3))
	((and (<= n2 n1) (<= n2 n3)) (sum-squares n1 n3))
	(else (sum-squares n1 n2))))
