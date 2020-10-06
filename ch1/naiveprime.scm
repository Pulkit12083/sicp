(define (isPrime? n)
  (define (primeIter ctr)
    (cond ((> (* ctr ctr) n) #t)
	  ((= (remainder n ctr) 0) #f)
	  (else (primeIter (+ ctr 1)))))
  (primeIter 2))

(isPrime? 43)


