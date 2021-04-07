;; 2.5

(define (exp x n)
  (if (= n 0) 1
      (* x (exp x (- n 1)))))

(define (cons a b)
  (lambda (m) (m (* (exp 2 a) (exp 3 b)))))

(define (find-power-of-2 num)
  (if (= (remainder num 2) 0)
      (+ 1 (find-power-of-2 (/ num 2)))
      0))

(define (find-power-of-3 num)
  (if (= (remainder num 3) 0)
      (+ 1 (find-power-of-3 (/ num 3)))
      0))

(define (car pr)
  (pr find-power-of-2))

(define (cdr pr)
  (pr find-power-of-3))

(define a (cons 3 7))

(car a)
(cdr a)


