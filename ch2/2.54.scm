;; 2.54


(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
	((and (not (pair? l1))
	      (not (pair? l2))) (eq? l1 l2))
	((and (pair? l1)
	      (pair? l2)) (and (equal? (car l1)
				       (car l2))
			       (equal? (cdr l1)
				       (cdr l2))))
	(else #f)))

(equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)) 
 ;Value: #t 

 
 (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6)) 
 ;Value: #f 
	      



(equal? '(this is a list) '(this is a list))


