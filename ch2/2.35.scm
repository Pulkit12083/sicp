;;Exercise 2.35.  Redefine count-leaves from section 2.2
;; relies on usage of enumerate
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate t)))) ; unbound variable enumerate

(define (count-leaves-rec t)
  (accumulate + 0
	      (map (lambda (x)
		     (cond ((null? x) 0)
			   ((pair? x) (count-leaves-rec x))
			   (else 1)))
		   t)))

(define tree (list 1 2 (list 3 4) (list 5 (list 6 7))))
(count-leaves-rec tree)
