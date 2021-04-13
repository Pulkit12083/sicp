;; 2.29

(define (make-mobile left right)
  (list left right))



(define (make-branch length structure)
  (list length structure))
;;a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))


;;b
(define (total-weight mobile)
  (cond ((null? mobile) 0)
	((not (pair? mobile)) mobile)
	(else (+ (total-weight (branch-structure (left-branch mobile)))
		 (total-weight (branch-structure (right-branch mobile)))))))
;;c      
(define (balanced mobile)
  (cond ((null? mobile) #t)
	((not (pair? mobile)) #t)
	(else (and (= (* (branch-length (left-branch mobile))
			 (total-weight (branch-structure (left-branch mobile))))
		      (* (branch-length (right-branch mobile))
			 (total-weight (branch-structure (right-branch mobile)))))
		   (balanced (branch-structure (left-branch mobile)))
		   (balanced (branch-structure (right-branch mobile)))))))

(define a (make-mobile (make-branch 2 3) (make-branch 2 3))) 
(total-weight a) ;; 6 

 (define m1 (make-mobile 
             (make-branch 4 6) 
             (make-branch 5 
                          (make-mobile 
                           (make-branch 3 7) 
                           (make-branch 9 8))))) ; 21 #f

 (define m2 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4))))); 18 #t

(total-weight m1) ; correct
(balanced m1) ; correct
(balanced m2) ; correct


;;d for cons - nothing but the following changes
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))
