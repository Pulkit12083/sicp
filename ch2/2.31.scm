;; Exercise 2.30.  Define a procedure square-tree analogous to the square-list procedure of exercise 2.21. That is, square-list should behave as follows:

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))


(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;;(1 (4 (9 16) 25) (36 49))

;;Define square-tree both directly (i.e., without using any higher-order procedures) and also by using map and recursion. 
;; without higher order procedures
(define (square-tree-simple tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree-simple (car tree))
		    (square-tree-simple (cdr tree))))))

(square-tree-simple
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;;2.31
(define (tree-map proc tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (proc tree))
	(cons (tree-map proc (car tree)) (tree-map proc (cdr tree)))))
  
(define (square-tree tree) (tree-map square tree))

