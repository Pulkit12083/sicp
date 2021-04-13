;; 2.28 (flatten tree into list)

(define (fringe tr)
  (cond ((null? tr) tr)
	((not (pair? tr)) (list tr))
	(else (append (fringe (car tr)) (fringe (cdr tr))))))

(define x (list (list 1 2) (list 3 4)))



(append x x '())
(fringe x)
;;(1 2 3 4)

(fringe (list x x))
;;(1 2 3 4 1 2 3 4)

(fringe (list x x x (list x x)))

