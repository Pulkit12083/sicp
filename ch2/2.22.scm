;; Exercise 2.22.  Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so that it evolves an iterative process:

;; This method appends the current list of squares after the next square thereby building backward
(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))

(square-list-1 (list 1 2 3 4 5 6))
;;;Value: (36 25 16 9 4 1)
;; this doesn't work either:
;; Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))
;; this builds pairs of current pair with next square like below

(square-list-2 (list 1 2 3 4 5 6))
;Value: ((((((() . 1) . 4) . 9) . 16) . 25) . 36)
