;; Exercise 2.21.  The procedure square-list takes a list of numbers as argument and returns a list of the squares of those numbers. 
;; (square-list (list 1 2 3 4))
;; (1 4 9 16)

(define (square-list lst)
  (map (lambda (x) (* x x)) lst))

(square-list (list 1 2 3 4))

(define (square-list-2 items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list-2 (cdr items)))))

(square-list-2 (list 1 2 3 4))
