(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
	  (accumulate op init (cdr sequence)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (adjoin-position row k rest-queens)
  (cons (cons row k) rest-queens))

(define (safe? k positions)
  (accumulate (lambda (x y) (and x y))
	      #t
	      (map (lambda(x)
			 (and (not (= (car (car positions))
				      (car x)))
			      (not (= (- (car (car positions)) (- k (cdr x)))
				      (car x)))
			      (not (= (+ (car (car positions)) (- k (cdr x)))
				      (car x)))))
		       (cdr positions))))

(define nil '())
(define empty-board nil)

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 5)
;Value: (((3 . 4) (1 . 3) (4 . 2) (2 . 1)) ((2 . 4) (4 . 3) (1 . 2) (3 . 1)))
;; correct

