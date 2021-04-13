					;warmup append
(define (append-list lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append-list (cdr lst1) lst2))))

					; 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      (car lst)
      (last-pair (cdr lst))))
					;2.17 better
(define (last-pair-improved lst)
  (cond ((null? lst) (error "empty list"))
	((null? (cdr lst)) (car lst))
	(else (last-pair-improved (cdr lst)))))

					;2.18 
(define (reverse-list lst)
  (define (revlist l r)
    (if (null? l)
	r
	(revlist (cdr l) (cons (car l) r))))
  (revlist lst '()))

