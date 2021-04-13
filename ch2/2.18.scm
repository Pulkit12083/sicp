(define (reverse-list lst)
  (define (revlist l r)
    (if (null? l)
	r
	(revlist (cdr l) (cons (car l) r))))
  (revlist lst '()))
