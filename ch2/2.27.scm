;; 2.27
(define (reverse-list lst)
  (define (revlist l r)
    (if (null? l)
	r
	(revlist (cdr l) (cons (car l) r))))
  (revlist lst '()))

(define (deep-reverse l)
  (if (not (pair? l))
      l
      (map deep-reverse (reverse-list l))))



(define x (list (list 1 2) (list 3 4)))

(reverse x)

(deep-reverse x)

;;((4 3) (2 1))
