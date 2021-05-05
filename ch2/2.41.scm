(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
	  (accumulate op init (cdr sequence)))))

;; i love this lanuage
(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define nil '())

(define (unique-pairs n)
  (accumulate append
	      nil
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs-simple n)
  (flatmap (lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(unique-pairs-simple 5)


(define (unique-triples n)
  (accumulate append
	      nil
	      (map (lambda (i)
		     (map (lambda (j)
			    (map (lambda (k) (list i j k))
				 (enumerate-interval 1 (- j 1))))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))
			    
			  
		     
(unique-triples 5)

;; correct but bloated and awful.
;; possible reduction in code lines: using an abstraction for the combination of map and filter
;; using an abstraction for the combination of accumulate map and filter.
;; Complexity is O(n3) when it can be brought down to O(n2)

(define (unique-triples-2 n sumval)
  (define (triplet-adds-toval? trp)
    (= (+ (car trp) (car (cdr trp)) (car (cdr (cdr trp)))) sumval))
  (define (filter condit seq)
    (if (null? seq)
	'()
	(if (condit (car seq))
	    (cons (car seq) (filter condit (cdr seq)))
	    (filter condit (cdr seq)))))
  (accumulate append
	      nil
	      (map (lambda (i)
		     (accumulate append
				 nil
				 (map (lambda (j)
					(filter triplet-adds-toval?
						(map (lambda (k) (list i j k))
						     (enumerate-interval 1 (- j 1)))))
				      (enumerate-interval 1 (- i 1)))))
		   (enumerate-interval 1 n))))

(unique-triples-2 6 10)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))



;; filter is an inbuilt primitive
#|  (define (filter condit seq)
    (if (null? seq)
	'()
	(if (condit (car seq))
	    (cons (car seq) (filter condit (cdr seq)))
	    (filter condit (cdr seq)))))
|#
;; complexity is O(n3)
(define (unique-triples-3 n sumval)
  (define (triplet-adds-toval? trp)
    (= (+ (car trp) (car (cdr trp)) (car (cdr (cdr trp)))) sumval))
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(filter triplet-adds-toval?
				(map (lambda (k) (list i j k))
				     (enumerate-interval 1 (- j 1)))))
		      (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

;;making use of accumulate
(define (unique-triples-4 n sumval)
  (define (triplet-adds-toval? trp)
    (= (accumulate + 0 trp) sumval))
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(filter triplet-adds-toval?
				(map (lambda (k) (list i j k))
				     (enumerate-interval 1 (- j 1)))))
		      (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

;; O(n2)
;; used http://community.schemewiki.org/?sicp-ex-2.41 for reference
(define (unique-tripples-5 n sumval)
  (define (distinct-triple? tr)
    (let ((x (car tr))
	  (y (cadr tr))
	  (z (car (cdr (cdr tr)))))
      (and (> z x) (> z y)))) ; this ensures uniqueness because z will always remain the smaller value
  (filter distinct-triple?
	  (flatmap (lambda (i)
		     (flatmap (lambda (j)
				(list i j (- sumval (+ i j))))
			      (enumerate 1 (- i 1))))
		   (enumerate 1 n))))
		       
(unique-triples-4 6 10)


;; what about generalizing for k-sized tuples?
;; 1. generate unique tuples
;; 2. filter with accumulate sum check
;;
;; generating unique k sized tuples
;; not unique
(define (gen-tuples k n)
  (cond ((= k 0) (list nil))
	((< n k) nil)
	(else (accumulate append
			  nil
			  (map (lambda (i) (map (lambda (l) (append (list i) l))
						(gen-tuples (- k 1) (- n i))))
			       (enumerate-interval 1 (- n 1)))))))

(gen-tuples 4 7)
