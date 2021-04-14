(define (linear-combination-simple a b x y)
  (+ (* a x) (* b y)))

(define (linera-combination-generic a b x y)
  (add (mul a x) (mul by )))


(define (make-rat n d) (cons n d))
(define (num r) (car r))
(define (den r) (cdr r))

(define (print-rat r)
  (display (num r))
  (display "/")
  (display (den r)))

;2.1
(define (make-rat a b)
  (if (< b 0)
      (cons (* -1 a) (*-1 b))
      (cons a b)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (seg-s s)
  (car s))
(define (seg-e s)
  (cdr e))

(define (mid-point s)
  (define (avg x y) (/ (+ x y) 2.0))
  (make-point (avg (x-point (seg-s s)) (x-point (seg-e s)))
	      (avg (y-point (seg-s s)) (y-point (seg-e s)))))



(define (rect1 s1 s2)
  (cons s1 s2))

(define (getseg1 rec)
  (car rec))
(define (getseg2 rec)
  (cdr rec))

;---------- data abstraction layer --------
;methods below don't need to know how rect or getseg1 or getseg2 are implemented.
;just that they fulfill the idea of returning two adjacent segments of a triangle.

(define (getlength s)
  (sqrt (+ (expt (- (x-point (seg-s s)) (x-point (seg-e s))) 2)
	   (expt (- (y-point (seg-s s)) (y-point (seg-e s))) 2))))
	      
(define (rect-peri rec)
  (* 2 (+ (getlength (getseg1 rec)) (getlength (getseg2 rec)))))

(define (rect-are rec)
  (* (getlength (getseg1 rec)) (getlength (getseg2 rec))))


  
;--------
					;2.5


(define (cons a b)
  (lambda (m) (m (* (expt 2 a) (expt 3 b)))))

(define (car pr)
  (pr power-of-2 ))

(define (cdr pr)
  (pr power-of-3))

(define (power-of-exp e num)
  (if (not (= (modulo num e) 0))
      0
      (+ 1 (power-of-exp e (/ num e)))))

(define (power-of-2 num)
  (power-of-exp 2 num))

(define (power-of-3 num)
  (power-of-exp 3 num))




;; mapping :)

;; of the general form:

(define (map proc items)
  (if (null? items)			;
      nil				; unbound variable nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))
;; (10 2.5 11.6 17)
(map (lambda (x) (* x x))
     (list 1 2 3 4))
;; (1 4 9 16)
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(scale-list (list 1 2 3 4 5) 5)
;; (5 10 15 20 25)
(scale-list (list 1 2 3 4 5) 10)
;; (10 20 30 40 50)

;; mapping over trees
(define nil '())
(define (scale-tree tree factor)
        ;; tree first condition is nil
  (cond ((null? tree) nil)
	;; tree second condition is not pair
        ((not (pair? tree)) (* tree factor))
	;; tree third condition is pair
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))


(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

;;(10 (20 (30 40) 50) (60 70))

;; recursive map over each tree list

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))


(map square (list 1 2 3 4 5))
;;(1 4 9 16 25)

;; filter on predicatae
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
