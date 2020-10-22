;; 2.3 rectangle

(define (make-rectangle refPoint radian width length)
  (cons (cons refPoint radian)
	(cons width length)))

(define (rectangle-width rect)
  (car (cdr rect)))

(define (rectangle-height rect)
  (cdr (cdr rect)))

;; use
(define (rectangle-perimenter rect)
  (* 2 (+ (rectangle-width rect) (rectangle-height rect))))
(define (rectangle-area rect)
  (* (rectangle-width rect) (rectangle-height rect)))






	 
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
	      (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define p1 (make-point 1 2))
(define p2 (make-point 12 7))
(print-point (midpoint-segment (make-segment p1 p2)))
