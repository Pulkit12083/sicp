;;2.8+2.9

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define a (mul-interval (make-interval -2 3) (make-interval 4 7)))


(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
		 (- (upper-bound a) (lower-bound b))))



;;2.9
; width is ARITHMETIC function of a b
(define (width-sum-interval a b)
  (/ (- (+ (upper-bound a) (upper-bound b))
	(+ (lower-bound a) (lower-bound b)))
     2))
; width function of widths of args

;w1 = [aH-aL/2]
;w2 = [bH-bL/2]
;w3 = [aH+bH]-[aL+bL]/2
;   = [aH-aL]+[bH-bL]/2
;   = w1+w2

(define (width int)
  (/ (- (upper-bound int) (lower-bound int)) 2))

(define (width-sum-interval-args int1 int2)
  (+ (width int1) (width int2)))

;---subtraction
; w3 = [aH-bL] - [aL-bH] /2
;    = [aH-aL] + [bH-bL] /2
;    =  w1 + w2 /2
;
;
; for multiplication not possible with just widths. 					
; because multipling different intervals with same widths reveals different width results
;  [-5, 5] * [-1, 1] = [-5, 5]   (width = 5)
;  [0, 10] * [0, 2] = [0, 20]   (width = 10)
;
;
;sicp provided

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;2.10
(define (div-interval-check x y)
  (if (or (= (upper-bound y) 0)
	  (= (lower-bound y) 0)
	  (= (width y) 0))
      (error "division of interval spanning zero OR bound is zero for divider" y)
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))





					; interface for end user electricians!
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


