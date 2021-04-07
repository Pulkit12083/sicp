(define (make-int a b)
  (if (<= a b)
      (cons a b)
      (cons b a)))
(define (lower-bound int)
  (car int))
(define (upper-bound int)
  (cdr int))

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (pos? a) (>= a 0))

(define (posint? int) (and (pos? (car int)) (pos? (cdr int))))
(define (negint? int) (and (not (pos? (car int))) (not (pos? (cdr int)))))
(define (mixint? int) (and (not (pos? (car int))) (pos? (cdr int))))

(define (mul x y)
  (cond ((and (posint? x) (posint? y)) (make-int (* (car x) (car y)) (* (cdr x) (cdr y))))
	((and (posint? x) (negint? y)) (make-int (* (cdr x) (car y)) (* (car x) (cdr y))))
	((and (posint? x) (mixint? y)) (make-int (* (cdr x) (car y)) (* (cdr x) (cdr y))))
	((and (negint? x) (posint? y)) (make-int (* (car x) (cdr y)) (* (cdr x) (car y))))
	((and (negint? x) (negint? y)) (make-int (* (cdr x) (cdr y)) (* (car x) (car y))))
	((and (negint? x) (mixint? y)) (make-int (* (car x) (cdr y)) (* (cdr x) (car y))))
	((and (mixint? x) (posint? y)) (make-int (* (car x) (cdr y)) (* (cdr x) (cdr y))))
	((and (mixint? x) (negint? y)) (make-int (* (cdr x) (cdr y)) (* (car x) (car y))))
	((and (mixint? x) (mixint? y)) (make-int (min (* (car x) (cdr y)) (* (cdr x) (car y)))
						 (max (* (car x) (car y)) (* (cdr x) (cdr y)))))))


(define (make-center-percent center tolerance) ;; assuming non negative center
  (make-int (- center (* tolerance center))
	    (+ center (* tolerance center))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (intolerance i)
  (/ (width i) (center i)))

					; 2.13

					; (x+dx)*(y+dy) = xy+xdy+ydx+dxdy = xy + xy*(dxdy/xy + dy/y + dx/x)
					;                                 = xy + xy*(t1+t2 + t1t2)
					; (x-dx)*(y-dy) = xy-xdy-ydx+dxdy = xy - xy*(t1+t2 - t1t2)
					; for small enough t1 and t2 we cna ignore t1t2 and conclude tolerance as (t1+t2)



; 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

					; A: [20 22]
					; B [2 4]
					; r1r2/r1+r2, 1/1/r1+1/r2:
					; A/A: [20/22 22/20] -> [0.9 1.1]
					; A/B: [5 11]
					; B/B: [0.5 2]
					; par1: [88 40]/[22 26] = [1.5 4]
					; par2: 1/(1/[20 22] +1/[2 4]) = 1/([.04 .06]+[.25 .5])
					;       1/[.29 .56]
					;       [1.78 3.44] != [1.5 4]
					; lem e tweakit is right

					; 2.15 she is right because you don't reintroduce identity errors
					; 2.16 different algebraic expressions may compound errors differently
					; to avoid this idk. maybe not?




