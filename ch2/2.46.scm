;; tedious
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

((frame-coord-map a-frame) (make-vect 0 0))
(origin-frame a-frame)

;;Exercise 2.46.  

;; add-vect
(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2))
	     (+ (ycor-vect vec1) (ycor-vect vec2))))
;; sub-vect
(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1) (xcor-vect vec2))
	     (- (ycor-vect vec1) (ycor-vect vec2))))
;; scale-vect
(define (scale-vect sc vec2)
  (make-vect (* sc (xcor-vect vec2))
	     (* sc (ycor-vect vec2))))

;;make=vect
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
