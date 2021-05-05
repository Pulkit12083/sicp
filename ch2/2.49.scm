;;2.46
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


;; 2.47

;;part A
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame a-frame)
  (car a-frame))

(define (edge1-frame a-frame)
  (car (cdr a-frame)))

(define (edge2-frame a-frame)
  (car (cdr (cdr a-frame))))

;; part B
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame a-frame)
  (car a-frame))

(define (edge1-frame a-frame)
  (car (cdr a-frame)))

(define (edge2-frame a-frame)
  (cdr (cdr a-frame)))


;; 2.48
;; make-segment
(define (make-segment x1 y1 x2 y2)
  (cons (make-vect x1 y1)
	(make-vect x2 y2)))

;; start-segment
(define (start-segment segment)
  (car segment))

;; end-segment. 
(define (end-segment segment)
  (cdr segment))
;; this was waya too easy. am i missing something?


;; 2.49
;; given
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
;; a.  The painter that draws the outline of the designated frame.
(define (outline->painter frame)
  (let ((tl (make-vect 0 1))
	(tr (make-vect 1 1))
	(bl (make-vect 0 0))
	(br (make-vect 0 1)))
    (let ((s1 (make-segment bl tl))
	  (s2 (make-segment tl tr))
	  (s3 (make-segment tr br))
	  (s4 (make-segment br bl)))
      (segments->painter (list s1 s2 s3 s4)))))

;; b.  The painter that draws an ``X'' by connecting opposite corners of the frame.
(define (X->painter frame)
  (let ((tl (make-vect 0 1))
	(tr (make-vect 1 1))
	(bl (make-vect 0 0))
	(br (make-vect 0 1)))
    (let ((s1 (make-segment bl tr))
	  (s2 (make-segment br tl)))
      (segments->painter (list s1 s2)))))

;; c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define (X->painter frame)
  (let ((lh (make-vect 0 0.5))
	(th (make-vect 0.5 1))
	(rh (make-vect 1 0.5))
	(bh (make-vect 0.5 0)))
    (let ((s1 (make-segment lh th))
	  (s2 (make-segment th rh))
	  (s3 (make-segment rh bh))	  
	  (s4 (make-segment bh lh)))
      (segments->painter (list s1 s2 s3 s4)))))

;; d.  The wave painter. 
;; hell no
