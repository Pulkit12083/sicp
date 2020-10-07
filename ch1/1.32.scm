;; build generate accumulate with combine
(define (identity x) x)
(define (incByOne x) (+ x 1))

;;recursive accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;;iterative accumulate
(define (accumulateIter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (mult a b) (* a b))
(define (rangedProduct a b)
  (accumulate mult 1 identity a incByOne b))

(define (factorial n)
  (rangedProduct 1 n))

(factorial 5)

(define (addit a b) (+ a b))

(define (rangedSum a b)
  (accumulateIter addit 0 identity a incByOne b))

(rangedSum 1 4)

(define (accumulate 
;; recursive procedure for product
(define (productRec term a next b)
  (if (> a b) 1 (* (term a) (productRec term (next a) next b))))

(define (incVal x) (+ x 2))

(define (productItr term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (* result (term a)))))
  (iter a 1))

;; approximating pi/4
(define (pi4IntegRec n)
  (define (inc x) (+ x 2.0))
  (/ (* 8 (/ (productRec square 4.0 inc (* 2 n)) (productRec square 3.0 inc (- (* 2 n) 1)))) (* 2 n)))


(pi4IntegRec 15)
