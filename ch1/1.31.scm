;; Approximate pi/4: (2.4.4.6.6.8.8.10.10.12.12.14)/(3.3.5.5.7.7.9.9.11.11.13.13..)

;; recursive procedure for product
(define (productRec term a next b)
  (if (> a b) 1 (* (term a) (productRec term (next a) next b))))

(define (productItr term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)
(define (incVal x) (+ x 1))
(productItr identity 1 incVal 4)

;; approximating pi
(define (pi4IntegRec n)
  (define (inc x) (+ x 2.0))
  (/ (* 8 (/ (productRec square 4.0 inc (* 2 n)) (productRec square 3.0 inc (- (* 2 n) 1)))) (* 2 n)))


(pi4IntegRec 15)
