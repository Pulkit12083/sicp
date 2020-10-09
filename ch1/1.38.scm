;; f (n d k) = N1/(D1 + (N2/(D2+ N3(D4..+Nk/Dk))))

;; reversing k indices for ease tbh
;; works only for procedures that reverrse n d
(define (contfracrev n d k)
  (cond ((= k 1) 0)
	(else (/ (n k) (+ (d k) (contfrac n d (- k 1)))))))

(define (contfracrec n d k)
  (define (iter i)
    (cond ((= i k) (/ (n i) (d i)))
	  (else (/ (n i) (+ (d i) (iter (+ i 1)))))))
  (iter 1))

(define (contfraciter n d k)
  (define (iter i result)
    (cond ((= i 0) result)
	  (else (iter (- i 1) (/ (n i) (+ (d i) result))))))
  (iter k 0))
	   
(contfraciter (lambda (i) 1.0)
	  (lambda (i) 1.0)
	  50)

;;1.38.scm
(define (baseLogEuler step)
  (contfraciter (lambda (i) 1.0)
		(lambda (i) (if (= (remainder (+ 1 i) 3) 0)
				(* 2 (/ (+ i 1.0) 3.0))
				1))
		step))

(baseLogEuler 60)

;;1.39.scm
(define (tan x step)
  (contfractiter (lambda (i) (if (= i 1) x (* x x)))
		 (lambda (i) (- (* 2 i) 1))
		 step))



