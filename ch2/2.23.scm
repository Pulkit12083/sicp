;;(for-each (lambda (x) (newline) (display x))
;;          (list 57 321 88))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;; implementation for each my own

(define (for-each-2 proc lst)
  (if (null? lst) #t (proc (car lst)))
  (if (null? lst) #t (for-each-2 proc (cdr lst))))

(for-each-2 (lambda (x) (newline) (display x))
          (list 57 321 88))


(define (return-multiple-things x)
  (* x 2)				;discarded
  (* x 3))				;returned

(return-multiple-things 3)


(define x (cons (list 1 2) (list 3 4)))
(length (cons 1 2));error
(length x); not error?

(define y (cons 1 2))
(length y); error


(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
