;; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;;What are the values of

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list nil (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; (((() 1) 2) 3)

;; property of op for fold-left/right to yield same result:
;; symmetry: (= (op a b) (op b a)) for any a, b?

;; 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append (list x) y)) nil sequence))

(reverse (list 1 2 3 4 5))

(define (reverse sequence)
  (fold-left (lambda (x y) (append x (list y))) nil sequence))

(reverse (list 1 2 3 4 5))
