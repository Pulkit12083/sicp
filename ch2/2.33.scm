
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq seq2))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length (list 1 2 3 4 5 6 7))
(append (list 2 3 4) (list 1 23 3))
(map (lambda (x) (* x x)) (list 1 2 3 4 5))
