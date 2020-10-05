;Recursive
;f(n) = n (if n<3)
;f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3)
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3))))))

;Iterative process

(define (f-iterative n)
  (define (f-iter count fc1 fc2 fc3)
    (define fc (+ fc1 (* 2 fc2) (* 3 fc3)))
    (if (= count n)
        fc
        (f-iter (+ count 1) fc fc1 fc2)))
  (if (< n 3)
      n
      (f-iter 3 2 1 0)))


