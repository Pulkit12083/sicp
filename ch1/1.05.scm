;;; applicative vs normal order

(define (p) (p)); recursively infinite

(define (test x y)
  (if (= x 0); normal order exits lazily
      0      ; applicative order evaluates all arguments
      y))

(test 0 (p)) ; infinite loop since (p) never completes evaluation
