;; AAAAH sicp you're quite a nuisance sometimes.
;; find a way to test this somehow
;; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))))
	(below painter (beside up up)))))

;;Exercise 2.45.  Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating

(define right-split (split beside below))
(define up-split (split below beside))
