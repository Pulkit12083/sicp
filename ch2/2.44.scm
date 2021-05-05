;; AAAAH sicp you're quite a nuisance sometimes.
;; find a way to test this somehow
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))))
	(below painter (beside up up)))))

