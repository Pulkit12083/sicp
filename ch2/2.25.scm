;; return 7 from these:
(define a (list 1 3 (list 5 7) 9))
;;(1 3 (5 7) 9)


(define b (list (list 7)))
;; ((7))

(define c (list (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))
;;(1 (2 (3 (4 (5 (6 7))))))


