#lang racket

; Tree recursive procedure
(define (get-denomination kind-of-coin)
  (cond ((= kind-of-coin 1) 1)
        ((= kind-of-coin 2) 5)
        ((= kind-of-coin 3) 10)
        ((= kind-of-coin 4) 25)
        ((= kind-of-coin 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((= kinds-of-coins 0) 0)
        (else (+ (cc (- amount (get-denomination kinds-of-coins)) kinds-of-coins)
                 (cc amount (- kinds-of-coins 1))))))


(define (count-change amount)
  (cc amount 5))

; iterative process: necessarily require memoization? dp?

