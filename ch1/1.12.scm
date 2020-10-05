#lang scheme
; Pascal's triangle
; Compute elements of pascal's triangle using recursive process
; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1

;recursive
(define (pascal row col)
  (cond ((or (< row 1) (< col 1) (< row col)) 0)
        ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

;iterative would require a table.
