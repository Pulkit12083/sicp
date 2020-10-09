
(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))

(f f) ; err because (f f) calls (f 2) calls (2 2) -> syntactically incorrect since 2 is not a procedure
;; and indeed
;; <error>  The object 2 is not applicable.
