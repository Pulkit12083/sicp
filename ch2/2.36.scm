;; return an output sequence that operates on corresponding indexes of the sequences
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car (x)) seq)))
            (accumulate-n op init (map (lambda (x) (cdr (x)) seq))))))

;; more obvious: (lambda (x) (car x)) is the same as car

(define (accumulate-n-simple op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seq))
            (accumulate-n op init (map cdr seq)))))

