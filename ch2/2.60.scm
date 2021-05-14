(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; 2.59
;; union-set
(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) (append set1 set2))
        ((not (element-of-set? (car set1) set2))        
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;; 2.60
;; duplicates exist in the list

;; element-of-set? remains the same
;; adjoin-set becomes constant time
(define (adjoin-set-dupl x set)
  (cons x set))
;;union set becomes linear time instead of quadratic
(define (union-set set1 set2)
  (append set1 set2))
;; intersection is trickier. do we want to keep min of duplicates in either set?
;; or not care about duplicates and just keep one?
;; if latter, intersection can be kept the same with same complexity
;; yes. frequent adjoins and lookups. a growing set. 
