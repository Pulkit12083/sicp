(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


;; 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      ;; base case
      (cons '() elts)
      ;; calculates half of n
      (let ((left-size (quotient (- n 1) 2)))
	;; recursively makes first half of balanced tree
        (let ((left-result (partial-tree elts left-size)))
	  ;; save tree
          (let ((left-tree (car left-result))
		;; elements from which to get right tree
                (non-left-elts (cdr left-result))
		;; calculate leftover element size
                (right-size (- n (+ left-size 1))))
	    ;; root node value is first element of remaning elements
            (let ((this-entry (car non-left-elts))
		  ;; recursively call partial tree over remaaining n-left elements 
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
	      ;; extract right tree
              (let ((right-tree (car right-result))
		    ;; extract final remaining elements
                    (remaining-elts (cdr right-result)))
		;; build result
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


;; order is O(n) obviously
;; (1 3 5 7 9 11)
;;     5
;;   -   -
;; 1       7
;;  -     -  -
;;    3  9    11

