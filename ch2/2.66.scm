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

;; ordered adjoin
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (cond ((= (car set) x) set)
	    ((< (car set) x) (adjoin-set x (cdr set)))
	    (else (cons x set)))))

;; 2.62.scm
;; ordered union
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
	((< (car set2) (car set1)) (cons (car set2) (union-set set1 (cdr set2))))
	(else (union-set (cdr set1) set2))))



;; 2.63

;; O(nlogn)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
;; O(n)
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; 2.64
;; O(n)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; O(n)
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

;; Exercise 2.65.  Use the results of exercises 2.63 and  2.64 to give (n) implementations of union-set and intersection-set for sets implemented as (balanced) binary trees.41 

(define (union-tree-set set1 set2)
  (list->tree (union-set (tree->list2 set1) (tree->list2 set2))))

(define (intersection-tree-set set1 set2)
  (list->tree (intersection-set (tree->list2 set1) (tree->list2 set2))))
  


;;  Exercise 2.66.  Implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.

;; given: lookup for unordered
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
	((equal? given-key (key (entry tree-of-records))) (entry tree-of-records))
	((< given-key (key (entry tree-of-records))) (lookup given-key (left tree-of-records)))
	(else (lookup given-key (right tree-of-records)))))
