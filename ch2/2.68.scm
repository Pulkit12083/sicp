;; selectors and constructors
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;; decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; weighted element sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


;;2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

 (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;;;Value: (a d a b b c a)



;; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;;
;;(define (choose-branch bit branch)
;;  (cond ((= bit 0) (left-branch branch))
;;        ((= bit 1) (right-branch branch))
;;        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

 (define (encode-symbol symbol tree)
   (cond ((leaf? tree) (if (eq? symbol (symbol-leaf tree)) '() (error "bad symbol, not found in tree")))
	 ((element-of? symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
	 ((element-of? symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
	 (else (error "bad symbol -- not found in tree"))))

(define (element-of? x set)
  (if (null? set) False
      (if (eq? x (car set)) True (element-of? x (cdr set)))))

(encode '(a d a b b c a) sample-tree)
;; ;Value: (0 1 1 0 0 1 0 1 0 1 1 1 0) yes : )
