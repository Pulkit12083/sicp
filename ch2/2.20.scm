

;; 2.20 recursive
;; efficient and elegant
(define (same-parity n . nList)
  (define (same-parity-helper num cList)
    (if (null? cList)
	(list num)
	(if (= (modulo num 2) (modulo (car cList) 2))
	    (cons num (same-parity-helper (car cList) (cdr cList)))
	    (same-parity-helper num (cdr cList)))))
  (same-parity-helper n nList))

;; 2.20 WRONG attempt
(define (same-parity n . nList)		; this takes a sequence of integers and CONVERTS into a list by adding () around parameters
  (if (null? cList)			; works as expected 
      (list num)
      ;; throws an error on the second attempt. because a list has been passed to nList which is converted by the .notation into another list
      (if (= (modulo num 2) (modulo (car cList) 2))
	  ;; cdr makes a list already. the second same-parity call uses the .notation to convert that list into ANOTHER nested list causing modulo operator to crash on the inner list next time
	  ;; because car of that cList (converted by .notation) will return a list made by cdr here
	  (cons num (same-parity (car cList) (cdr cList)))
	  ;; same issue as above
	  (same-parity num (cdr cList)))))
;; issue is fixed by using a helper which basically avoids .notation and uses the list converted by the .notation in parent function straight away. only 2 lines of extra code needed
;; i like this soln. better than the kind on the sicp wiki :3
;; also i just learned that ;; indents code to the appropriate method nest in the program
;;; and triple indent indents to the left
					; indents to the center (or towards the right of a line of code
;;; TIL indentation guidelines emacs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html
