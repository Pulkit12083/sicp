#|
Exercise 2.55.  Eva Lu Ator types to the interpreter the expression
(car ''abracadabra)
To her surprise, the interpreter prints back quote. Explain. 

my first ans: the nested expression 'abracadabra evaluates to a general type 'quote that represents symbolic strings?d and the symbol for which is quote?
my second answer

my final answer: car returns the first symbol in the symbol list of 'abracadabra which is a quote.

|#

;; -----------------------------------------------------------------------------
 ;; scheme community wiki answer:  
 ;; (car ''something) is treated by the interpreter as: 
 ;; (car (quote (quote something))) 
 ;; The first occurrence of 'quote' quotes the next entity 
 ;; (quote something),which is actually a list with two elements, so 
 ;; caring this list yields 'quote. However, this is just a quoted 
 ;; symbol, not a procedure, typing quote in the interpreter prints: 
  
;; quote 
  
 ;; =>(#@keyword . #<primitive-macro! #<primitive-procedure quote>>) 
 ;; whereas typing 'quote just yielded it literally. 
;;---------------------------------------------------------------------------------------

;; close enough
