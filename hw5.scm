;;;;CSci 1901 Summer 2013
;;;;HW 5
; =========
;;;;Author:Kai Ren
;;;;ID #:3941426 
;;;;Lab Section:003


;; Note: Answers to some test cases are given below. For those test
;; cases not having given answers, you should check your code result
;; against what you get when you evaluate the problem manually.
;; If there are no test cases, be sure to write some.

;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "hw5.scm")  ; Change file name if copied to a new file.
)


;;Problem 1
(define (make-order)
  (let ((order ()))
    (define (add-order name price);add-order function
      (set! order (cons (list name price) order));get information to build table
      order);using set! 
    (define (getOrder)
      (define (helper order name);iterative procedure, build a helper function
      (cond ((null? order) name);basic step, return name if the order is empty
	    (else (helper (cdr order) (string-append name" " (caar order))))));return the name of the order (caar order), and recursively append the string(name) to the previous one  
      (helper order ""));call helper function, the order is empty at the first
    (define (getCost)
      (define (helper order cost);create a helper function for iterative procedure
	(cond ((null? order) (string-append "$" (number->string cost)));basic step: return a string number which is the cost if the order is empty
	      (else (helper (cdr order) (+ cost (cadar order))))));
	(helper order 0));call the helper function cost is 0 initially
    (define (clear);clear order function
      (begin (set! order '())));set the order to empty
    (define (dispatch op);dispatch function to pass messages
      (cond 
       ((eq? op 'add-hamburger) (add-order "Hamburger" 4));if typing 'add-hamburger, it will call the add-order function to put the Hamburger and costs 4 into the bill. 
       ((eq? op 'add-fries) (add-order "Fries" 2))
       ((eq? op 'add-chicken) (add-order "Chicken" 3))
       ((eq? op 'add-soda) (add-order "Soda" 1))
       ((eq? op 'add-water) (add-order "Water" 0))
       ((eq? op 'getCost) (getCost));call getCost function
       ((eq? op 'getOrder) (getOrder));call getOrder function
       ((eq? op 'clear-order) (clear));call clear function
       (else "unknown message")))
dispatch)
)

;;Problem 1 test cases
(define Bill (make-order))
(define Jill (make-order))

;Be sure to test your own cases! Not all of the functionality is tested yet!
(Bill 'add-hamburger)(newline)
(display (Bill 'getOrder)) ; "Hamburger "
(Bill 'add-soda)(newline)
(display (Bill 'getOrder))(newline) ; "Soda Hamburger "
(display (Bill 'getCost))(newline) ; "$5"
(display (Jill 'getCost))(newline) ; "$0"
(Jill 'add-fries)(newline) 
(display (Jill 'getCost))(newline) ;"$2"
(Bill 'clear-order)(newline)
(display (Bill 'getOrder))(newline) ; ""
(display (Jill 'getOrder))(newline) ;"Fries "
(Jill 'add-water)(newline)
(display (Jill 'getOrder))(newline) ;"Water Fries "


;;Cycles

;part 2a

(define (make-n-cycle n)
  (let ((start 0))
    (lambda ()
      (if (< start n)
	  (begin (set! start (+ start 1)) start);recursively calling, set the start to 0+1 at the first time when start < n
	  (begin (set! start (- start (- n 1))) start))));return start-(n-1) when start >= n
)


;;Test cases

(define c3 (make-n-cycle 3))
(define c2 (make-n-cycle 2))
(display (c3))(newline) ;outputs 1
(display (c2))(newline) ;outputs 1
(display (c3))(newline) ;outputs 2
(display (c2))(newline) ;outputs 2
(display (c3))(newline) ;outputs 3
(display (c2))(newline) ;outputs 1
(display (c3))(newline) ;outputs 1
(display (c2))(newline) ;outputs 2


;2b

(define (cycle-sum cyc1 cyc2 n)
      (cond
       ((= n 0) '());basic step, return empty set when n=0
       ((> n 0) (cons (+ (cyc1) (cyc2)) (cycle-sum cyc1 cyc2 (- n 1)))))
);building a recursive procedure when n>0, using cons to build the list

;Test Cases for 2b

(define cy2 (make-n-cycle 2))
(define cy3 (make-n-cycle 3))

(display (cycle-sum cy3 cy2 6)); outputs (2 4 4 3 3 5)
