;;;;Author:Kai Ren  
;;;;ID #:3941426                
;;;;Lab Section:003


;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "hw4.scm")  ; Change file name if copied to a new file.
)


(define (display+ . args)
  (for-each
    (lambda (item) (display item))
    args)
  (newline))


;;Square
(define (square x) (* x x))

;;Cube
(define (cube x) (* x x x))

;;Increment Function
(define (inc x) (+ 1 x))

;;Nil
(define nil '())

;;Accumulate (as defined in lab )
(define (accumulate combiner null-value term a next b)
	(if (> a b)
		 null-value
		(combiner (term a) 
			(accumulate combiner null-value term (next a) next b))))
			
;;Filter, as defined in the textbook
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))




;;;; Problem 1 - Manipulating lists (30 pts)
;;;; ===================================
  (define (max l);find the max numer in list
    (cond ((null? (cdr l)) (car l))
	  ((> (car l) (max (cdr l))) (car l))
	  (else (max (cdr l)))))

;; Find depth of nested list
(define (tree-height lst)
  (inc (max  
  (map (lambda (x) (cond ((number? x) 0)
			 ((list? x) (tree-height x)))) lst)
)))

;; Find the nth prime number 
;; Hint: (iota n) returns a list from (1 2 3 ... n). Variations on iota exist.

(define (prime-sequence n)
  (define (nit n seq)
    (if (< n 2) ; From 2
        seq
        (nit (- n 1) (cons n seq))))
  (nit n '()))
(define (nth-prime n)
  (define (co-prime? x)
    (lambda (y)
      (positive? (remainder y x))))
  (define (prime-filter n seq)
    (cond
     ((null? seq) null)
     ((>= n 1000) '())
     ((< n 2) (car seq))
     (else (prime-filter (- n 1) (filter (co-prime? (car seq)) seq)))))
  (prime-filter n (prime-sequence 1000)))

(display+ "====Problem 1 Test Cases====")

;; Insert your test cases here - write about five for each problem
(display (tree-height '(1 2 3 4)))(newline)
(display (tree-height '(1 2 3)))(newline)
(display (tree-height '((1) (2 (3)))))(newline)
(display (tree-height '((1 2) (3 (4)))))(newline)

(display (nth-prime 1))(newline)
(display (nth-prime 5))(newline)
(display (nth-prime 1000))(newline)

;;;; Problem 2 - Working with Matrices (30 pts)
;;;; ==========================================


(define (matrix? mtrx)

  (cond
   ((= (length mtrx) 1) #t) ;return true if the length of the mtrx is 1 
   ((and (list? (car mtrx)) (list? (car (cdr mtrx)))
	 (= (length (car mtrx)) (length (car (cdr mtrx))))) (matrix? (cdr mtrx))); return the (cdr mtrx) if the length of the first sublist equals to the second one, this is a recursive procedure, it will make the length of mtrx shorter and shorter till to 1, then return #t 
   (else #f))); return #f if the length of each sublist in the mtrx not equal


(define (equal-size-matrix? mtrx1 mtrx2);assuming mtrx1 and mtrx2 are matrices
  (if (= (length mtrx1) (length mtrx2));compare the row of them
      (if (= (length (car mtrx1)) (length (car mtrx2)));compare the column of them
	  #t
	  #f)
      #f)
)


(define (add-matrix mtrx1 mtrx2)
  (cond ((equal-size-matrix? mtrx1 mtrx2);only works between equal size matrices
      (let iter ((X mtrx1) (Y mtrx2) (result '()));it is a iterative procedure
	(if (or (null? X) (null? Y))
	    (reverse result)
	    (iter (cdr X) 
		  (cdr Y)
		  (cons (map + (car X) (car Y)) result)))));using map to calculate and cons to  return a new matrix
	(else "Cannot add provided matrices")));return cannot add provided matrices if not equal size 

(define (sub-matrix mtrx1 mtrx2)
  (cond ((equal-size-matrix? mtrx1 mtrx2)
      (let iter ((X mtrx1) (Y mtrx2) (result '()))
	(if (or (null? X) (null? Y))
	    (reverse result)
	    (iter (cdr X) 
		  (cdr Y)
		  (cons (map - (car X) (car Y)) result)))))
	(else "Cannot subtract provided matrices"))); return "cannot subtract provided matrices" if not equal size matrices

(define (transpose mtrx)
  (if (matrix? mtrx)
      (if (null? (car mtrx))
	  '()
	  (cons (map car mtrx) (transpose (map cdr mtrx))))
      "Invalid matrix")
)

; Some test cases provided for P2, but write your own for P3. Also you should probably write some more for P2.
(display+ "====Problem 2 Test Cases====")
(define matrix-1 '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define matrix-2 '((6 1 1 3) (2 2 5 7) (5 4 3 2)))
(define matrix-3 '((3 4 5) (2 3 4)))
(define matrix-4 '((2 3) (5 6)))
(define matrix-5 '((5 4) (3 2) (7 1)))
(define matrix-no-1 '(3 3 4 (4 5 6) (7 3 9)))
(define matrix-no-2 '((9 6) (3 4 5) (1 9 3)))

(display+ (matrix? matrix-no-2))	; #f
(display+ (matrix? matrix-no-1))	; #f
(display+ (matrix? matrix-3))		; #t
(display+ (matrix? matrix-5))		; #t
(display+ (matrix? '((1 2 3 4))))	; #t

(display+ (equal-size-matrix? matrix-1 matrix-2))	; #t
(display+ (equal-size-matrix? matrix-1 matrix-5))	; #f
(display+ (equal-size-matrix? matrix-4 matrix-5))	; #f

(display+ (add-matrix matrix-1 matrix-2))               ; ((7 3 4 7) (6 7 11 13) (11 11 11 11))
(display+ (add-matrix matrix-1 matrix-3))               ; "Cannot add provided matrices"
(display+ (add-matrix matrix-1 matrix-no-1))            ; "Cannot add provided matrices"

(display+ (sub-matrix matrix-1 matrix-2))               ; ((-5 1 2 1) (2 3 1 -1) (1 3 5 7))
(display+ (sub-matrix matrix-1 matrix-3))               ; "Cannot subtract provided matrices"
(display+ (sub-matrix matrix-1 matrix-no-1))            ; "Cannot subtract provided matrices"

(display+ (transpose matrix-1))         ; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(display+ (transpose matrix-5))         ; ((5 3 7) (4 2 1))
(display+ (transpose matrix-no-1))      ; "Invalid matrix."

;; Feel free to add more test cases here.
(display+ (matrix? '(1 2 3)));#f
(display+ (equal-size-matrix? '() matrix-1));#f
(display+ (add-matrix '() matrix-1));"Cannot add provided matrices"
(display+ (sub-matrix matrix-3 matrix-2));"Cannot subtract provided matrices"
(display+ (transpose matrix-no-2));"Invalid matrix"


;;;; Problem 3 - Message Passing (20 pts)
;;;; ===================================


(define (my-matrices first-matrix second-matrix)
  (define (dispatch op);using dispatch procedure to pass messages
    (cond ((eq? op 'first) (if (matrix? first-matrix);if op equals 'first
			       first-matrix;return the first matrix if it is #t 
			       "Invalid matrix"));else return invalid matrix
	  ((eq? op 'second) (if (matrix? second-matrix) 
				second-matrix
				"Invalid matrix"))
	  ((eq? op 'add) (add-matrix first-matrix second-matrix));call add-matrix function
	  ((eq? op 'sub) (sub-matrix first-matrix second-matrix));call sub-matrix function
	  ((eq? op 'transpose) (lambda (op2) (cond 
					      ((eq? op2 'first) (transpose first-matrix))
					      ((eq? op2 'second) (transpose second-matrix))
					      (else "Unknown matrix"))))
	  (else "Invalid message")))
dispatch)


(display+ "====Problem 3 Test Cases====")
(define y (my-matrices matrix-1 matrix-2))
(define n (my-matrices matrix-no-1 matrix-1))

(display+ (y 'first))                           ; ((1 2 3 4) (4 5 6 6) (6 7 8 9))
(display+ (y 'second))                          ; ((6 1 1 3) (2 2 5 7) (5 4 3 2))
(display+ (n 'first))                           ; "Invalid matrix"

(display+ (y 'add))                             ; ((7 3 4 7) (6 7 11 13) (11 11 11 11))
(display+ (y 'sub))                             ; ((-5 1 2 1) (2 3 1 -1) (1 3 5 7))
(display+ (n 'add))                             ; "Cannot add provided matrices"
(display+ (n 'sub))                             ; "Cannot subtract provided matrices"

(display+ ((y 'transpose) 'first))              ; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(display+ ((y 'transpose) 'second))             ; ((6 2 5) (1 2 4) (1 5 3) (3 7 2))
(display+ ((n 'transpose) 'first))              ; "Invalid matrix"
(display+ ((n 'transpose) 'bad-input))          ; "Unknown matrix"

(display+ (y 'bad-input))                       ; "Invalid message"
(display+ (n 'unknown-message))                 ; "Invalid message"

;; Add 5 or more test cases here (see writeup)
(display+ (n 'second));((1 2 3 4) (4 5 6 6) (6 7 8 9))
(display+ ((y 'transpose) 'where));Unkown matrix
(display+ ((n 'transpose) 'second));((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(display+ (y 'nothing));Invalid message
(display+ (n 'other));Invalid message






