;; ====== LAB 4 ======  
;;    Author(s):  
;;               Kai Ren
;;               Evan Kearney
;;               
;;  Lab Section: 3


;;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "lab4 (2).scm")  ; Change file name if copied to a new file.
)

(define (display+ . args)
  (for-each
    (lambda (item) (display item))
    args)
  (newline))

;;;; Test Case Values (For Steps 5-9):
;;;;   These are to help simplify the test cases by reusing the lists.

;; Lists
(define test-list1 '(1 2 3 4 5 6 7 8 9))
(define test-list2 '(5))
(define test-list3 '(1 2))
(define test-list4 '())
(define test-list5 '(1 2 3 4))

;; Trees
(define test-tree1 '(1 (2 3) 4))
(define test-tree2 '((1 2 3 4)))
(define test-tree3 '(1 ((2) 3) (4)))
(define test-tree4 '((1 (2 (3 (4 (5)))))))
(define test-tree5 '(((((1) 2) 3) 4) 5))
(define test-tree6 '((((1 2) 3 4) 5 6) 7 8))


;;;; Test Case Code (For steps 5-9):
;;;;   This will handle execution of the test cases we've included below.
;;;;   To run test cases for a step, uncomment the (do-tests #) line.
;;;;   Note:  This code will run on MIT Scheme, but would have to be modified
;;;;          to work with other versions of Scheme. 
;;;;          Change #t to #f in the line below to use for Dr Scheme / STk.
;;;;          Behavior under Dr Scheme / STk is not tested.

(define (do-tests n)
  (let* ((in-mit-scheme #t)  ;; ** Change this value 
	 (tests-symbol 
	  (string->symbol 
	   (string-append "test-cases-step-" 
			  (number->string n))))

	 (test-cases 
	  (if in-mit-scheme 
	      (eval tests-symbol user-initial-environment)
	      (eval tests-symbol)))

	 (display-string (string-append 
			  "\n--- Test Cases for Step "
			  (number->string n)
			  " ---\n")))

    (display display-string)

    (for-each 
     (lambda (x)
       (if (and (pair? x) (eq? (car x) 'define))
	   (if in-mit-scheme 
	       (eval x user-initial-environment) 
	       (eval x))
	   (begin 
	     (display x)
	     (newline)
	     (display (if in-mit-scheme 
			  (eval x user-initial-environment) 
			  (eval x)))
	     (newline))))
     test-cases)))



;; REMINDER:
;;   You must include test cases for all procedures you write in Steps 1-4 and 10-11.
;;   No credit will be given without test cases showing working code.
;;   
;;   This lab gives specific instructions for testing the code.
;;   These are minimum requirements, but you may do further tests as needed.
;;   Use define to store the results of your tests so they can be used in future
;;   steps.
;;
;;   Read through the lab writeup for full instructions and hints on how to
;;   approach this lab.
;;
;;   Also pay attention to hints and clarifications provided in this template
;;   regarding the test cases.



;;;; ====================================================================

;;;;
;;;; Step 1 - Point Abstraction: Starting a 2-Dimensional Point System
;;;;

;; make-point
(define (make-point x y)
  (cons x y))

;; get-x
(define (get-x p)
  (car p))

;; get-y
(define (get-y p)
  (cdr p))

;; Test Code Instructions:
;;   Define a new point.  Display it.
;;   Display the x and y values separately using your selectors.
;;   You may use this point in future tests as well.


;; Note:
;;   The above is done for you below -- just uncomment those lines.
;;   You may want to define some other points here to use in future steps.

(display+ "--- STEP 1 TEST CASES ---")
;; Example Test Case:
(define pt1 (make-point 2 4))
(display+ "Point: "pt1)            ;; Expecting (2 . 4)
(display+ "X-Coord: " (get-x pt1)) ;; Expecting 2
(display+ "Y-Coord: " (get-y pt1)) ;; Expecting 4

;; Define Additional Points:

(define blabla (make-point 10 20)) (newline)
(display blabla) (newline)
(display (get-x blabla)) (newline)
(display (get-y blabla)) (newline)

(define pt2 (make-point 5 9))
(define pt3 (make-point 3 1))

;;;;
;;;; Step 2 - Maintaining a List of Points
;;;;

;; make-pt-list
(define (make-pt-list p pt-list); p is a point, pt-list is a list of points
  (cons p pt-list))

;; the-empty-pt-list
(define the-empty-pt-list '())

;; get-first-point
(define (get-first-point pt-list) (get-x pt-list))

;; get-rest-points
(define (get-rest-points pt-list) (get-y pt-list))


;; Test Code:
;;   Using make-pt-list and the-empty-pt-list, define a list with 6+ points.
;;   Show the list after each point is added.
;;   Display the entire list, the first point, and all but the first point.
;;   Display the second point.
;;   Display all except the first two points.

(display+ "--- STEP 2 - Building The List ---")
;; How to start building the list:
(define my-point-list (make-pt-list pt1 the-empty-pt-list))
(define my-point-list (make-pt-list pt2 my-point-list))
(define my-point-list (make-pt-list blabla my-point-list))
(define my-point-list (make-pt-list pt3 my-point-list))
(define my-point-list (make-pt-list pt1 my-point-list))
(define my-point-list (make-pt-list pt2 my-point-list))

(display+ my-point-list)

(display+ "--- STEP 2 - First Point ---")
(display+ (get-x my-point-list))

(display+ "--- STEP 2 - Second Point ---")
(display+ (get-x (get-y my-point-list)))

(display+ "--- STEP 2 - All Except First Two Points ---")
(display+ (get-y (get-y my-point-list)))


;;;;
;;;; Step 3 - Operations on pt-lists
;;;;

;; sum-xcoord
(define (sum-xcoord pt-list)
  (if (null? pt-list)
      0
      (+ (get-x (get-first-point pt-list)) (sum-xcoord (get-rest-points pt-list)))))

;; max-xcoord
(define (max-xcoord pt-list)
  (define (max-x max pt-list)
    (if (null? pt-list)
	max
	(if (> (get-x (get-first-point pt-list)) max)
	    (max-x (get-x (get-first-point pt-list)) (get-rest-points pt-list))
	    (max-x max (get-rest-points pt-list)))))
  (if (null? pt-list)
      0
      (max-x (get-x (get-first-point pt-list)) pt-list)))						     

;; distance
(define (distance p1 p2)
  (sqrt (+ (square (- (get-x p2) (get-x p1))) (square (- (get-y p2) (get-y p1))))))

;; max-distance
(define (max-distance p pt-list)
  (define (max-d p pt-list max)
    (if (null? pt-list)
	max
	(if (> (distance p (get-first-point pt-list)) max)
	    (max-d p (get-rest-points pt-list) (distance p (get-first-point pt-list)))
	    (max-d p (get-rest-points pt-list) max))))
  (if (null? pt-list)
      0
      (max-d p pt-list (distance p (get-first-point pt-list)))))

;; Test Code
;;   Use the list you created in step 3 and the point created in step 2.
;;   Show the results you get using these values in the above operations.
;;   Test the procedures with an empty point list as well.

(display+ "--- STEP 3 - sum-xcoord ---")
(display+ "List: " my-point-list)
(display+ "Sum of x values: " (sum-xcoord my-point-list)); should return 27, it does
(display+ "List: " the-empty-pt-list)
(display+ "Sum of x values: " (sum-xcoord the-empty-pt-list)); should return 0, it does

(display+ "--- STEP 3 - max-xcoord ---")
(display+ "List: " my-point-list)
(display+ "Max X-Coordinate: " (max-xcoord my-point-list)); should return 10, it does
(display+ "List: " the-empty-pt-list)
(display+ "Max X-Coordinate: " (max-xcoord the-empty-pt-list)); should return 0


(display+ "--- STEP 3 - distance ---")
(display+ "Point 1: " pt1)
(display+ "Point 2: " blabla)
(display+ "Distance Between: " (distance pt1 blabla)); should return ~17.89, it does

(display+ "--- STEP 3 - max-distance ---")
(display+ "Point: " pt1)
(display+ "List: " my-point-list)
(display+ "Max Distance: " (max-distance pt1 my-point-list)); should return ~17.89, it does
(display+ "Point: " pt1)
(display+ "List: " the-empty-pt-list)
(display+ "Max Distance: " (max-distance pt1 the-empty-pt-list)); should return 0


;;;;
;;;; Step 4 - A Question
;;;;

;; Answer to Question:
;; Using our abstractions helps us 1) Practice abstracting away details, which is important in computer science.
;; and 2) Visualize our data the way we want. In other words, we think about the data as a list of coordinates
;; instead of just thinking about the underlying structure of the data.


;;;;
;;;; Step 5 - Skipping Over Elements
;;;;

;; PART A: get-tail
(define (get-tail lst index)
  (if (null? lst)
      '()
      (if (> index 0)
	  (get-tail (cdr lst) (- index 1))
	  (cons (car lst) (get-tail (cdr lst) index)))))


;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-5 
 '(
    (get-tail test-list1 0)
    (get-tail test-list1 6)
    (get-tail test-list1 8)
    (get-tail test-list1 9)
    (get-tail test-list1 10)
    (get-tail test-list2 0)
    (get-tail test-list2 1)
    (get-tail test-list2 2)
    (get-tail test-list3 0)
    (get-tail test-list3 1)
    (get-tail test-list3 2)
    (get-tail test-list4 0)
    (get-tail test-list4 5)
    (get-tail test-list5 -1)
  ))

(do-tests 5)



;; PART B: skip
(define (skip lst)
  (cond ((null? lst) '())
	((null? (cdr lst)) (car lst))
        (else (skip (cdr lst)))))
  


;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-51 
 '(
    (skip test-list1)
    (skip test-list2)
    (skip test-list3)
    (skip test-list4)
    (skip test-list5)
  ))

(do-tests 51)


;;;;
;;;; Step 6 - Mapping
;;;;

;; square-list using cons
(define (square-list-cons items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-cons (cdr items)))))


;; square-list using map
(define (square-list-map items)
  (map square items))



;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-6
 '(
    (square-list-cons test-list1)
    (square-list-cons test-list2)
    (square-list-cons test-list3)
    (square-list-cons test-list4)
    (square-list-cons test-list5)
    (square-list-map test-list1)
    (square-list-map test-list2)
    (square-list-map test-list3)
    (square-list-map test-list4)
    (square-list-map test-list5)
  ))

(do-tests 6)




;;;;
;;;; Step 7 - Deep-Reverse
;;;;

; normal (not deep) reverse. This might be a good starting place in implementing deep reverse.
(define (reverse lst)
  (cond ((null? lst) ())
	(else (append (reverse (cdr lst))
		      (list (car lst))))))

;; deep-reverse
(define (deep-reverse lst)
  (cond ((null? lst) ())
	((list? (car lst)) (append (deep-reverse (cdr lst)) (list (deep-reverse (car lst)))))
	(else (append (deep-reverse (cdr lst)) (list (car lst))))))
  

;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-7 
 '(
    (deep-reverse test-list1)
    (deep-reverse test-list2)
    (deep-reverse test-list3)
    (deep-reverse test-list4)
    (deep-reverse test-list5)
    (deep-reverse test-tree1)
    (deep-reverse test-tree2)
    (deep-reverse test-tree3)
    (deep-reverse test-tree4)
    (deep-reverse test-tree5)
    (deep-reverse test-tree6)
  ))

(do-tests 7)


;;;;
;;;; Step 8 - Using Accumulate
;;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; map
(define (my-map proc sequence)
  (accumulate (lambda (x y) (cons (proc x) y)) () sequence))

;; append
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

;; length
(define (my-length sequence)
  (accumulate (lambda (x y) (1+ y)) 0 sequence))

;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-8 
 '(
    (my-map square test-list4)
    (my-map square test-list2)
    (my-map square test-list3)
    (my-map square test-list5)

    (my-append test-list4 test-list5)
    (my-append test-list5 test-list4)
    (my-append test-list3 test-list3)
    (my-append test-list2 test-list3)

    (my-length test-list1)
    (my-length test-list2)
    (my-length test-list3)
    (my-length test-list4)
    (my-length test-list5)
    (my-length test-tree1)
    (my-length test-tree2)
    (my-length test-tree3)
  ))

(do-tests 8)

;;;;
;;;; Step 9 - One More Operation on pt-lists
;;;;

;; max-range
(define (max-range pt-list)
  (define (max-r pt-list max)
    (cond ((null? pt-list) max)
	  ((> (max-distance (get-first-point pt-list) (get-rest-points pt-list)) max)
	        (max-r (get-rest-points pt-list) (max-distance (get-first-point pt-list) (get-rest-points pt-list))))
	  (else (max-r (get-rest-points pt-list) max))))
  (if (null? pt-list)
      0
      (max-r pt-list 0)))
	    

(display+ "--- STEP 9 TEST CASES ---")
(display+ my-point-list)
(display+ (max-range my-point-list)); should return ~20.25, distance betw. (10. 20) and (3 . 1)

(define test-pt-list (make-pt-list (make-point 3 5) the-empty-pt-list))
(define test-pt-list (make-pt-list (make-point -1 1) test-pt-list))
(define test-pt-list (make-pt-list (make-point -2 4) test-pt-list))
(define test-pt-list (make-pt-list (make-point 5 0) test-pt-list))
(define test-pt-list (make-pt-list (make-point 2 -6) test-pt-list))
(define test-pt-list (make-pt-list (make-point 0 0) test-pt-list))

(display+ test-pt-list)
(display+ (max-range test-pt-list)); should return ~11.05, the distance betw. (2 . -6) and (3 . 5)







;;;;
;;;; Step 10 - Maintaining a Sorted Point-List
;;;;

;; make-sorted-pt-list
(define origin (make-point 0 0))
;(define (make-sorted-pt-list2 p pt-list)
;  (define (mspl p pt-list progress)
;    (if (> (distance p origin) (distance (get-first-point pt-list) origin))
;	(mspl p (get-rest-points pt-list)

(define (make-sorted-pt-list p pt-list)
  (cond ((null? pt-list) (cons p the-empty-pt-list)) 
	((<= (distance p origin) (distance (get-first-point pt-list) origin)) (cons p pt-list))
	(else (cons (get-first-point pt-list) (make-sorted-pt-list p (get-rest-points pt-list))))))

;; Answer to Question: The operators written so far aren't helped that much by having the list sorted,
;;        so when solving those problems it's not worth putting the list in sorted order.  Some operator 
;;        concerning distance from the origin would probably be more efficient in a sorted list because
;;        it would be easy to call up the farthest/nearest points.

;; Test Code:
;;   Create a sorted list of at least 6 points.

;;   Be sure to test addition of points to the front, back, and middle.
;;   Show the list after each point is added.


(display+ "--- STEP 10 TEST CASES ---")
(define sorted-list (make-sorted-pt-list (make-point 1 5) the-empty-pt-list))
(define sorted-list (make-sorted-pt-list (make-point -2 10) sorted-list))
(define sorted-list (make-sorted-pt-list (make-point 31 32) sorted-list))
(define sorted-list (make-sorted-pt-list (make-point -10 15) sorted-list))
(define sorted-list (make-sorted-pt-list (make-point 100 -100) sorted-list))
(define sorted-list (make-sorted-pt-list (make-point 0 69) sorted-list))
(display+ sorted-list)
(display+ (make-sorted-pt-list (make-point 0 0) sorted-list));should put point at beginning
(display+ (make-sorted-pt-list (make-point 1000 0) sorted-list));should put point at end
(display+ (make-sorted-pt-list (make-point -99 100) sorted-list));should put point in next to end-most position

