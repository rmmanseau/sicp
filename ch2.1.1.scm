
; Ch2 - Building Abstractions with Data

; 2.1 - Introduction to Data Abstraction

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.1.1 Example: Arithmetic Operations for Rational Numbers                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We want to be able to add, subtract, multiply, divide, and test the equality
; of rational numbers. If we assume that we have a procedure to construct
; rational numbers, and a procedure to select the numerator and denominator from
; a rational number, we can create these arithmetic procedures

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y) 
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

; We now have all of the arithmetic operations defined in terms of the
; rational number constructors and selectors, but we have yet to define them
; We need some way of gluing together a numerator and denominator

; Scheme provides a compound structure called a pair, which can be constructed
; with the primitive procedure (cons). This procedure takes two arguments and
; returns a compound data object. To extract the parts of the pair we can use
; the primitives car and cdr. They are used as follows:

; (define x (cons 1 2))
; (car x)
; 1
; (cdr x)
; 2

; We can use these pairs to define the constructors and selectors of our
; rational number system

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

; And finally, we need a way to print out a rational number

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Currently, our rational number implementation doesn't reduce numbers to
; their lowest terms. This can be solved by dividing both terms by their gcd
; in the make-rat procedure

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; Exercise 2.1
; Define a better version of make-rat that handles both positive and negative
; arguments.

(define (sign x)
  (if (< x 0) -1 1))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (* (sign (* n d))
             (abs (/ n g)))
          (abs (/ d g)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.1.2 Abstraction Barriers                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 2.2
; Consider the problem of representing line segments in a plane. Youll need
; a way of representing points. After creating selectors and constructors for
; segments and points, define a procedure that finds the midpoint of a segment

(define (make-segment s e) (cons s e))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (add-point a b)
  (make-point (+ (x-point a) (x-point b))
              (+ (y-point a) (y-point b))))

(define (scale-point p s)
  (make-point (* (x-point p) s)
              (* (y-point p) s)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (scale-point (add-point (start-segment s)
                          (end-segment s))
               (/ 1 2)))

; Exercise 2.3
; Implement a representation for rectangles in a plane, and then create
; procedures that calculate the perimeter and area of of the rectangle.
; After, implement a different representation for rectangles. Try to create
; abstraction barriers such that the area and parameter procedures work
; with either representation

; Rectangle represented by two points
(define (make-rectangle a b) (cons a b))

(define (width-rectangle r)
  (abs (- (x-point (car r))
          (x-point (cdr r)))))
(define (length-rectangle r)
  (abs (- (y-point (car r))
          (y-point (cdr r)))))

(define (rectangle-perimeter r)
  (+ (* 2 (length-rectangle r))
     (* 2 (width-rectangle r))))

(define (rectangle-area r)
  (* (length-rectangle r)
     (width-rectangle r)))

; Rectangle represented by one point, a length, and a width
(define (make-rectangle a w l) (cons a (cons w l)))

(define (width-rectangle r)
  (car (cdr r)))
(define (length-rectangle r)
  (cdr (cdr r)))


