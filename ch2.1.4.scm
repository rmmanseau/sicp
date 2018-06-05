
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.1.4 Extended Exercise: interval Arithmetic                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We are going to implement an interval arithmetic system. We'll start by
; creating the procedures that do simple arithmetic.

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

; Exercise 2.7
; The program is incomplete because an implementation of the interval
; abstraction has not been specified. Here is a definition of the interval
; constructor

(define (make-interval a b) (cons a b))

; Define selectors upper-bound and lower-bound

(define (lower-bound x) (car x)) 
(define (upper-bound x) (cdr x)) 

; Exercise 2.8
; Write a subtraction procedure called sub-interval

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                    (upper-bound x) (upper-bound y)))

; Exercise 2.9
; The width of an interval is the difference between the upper and lower bound
; of the interval

(define (width i)
  (- (upper-bound i) (lower-bound i)))

; Show that the width of the sum / difference of two intervals is a function
; only of the widths of the intervals being added / subtracted.

; To show this we can test if the sum of the widths of each interval is equal
; to the width of the combined intervals

; for an interval x (xl, xu) and y (yl, yu)
; (width x) + (width y) = (width (x + y))
; (xu - xl) + (yu - yl) = ((xu + yu) - (xl + yl))
; xu - xl + yu - yl = xu + yu - xl - yl
; this is true, therefor addition is a function of the widths of the parameters

(define (test-add-widths i j)
  (let ((width-of-sum (width (add-interval i j)))
        (sum-of-widths (+ (width i) (width j))))
    (display "width of sum:  ") (display width-of-sum) (newline)
    (display "sum of widths: ") (display sum-of-widths) (newline)
    (if (= width-of-sum sum-of-widths)
        (display "sum width IS function of parameter widths")
        (display "sum width IS NOT function of parameter widths"))))

; (test-add-widths (make-interval 1 4) (make-interval 8 8.1))
; IS function of parameter widths

; For multiplication / division, we can show that the statement is false
; by finding a counter example

(define (test-mul-widths i j)
  (let ((width-of-product (width (mul-interval i j)))
        (product-of-widths (* (width i) (width j))))
    (display "width of product:  ") (display width-of-product) (newline)
    (display "product of widths: ") (display product-of-widths) (newline)
    (if (= width-of-product product-of-widths)
        (display "product width IS function of parameter widths")
        (display "product width IS NOT function of parameter widths"))))

; (test-mul-widths (make-interval 1 4) (make-interval 8 8.1))
; width of product:  24.4
; product of widths: .29999999999999893
; product width IS NOT function of parameter widths

; Exercise 2.10
; Modify the interval division procedure to throw an error when there is a
; division by an interval that spans zero

(define (interval-contains i a)
  (and (>= a (lower-bound i))
       (<= a (upper-bound i))))

(define (interval-contains-zero i)
  (interval-contains i 0))

(define (div-interval x y)
  (if (interval-contains-zero y)
      (error "Error: The divisor interval should not span zero")
      (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))))))

; Exercise 2.11
; By testing the signs of the endpoints of each interval, it is possible to
; break mul-interval into nine cases, only one of which requires more than
; two multiplications. Rewrite the procedure this way.

; Note than an interval can never be (+ -), because the left side is always
; the lower bound. With this in mind, these are the nine cases.

; (+ +) * (+ +)
; (+ +) * (- +)
; (+ +) * (- -)
; (- +) * (+ +)
; (- +) * (- +)
; (- +) * (- -)
; (- -) * (+ +)
; (- -) * (- +)
; (- -) * (- -)

(define (mul-interval-11 x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((and (>= xl 0)
                (>= xu 0)
                (>= yl 0)
                (>= yu 0))
            ; (+ +) * (+ +)
            (make-interval (* xl yl) (* xu yu)))
          ((and (>= xl 0)
                (>= xu 0)
                (<= yl 0)
                (>= yu 0))
            ; (+ +) * (- +)
            (make-interval (* xu yl) (* xu yu)))
          ((and (>= xl 0)
                (>= xu 0)
                (<= yl 0)
                (<= yu 0))
            ; (+ +) * (- -)
            (make-interval (* xu yl) (* xl yu)))
          ((and (<= xl 0)
                (>= xu 0)
                (>= yl 0)
                (>= yu 0))
            ; (- +) * (+ +)
            (make-interval (* xl yu) (* xu yu)))
          ((and (<= xl 0)
                (>= xu 0)
                (<= yl 0)
                (>= yu 0))
            ; (- +) * (- +)
            (make-interval (min (* xl yu) (* xu yl))
                           (max (* xl yl) (* xu yu))))
          ((and (<= xl 0)
                (>= xu 0)
                (<= yl 0)
                (<= yu 0))
            ; (- +) * (- -)
            (make-interval (* xu yl) (* xl yl)))
          ((and (<= xl 0)
                (<= xu 0)
                (>= yl 0)
                (>= yu 0))
            ; (- -) * (+ +)
            (make-interval (* xl yu) (* xu yl)))
          ((and (<= xl 0)
                (<= xu 0)
                (<= yl 0)
                (>= yu 0))
            ; (- -) * (- +)
            (make-interval (* xl yu) (* xl yl)))
          ((and (<= xl 0)
                (<= xu 0)
                (<= yl 0)
                (<= yu 0))
            ; (- -) * (- -)
            (make-interval (* xu yu) (* xl yl))))))

(define (interval-equal? x y)
  (and (= (lower-bound x) (lower-bound y))
       (= (upper-bound x) (upper-bound y))))

(define (test-mul-11 x y)
  (interval-equal? (mul-interval-11 x y)
                   (mul-interval x y)))

; (define i++ (make-interval 3 4))
; (define i-+ (make-interval -2 7))
; (define i-- (make-interval -5 -1))

; (test-mul-11 i++ i++)
; Value: #t

; (test-mul-11 i++ i-+)
; Value: #t

; (test-mul-11 i++ i--)
; Value: #t

; (test-mul-11 i-+ i++)
; Value: #t

; (test-mul-11 i-+ i-+)
; Value: #t

; (test-mul-11 i-+ i--)
; Value: #t

; (test-mul-11 i-- i++)
; Value: #t

; (test-mul-11 i-- i-+)
; Value: #t

; (test-mul-11 i-- i--)
; Value: #t

; Exercise 2.12
; While the current program is functional, some users may want a program that
; can deal with intervals represented as a center value and an additive
; tolerance. For example, they could be represented as (3.5 +- 0.15) rather than
; (3.35, 3.65). This representation can be added by supplying alternate
; constructors and selectors.

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; However this is still not ideal. Engineers usually work with a small
; uncertainty measured as a ratio of the width of the interval to the
; midpoint of the interval. 
; Define a constructor make-center-percent that takes a center and a percentage
; tolerance and produces the desired interval

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

; Exercise 2.13-16
; These exercises focus on the shortcomings of computational representations of
; interval arithmetic. Dividing an interval by itself does not return 1, only
; a rough approximation. Because of this, two formulas that are algebraically
; equivalent might return widely different results. I'm skipping it because
; it is too heavily math oriented.


