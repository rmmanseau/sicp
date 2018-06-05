
; 1.3 - Formulating Abstractions with Higher-Order Procedures

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.3.1 Procedures as Arguments                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube a)
  (* a a a))

; Consider the following three procedures

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

; They all sum things, and so they all follow a very similar pattern:

; (define (<name> a b)
;   (if (> a b)
;       0
;       (+ (<term> a)
;          (<name> (<next> a) b))))

; This presence of this pattern suggests that theres a useful abstraction
; waiting to be made. In this case, the abstraction would be summation,
; which mathematicians have represented with sigma notation.

; We can create a procedure for summation by turning the above template
; into a procedure with formal paramters

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; We can now rewrite the earlier summation procedures in terms of our
; new sum procedure

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity n) n)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; Once we have sum, we can use it as a building block for further concepts.
; For instance, the definite integral of a function f between the limits a
; and b can be approximated using the formula:
; int[a->b](f) = [f(a + dx/2) + f(a + dx + dx/2) + f(a + 2dx + dx/2) + ... ]dx
; for small values of dx. This can be expressed as a procedure

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

; Exercise 1.29
; Simpsons Rule is a more accurate method of numerical integration than the
; method illustrated above. Using Simpsons Rule, the integral of a function
; f between a and b is approximated as:
; (h/3)*(y<0> + 4*y<1> + 2*y<2> + 4*y<3> + 2*y<4> + ... + 2*y<n-2> + 4*y<n-1> + y<n>)
; where h = (b - a)/n, for some even integer n, and y<k> = f(a + kh)

(define (odd? n)
  (= (remainder n 2) 1))

(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (y k)))
  (* (/ h 3)
     (sum term 0 inc n)))

; (integral cube 0 1 0.01)   *** .24998750000000042 
; (simpsons cube 0 1 100.0)  *** .24999999999999992 

; (integral cube 0 1 0.001)  *** .249999875000001
; (simpsons cube 0 1 1000.0) *** .2500000000000003

; As you can see, using the same amount of terms, the simpsons rule is the
; better approximation

; Exercise 1.30
; The sum procedure we have been using thus far generates a linear recursion.
; Use the following template to create an iterative sum

; (define (sum term a next b)
;   (define (iter a result)
;     (if <??>
;         <??>
;         (iter <??> <??>)))
;   (iter <??> <??>))

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31
; Write an interative and recursive product procedure that is analogous to sum. 
; Use it to define factorial and to approximate pi using the formula:
; pi/4 = (2*4*4*6*6*8*...)/(3*3*5*5*7*7*...)

; recursive
(define (product-r term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-r term (next a) next b))))

; iterative
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product a b)
  (define (pi-term n)
    (/ (* n (+ n 2))
       (square (+ n 1))))
  (define (pi-next n)
    (+ n 2))
  (product pi-term a pi-next b))

; Exercise 1.32
; Sum and product are both special cases of a more general notion called
; accumulate. The function looks like this:
; (accumulate combiner null-value term a next b)
; Write a recursive and iterative version, and then show how sum and product
; can be written in terms of accumulate

(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-r combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-r + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; Exercise 1.33
; You can obtain an even more general version of accumulate by introducing
; a filter on the terms to be combined. Create a filtered-accumulate procedure
; and use it to sum the squares of the prime numbers in the interval a to b

(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter a result)
    (cond ((> a b)
            result)
          ((filter a)
            (iter (next a) (combiner result (term a))))
          (else
            (iter (next a) result))))
  (iter a null-value))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))

(define (divides? a b) (= (remainder b a) 0))

(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 prime? square a inc b))











