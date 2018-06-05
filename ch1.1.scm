
; Ch1 - Building Abstractions with Procedures

; 1.1 - The Elements of Programming

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1.1 Expressions                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1.2 Naming and the Environment                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi 3.14159)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1.4 Compound Procedures                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x)       (*        x     x))
;^       ^      ^         ^        ^     ^
;to      square something multiply it by itself

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1.5 The Substitution Model for Procedure Application                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1.6 Conditional Expressions and Predicates                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The general form of the symbol cond is as follows:
; (cond (<P1> <E1>)
;       (<P2> <E2>)
;       ...
;       (<Pn> <En>))
; where P is the predicate and E is the consequent expression.
; Starting from P1, predicates are tested until one is found true,
; in which case the corresponding consequent expression is returned.

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; Else (or any predicate that always evaluates to true) can be used
; as the final predicate.

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

; If is another special form, a restricted type of conditional that
; can be used when there are precisely two cases in the case analysis
; (if <precidate> <consequent> <alternative>)

(define (abs x)
  (if (< x 0)
      (- x)
      x))

; In addition to primitive predicates like <, =, and >, there are
; logical composition operations
; (and <E1> ... <En>)
; (or <E1> ... <En>)
; (not <E>)
;
; (and) and (or) are special forms, not procedures, because the subexpressions
; are not all evaluated. The (and) stops at the first false expression and the
; (or) stops at the first true one

; Exercise 1.3
; Define a procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers

(define (ex1-3 a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (< b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))

; above fails (ex1-3 2 2 3), falsely evaluation (sum-of-squares 2 2)
; below, found online, does not have that issue

(define (larger x y)
  (if (> x y) x y))

(define (ex1-3 a b c)
  (if (= a (larger a b))
      (sum-of-squares a (larger b c))
      (sum-of-squares b (larger a c))))

; Exercise 1.4
; Understand the following procedure

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; in the case of (a-plus-abs-b 3 4),  (> b 0) is true,  resulting in (+ 3 4)
; in the case of (a-plus-abs-b 3 -4), (> b 0) is false, resulting in (- 3 -4)

; Exercise 1.5
; How do the results of the following expression differ in an interpreter that
; uses applicative order evaultion vs one that uses normal-order evaluation

; (define (p) (p))

; (define (test x y)
;   (if (= x 0)
;       0
;       y))

; (test 0 (p))

; applicative-order
; (test 0 (p))
; (test 0 (p))
; ...
; Program will recurse endlessly trying to evaluate (p)

; normal-order
; (test 0 (p))
; (if (= 0 0) 0 (p))
; (if #t 0 (p))
; 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1.7 Example: Square Roots by Newtons Method                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Newtons method for finding the root of a value x, is to choose a guess y
; and then average y with x/y to get a better guess. Multiple iterations
; will get closer and closer to the root of x

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve-sqrt guess x) x)))

; the guess is improved by averaging it with x and the old guess

(define (improve-sqrt guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; We also have to define good-enough. We want to improve the answer until it is
; within a certain range of the square

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; Finally we need a way to start the procedure. We can always guess the square
; of any number is 1.

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Exercise 1.6
; Why does if need to be a special condition? Why can't it be defined in terms
; of cond? Using this new-if, what happens to the sqrt-iter process

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (ex1-6 guess x)
  (new-if (good-enough? guess x)
          guess
          (ex1-6 (improve-sqrt guess x) x)))

; Maximum recursion depth is reached. The reason is because new-if is a regular
; procedure, and therefore it is evaluated in applicative-order. Before new-if
; is called, the then-clause and the else-clause are both evaluated, and 
; because the else-clause is a recursive call, it recurses forever.
; The regular if is evaluated in normal-order, so the else-clause is only
; evaluated if the condition specifies.

; Exercise 1.7
; Our version of good-enough is not very effective for finding the square roots
; of very large and very small numbers. Implement a version of good enough that
; watches how guess changes from one iteration to the next, and stops when the
; change is a very small fraction of the guess

; the old sqrt would result in .03230 for (sqrt 0.0001), and would
; fall into an endless loop for numbers as large as 10000000000000
; adjusting the good-enough procedure to compare old and new guesses solves
; both of these problems

(define (sqrt-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (sqrt-iter guess (improve-sqrt guess x) x)))

(define (good-enough? old-guess guess)
  (< (abs (- old-guess guess)) (/ guess 100000)))

(define (sqrt* x)
  (sqrt-iter 2.0 1.0 x))

; Exercise 1.8
; Implement a cube-root precedure analogous to the square-root procedure using
; newtons method for cube roots. Give a guess y for the root of x, a new guess
; can be found with ((x/y^2)+2y)/3

(define (cbrt-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (cbrt-iter guess (improve-cbrt guess x) x)))

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cbrt* x)
  (cbrt-iter 2.0 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1.8 Procedures as Block-Box Abstractions                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Block structure can be used to hide procedures that only exist to aid another
; procedure, such as the improve and iter procedures for sqrt and cbrt. They
; also introduce lexical scoping, so x would not have to be passed from
; procedure to procedure

(define (cbrt x)
  (define (cbrt-iter old-guess guess)
    (if (good-enough? old-guess guess)
        guess
        (cbrt-iter guess (improve guess))))
  (define (improve guess)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (cbrt-iter 2.0 1.0))
















