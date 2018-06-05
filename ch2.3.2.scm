
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.2 Example: Symbolic Differentiation                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To illustrate symbol manipulation and further illustrate data abstraction,
; consider the design of a procedure that performs symbolic differentiation of
; algebraic expressions. We would like the procedure to take an algebraic
; expression and a variable, and return the derivative of the expression with
; respect to the variable.

; We will first consider a very simple program that handles expression that
; are built up using only the operations of addition and multiplication with
; two arguments.

; We can differentiate any such expression by applying the following reduction
; rules

; rule 1: dc/dx = 0 (derivative of a constant with respect to x is 0)
; rule 2: dx/dx = 1 (derivative of x with respect to x is 1)
; rule 3: d(u+v)/dx = du/dx + dv/dx
; rule 4: d(uv)/dx  = u(dv/dx) + v(du/dx)

; Note that the last two rules are recursive

; We will assume we have the procedures to implement the following
; selectors, constructors, and predicates

; (variable? e)
; (same-variable? v1 v2)
; (sum? e)
; (addend s)
; (augend s)
; (make-sum a1 a2)
; (product? e)
; (multiplier p)
; (multiplicand p)
; (make-product m1 m2)

; Using these and the primitive predicate (number?), we can express the
; differentiation rules as the following procedure

(define (deriv exp var)
  (cond ((number? exp) 0)                                   ; rule 1
        ((variable? exp) (if (same-variable? exp var) 1 0)) ; rule 2
        ((sum? exp) (make-sum (deriv (addend exp) var)      ; rule 3
                              (deriv (augend exp) var)))
        ((product? exp)                                     ; rule 4
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        (else                                               ; else
          (error "unknown expression type: DERIV" exp))))

; This procedure is expressed in terms of abstract data, so it will work
; no matter how we choose to represent algebraic expressions, as long as we
; design a proper set of selectors and constructors

; You can imagine many way sto use list structures to represent algebraic
; expressions, but the most straightforward choice would be the same
; parenthesized prefix notation that lisp uses for combinations.

; We will choose to represent ax + b as (+ (* a x) b)

; In our representation, variables are symbols. They are identified with the
; primitive predicate (symbol?)
(define (variable? e) (symbol? e))

; Two variables are the same if the symbols representing them are eq?
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; Sums and products are constructed as lists
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

; A sum is a list whose first element is the symbol +
(define (sum? e) (and (pair? e) (eq? (car e) '+)))

; The addend is the second item of the sum list
(define (addend s) (cadr s))

; The augend is the third item
(define (augend s) (caddr s))

; A product is a list whose first element is the symbol *
(define (product? e) (and (pair? e) (eq? (car e) '*)))

; The multiplier is the second item of the product list
(define (multiplier p) (cadr p))

; The multiplicand is the third
(define (multiplicand p) (caddr p))

; With these procedures defined, we now have a working symbolic derivation 
; procedure.

; (deriv '(+ x 3) 'x)   D(x + 3) = 1 + 0
; (+ 1 0)

; (deriv '(* x y) 'x)   D(xy) = y
; (+ (* x 0) (* 1 y))

; The answers generated are correct, but they are unsimplified. Ideally the
; second example would return y, instead of (+ (* x 0) (* 1 y))

; This problem is reminescent of our rational number implementation. At first it
; did not reduce the rational numbers before returning them. Our solution then
; was to change the constructors and selectors of the implementation. We will
; do the same thing now.

; We want the change make-sum so that if both summands are numbers, make-sum
; will add them and return their sum. Also, if one summand is 0, then make-sum
; will return the other summand.

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; (deriv '(+ x 3) 'x)
; 1

; (deriv '(* x y) 'x)
; y

; (deriv '(* (* x y) (+ x 3)) 'x)   D(xy * (x+3)) = (y * x+3) + (xy * 1)
; (+ (* x y) (* y (+ x 3)))                       = xy + y(x+3)

; Exercise 2.56
; Show how to extend the basic differentator to handle more kinds of
; expression. For instance, implement the differentiation rule:

; d(u^n)/dx = nu^(n-1) du/dx

; by adding a new clause to the deriv program and defining appropriate
; procedures (exponentiation? e), (base e), (exponent e),
; and (make-exponentiation b e)

(define (deriv exp var)
  (cond ((number? exp) 0)                                   ; rule 1
        ((variable? exp) (if (same-variable? exp var) 1 0)) ; rule 2
        ((sum? exp) (make-sum (deriv (addend exp) var)      ; rule 3
                              (deriv (augend exp) var)))
        ((product? exp)                                     ; rule 4
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
          (make-product                                     ; rule 5
            (make-product (exponent exp)
                          (make-exponentiation
                            (base exp)
                            (- (exponent exp) 1)))
            (deriv (base exp) var)))
        (else                                               ; else
          (error "unknown expression type: DERIV" exp))))

(define (** b e) (expt b e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (exponentiation? e) (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

; (deriv '(** x 2) 'x)  D(x^2) = 2x
; (* 2 x)

; (deriv '(** x 4) 'x)  D(x^4) = 4x^3
; (* 4 (** x 3))

; Exercise 2.57
; Extend the differentiation program to handle sums and products of arbitrary
; numbers of (two or more) terms. Then the last of the 3 examples could be
; expressed as (deriv '(* x y (+ x 3)) 'x)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (sum sequence)
  (accumulate + 0 sequence))

(define (product sequence)
  (accumulate * 1 sequence))

(define (make-sum . values)
  (let ((symbols (filter (lambda (x) (not (number? x))) values))
        (number (sum (filter number? values))))
    (cond ((null? symbols) number)
          ((and (null? (cdr symbols)) (= number 0)) (car symbols))
          ((= number 0) (cons '+ symbols))
          (else (cons '+ (append symbols (list number)))))))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (make-product . values)
  (let ((symbols (filter (lambda (x) (not (number? x))) values))
        (number (product (filter number? values))))
    (cond ((= number 0) 0)
          ((null? symbols) number)
          ((and (null? (cdr symbols)) (= number 1)) (car symbols))
          ((= number 1) (cons '* symbols))
          (else (cons '* (append symbols (list number)))))))

(define (multiplicand s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '* (cddr s))))

; The following is a slight refactoring of the exponentation rule to take
; advantage of the ability to pass more than two arguments to make-product

(define (deriv exp var)
  (cond ((number? exp) 0)                                   ; rule 1
        ((variable? exp) (if (same-variable? exp var) 1 0)) ; rule 2
        ((sum? exp) (make-sum (deriv (addend exp) var)      ; rule 3
                              (deriv (augend exp) var)))
        ((product? exp)                                     ; rule 4
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
          (make-product                                     ; rule 5
            (exponent exp)
            (make-exponentiation
              (base exp)
              (- (exponent exp) 1))
            (deriv (base exp) var)))
        (else                                               ; else
          (error "unknown expression type: DERIV" exp))))

; (deriv '(* x y (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))

; Exercise 2.58
; 







