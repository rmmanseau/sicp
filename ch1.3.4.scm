
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.3.3 Procedures as Returned Values                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In the previos section we observed that sqrt(x) is a fixed-point of the
; function y -> x/y
; We then used average damping to make the approximation converge. We can
; express the idea of average damping using a procedure

(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

; Average damp is a procedure that takes a procedure as its argument and
; returns a procedure

; Using average-damp, we can reformulate our sqrt procedure from the last
; section

(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; Notice that this formulation of sqrt makes explicit the three ideas in the
; method. Fixed-point search, average damping, and the function y -> x/y
; While the process expressed by this procedure and the one in section 1.1.7
; are the same, this one is much cleaner.
; Breaking down procedures into smaller entities that can be reused is
; incredibly valuable. As an example notice how easy it is to reformulate
; our square root procedure into a cube root procedure, after noticing that
; the cube root is the fixed point of the function y -> x/y^2

(define (cbrt x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; When we first inroduced the square-root procedure, we mentioned it was a
; special case of Newton's Method.

; Newtons method is as follows:
; If                                  x -> g(x)
; is a differentiable function
; then a solution to the equation     g(x) = 0
; is a fixed point of the function    f(x) = x - g(x)/Dg(x)
; where Dg(x) is the derivative
; of g evaluated at x

; In order to implement newtons method as a procedure, we must first
; express the idea of a derivative
; If g is a function and dx is a small number, then
; Dg(x) = ( g(x + dx) - g(x) )/dx

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

; Like average-damp, deriv is a procedure that takes a procedure as argument
; and returns a procedure as value.
; As an example, the derivative of x^3 is 3x^2. D(x^3) evaluated at 5 would
; then be 3*(5^2) = 75

(define (cube x) (* x x x))

; ((deriv cube) 5)
; 75.00014999664018

; With deriv now defined, we can express the newtons method as a fixed-point
; process

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; We can now find the square root of x in terms of newtons method by finding
; a zero of the function y -> y^2 - x, starting with an initial guess of 1

(define (sqrt-newt x)
  (newtons-method
    (lambda (y) (- (square y) x)) 1.0))

; Weve now seen two ways to express the square root computation as an instance
; of a more general method. Once as a fixed point search and once using newtons
; method. Since newtons method is itself expressed as a fixed-point process,
; we actually saw two ways to compute square roots as fixed roots.
; Each method begins with a function and finds a fixed point of some
; transformation of the function. We can express this general idea as
; a procedure

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; Using this new method, we can express our two earlier sqrt procedures in
; even more general terms

(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (- (square y) x)) newton-transform 1.0))

; Exercise 1.40
; Define a procedure cubic that can be used together with the newtons-method
; procedure in expressions of the form
; (newtons-method (cubic a b c) 1)
; to approximate zeros of the cubic x^3 + ax^2 + bx + c

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; (newtons-method (cubic 3 -2.4 6) 1)
; -3.9813366488302706
; Example numbers to check answer pulled from billthelizards solution

; Exercise 1.41
; Define a proceure double that takes a procedure with one argument as
; argument, and returns a procedure that applies the original procedure twice.
; If inc is a procedure that adds 1, (double inc) should be a procedure that
; adds 2

(define (double f)
  (lambda (x) (f (f x))))

; (((double (double double)) inc) 5)
; 21

; Exercise 1.42
; Let f and g be two one-argument procedures. The composition f after g is
; defined to be the function x -> f(g(x)). Define a procedure compose that
; implements composition.

(define (compose f g)
  (lambda (x) (f (g x))))

; ((compose square inc) 6)
; 49

; Exercise 1.43
; Write a function that repeats a procedure a specified number of times, such
; that
; ((repeated inc 5) 2) -> 7
; ((repeated square 2) 5) -> 625

(define (repeated f n)
  (cond ((= n 1) f)
        (else (compose f (repeated f (- n 1))))))

; This is a weird backwards procedure I made because I misunderstood the prompt
; originally. Rather than
; ((repeated square 2) 5)
; generating
; (square (square 5))
; it generates
; (square (square (square (square (square 2)))))
; It also happens to be iterative

; (define (repeated f x)
;   (define (repeat n)
;     (define (iter g n)
;       (cond ((= n 1) (g x))
;             (else (iter (compose f g) (- n 1)))))
;     (iter f n))
;   repeat)

; Exercise 1.44
; The idea of smoothing a function is important to signal processing. If f
; is a function and dx is some small number, then the smoothed version of
; f is the function whose value at a point x is the average of
; f(x - dx), f(x), and f(x + dx). Write a procedure that takes a function f
; and returns a function to compute smoothed f

(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx))
                    (f x)
                    (f (- x dx)))
                 3)))

(define (n-smooth f n)
  ((repeated smooth n) f))

; I have no data to test these on, but I checked online and they are correct

; Exercise 1.45
; We saw that computing square roots by finding a fixed point of y -> x/y does
; not converge, so we introduced average damping. The same method works for
; finding cube roots as fixed points of the average damped y -> x/y^2.
; This doesn't work for fourth roots, as one average damp isnt enough. You have
; to average damp twice. Experiment to figure out how many average damps
; are required to compute the nth root as a fixed point search of y -> x/y^(n-1)
; then write a procedure using fixed-point, average-damp, and repeated.

; This function will let me gather data on how many damps are needed to
; converage as n increases

(define (nth-root-explicit x n damps)
  (fixed-point-of-transform
    (lambda (y) (/ x (expt y (- n 1)))) (repeated average-damp damps) 1.0))

; root | average-damps needed to converge
; 2    | 1
; 3    | 1
; 4    | 2
; 5    | 2
; 6    | 2
; 7    | 2
; 8    | 3
; 15   | 3
; 16   | 4

; It appears that the number of damps is equal to the floor of log2 of n

(define (log2 n)
  (/ (log n) (log 2)))

(define (nth-root x n)
  (fixed-point-of-transform
    (lambda (y) (/ x (expt y (- n 1))))
    (repeated average-damp
              (floor (log2 n)))
    1.0))

; Exercise 1.46
; Many of the numerical methods described in this chapter are instances of
; the extremely general computational strategy known as iterative improvement.
; Write a procedure iterative-improve that takes two procedures as arguments:
; one for improving the guess and another for checking if the guess is good
; enough. It should return as its value a procedure that takes a guess as
; argument and keeps improving the guess until it is good enough.

; (define (fixed-point f first-guess)
;   (let ((tolerance 0.00001))
;     (define (close-enough? v1 v2)
;       (< (abs (- v1 v2)) tolerance))
;     (define (try guess)
;       (let ((next (f guess)))
;         (if (close-enough? guess next)
;             next
;             (try next))))
;     (try first-guess)))

; (define (cbrt x)
;   (define (cbrt-iter old-guess guess)
;     (if (good-enough? old-guess guess)
;         guess
;         (cbrt-iter guess (improve guess))))
;   (define (improve guess)
;     (/ (+ (/ x (square guess))
;           (* 2 guess))
;        3))
;   (cbrt-iter 2.0 1.0))

; Rewrite the above procedures in terms of your new iterative-improve procedure

(define (iterative-improve improve good-enough?)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (cbrt x)
  ((iterative-improve
     (lambda (guess) (average guess (/ x (square guess))))
     (lambda (guess) (< (abs (- (cube guess) x))
                     0.001)))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (f guess))
    (lambda (guess) (< (abs (- (f guess) guess))
                       0.00001)))
   first-guess))

; YEET
; CH1 DONE
