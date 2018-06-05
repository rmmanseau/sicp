
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.3.3 Procedures as General Methods                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The half interval method is a way of finding the roots of an equation. By
; taking a two points a and b where f(a) < 0 < f(b), we know f must have a 0
; between a and b. if we average a & b into x, we can create a smaller interval
; that is also guaranteed to have a 0 in it, until the the interval is small
; enough

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                   (search f neg-point midpoint))
                ((negative? test-value)
                   (search f midpoint pos-point))
                (else midpoint))))))

(define (average a b) (/ (+ a b) 2))

(define (close-enough? a b) (< (abs (- a b)) 0.001))

; Because the search procedure doesn't stop you from inputting incorrect
; values, such as two positive numbers, we will use another method that verifys
; the input and then calls search.

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
            (search f a b))
          ((and (negative? b-value) (positive? a-value))
            (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

; The fixed point of a function is when a function f satisfies the equation
; f(x) = x. We can locate the fixed point of some functions by beginning with
; an initial guess and then repeatedly applying f
; f(x), f(f(x)), f(f(f(x))), ...
; until the value doesn't change much.

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

; We can use this to find the fixed point of the cosine function
; (fixed-point cos 1.0)
; .7390822985224024

; or the solution to the equation y = sin(y) + cos(y)
; (fixed-point (lambda (y) (+ (sin y) (cos y)))
;              1.0)
; 1.2587315962971173

; This procedure is reminescent of our sqrt procedure from before, in that it
; improves on a guess until it reaches a certain tolerance. Turns out we can 
; define sqrt in terms of our fixed-point procedure.

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

; Unfortunately, this doesn't converge. Consider the input y1.
; the next input, y2 = x/y1. y3 = x/y2 = x/(x/y1) = y1. This creates
; an infinite loop in which y1 and y2 oscillate about the answer.

; We can prevent this oscillation by preventing the guess from changing so
; drastically. 

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; Exercise 1.35
; Show taht the golden ratio is a fixed point of the transformation
; x -> 1 + 1/x
; Use this to compute the golden ratio via the fixed-point procedure

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

; (golden-ratio) 
; 1.6180327868852458

; Exercise 1.36
; Modifty fixed-point to print the sequence of approximations it generates.

(define (fixed-point-verbose f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (display guess) (newline)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (sqrt-v x)
  (fixed-point-verbose (lambda (y) (average y (/ x y)))
                       1.0))

; Find the solution to x^x = 1000 by finding a fixed point of
; x -> log(1000)/log(x)
; Compare the number of steps this takes with or without average damping

; Without average damping
; (fixed-point-verbose (lambda (x) (/ (log 1000) (log x))) 2.0)
; 2.0
; 9.965784284662087
; 3.004472209841214
; 6.279195757507157
; 3.759850702401539
; 5.215843784925895
; 4.182207192401397
; 4.8277650983445906
; 4.387593384662677
; 4.671250085763899
; 4.481403616895052
; 4.6053657460929
; 4.5230849678718865
; 4.577114682047341
; 4.541382480151454
; 4.564903245230833
; 4.549372679303342
; 4.559606491913287
; 4.552853875788271
; 4.557305529748263
; 4.554369064436181
; 4.556305311532999
; 4.555028263573554
; 4.555870396702851
; 4.555315001192079
; 4.5556812635433275
; 4.555439715736846
; 4.555599009998291
; 4.555493957531389
; 4.555563237292884
; 4.555517548417651
; 4.555547679306398
; 4.555527808516254
; 4.555540912917957
; Value: 4.555532270803653

; With average damping
; (fixed-point-verbose (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
; 2.0
; 5.9828921423310435
; 4.922168721308343
; 4.628224318195455
; 4.568346513136242
; 4.5577305909237005
; 4.555909809045131
; 4.555599411610624
; 4.5555465521473675
; Value: 4.555537551999825

; Exercise 1.37 - 1.39
; Do a whole bunch of stuff dealing with repeating fractions. Too mathy, not
; gonna do em

