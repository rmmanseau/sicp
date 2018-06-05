
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.3.2 Constructing Procedures Using Lambda                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In the last section, our definition of the procedure pi-sum had the trivial
; procedures pi-term and pi-next defined in it.

; (define (pi-sum a b)
;   (define (pi-term x)
;     (/ 1.0 (* x (+ x 2))))
;   (define (pi-next x)
;     (+ x 4))
;   (sum pi-term a pi-next b))

; It seems awkward to have to define a procedure in order to pass it as an
; argument. Instead, we can use lambdas to pass the procedure directly,
; without defining it.

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; We can also use lambdas to clean up our definition of integral

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; As you can see, lambda is used the same way as define, except no name is
; specified for the procedure. Despite not having a name, a lambda is still
; very much a procedure. In fact,

(define (plus4 x) (+ x 4))

(define plus4 (lambda (x) (+ x 4)))

; The above statements are equivalent.

; Lambdas are also useful for creating local variables. Take the function:
; f(x,y) = x(1 + xy)^2 + y(1 - y) + (1 + xy)(1 - y)

; This could also be expressed as
; a = 1 + xy
; b = 1 - y
; f(x,y) = xa^2 + yb + ab

; In writing a procedure for f, it would be useful to be able to define a and
; b as local variables. One way to accomplish this would be to define a helper
; function

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

; However, we could use a lambda to specify an anonymous procedure for binding
; the local variables.

(define (f x y)
  ((lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (+ 1 (* x y))
  (- 1 y)))

; Infact, this construct is so useful that there is a special form called let
; to make using it more convenient.

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; The general form of the let statement is

; (let ((<var-1> <exp-1>)
;       (<var-2> <exp-2>)
;       ...
;       (<var-n> <exp-n>))
;   <body>)

; Which is interpreted as an alternate syntax for

; ((lambda (<var-1> <var-2> ... <var-n>)
;     <body>)
;   <exp-1>
;   <exp-2>
;   ...
;   <exp-n>)

; We could use internal definitions to achieve the same effect as let, but
; for reasons that won't be explained until section 4.1.6, internal defintions
; should only be used for procedures. For variables, let should be preferred.

; Exercise 1.34
; Suppose we define the procedure

(define (f g) (g 2))

; Then we have
;(f square) = 4
;(f (lambda (z) (* z (+ z 1)))) = 6

; What happens if we ask the interpreter to evaluate (f f)?

; I would imagine that it recurses endlessly

; Turns out I imagined wrong.
; (f f) -> (f 2) -> (2 2). (2 2) throws an error because 2 is not a procedure




