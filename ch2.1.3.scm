
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.1.3 What is Meant by Data?                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In general, we can think of data as defined by some collection of selectors
; and constructors together with specified conditions that these procedure
; must fulfill in order to be a valid representation

; Consider the notion of a pair. A pair can be defined as any set of three
; procedures that satisfy the following conditions:
; For any objects x and y, if z is (cons x y) then (car z) is x and (cdr z) is y
; This can be illustrated by the fact that we could implement cons, car, and
; cdr without using any data structures.

(define (cons x y)
  (define (dispatch selection)
    (cond ((= selection 0) x)
          ((= selection 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

; The above implementation is only to prove the point that you do not need
; "real" data structures to represent data. As long as the three procedures
; fullfil the conditions that a pair needs to fulfill, and they are the only
; procedures used to interact with a pair, they are a valid representation

; Exercise 2.4
; This is an alternate representation of pairs. Very that (car (cons x y))
; yields x for any objects x and y

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

; (car (cons 2 5))
; 2

; What is the corresponding definition of cdr?

(define (cdr z)
  (z (lambda (p q) q)))

; Exercise 2.5
; Show that we can represent pairs of nonnegative integers using only numbers
; and arithmetic operations if we represent the pair a and b as the integer
; that is the product (2^a)*(3^b)

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car x)
  (if (= (remainder x 2) 0)
      (+ 1 (car (/ x 2)))
      0))

(define (cdr x)
  (if (= (remainder x 3) 0)
      (+ 1 (cdr (/ x 3)))
      0))

; Exercise 2.6
; Church numerals, mathy sort of thing, maybe do this eventually










