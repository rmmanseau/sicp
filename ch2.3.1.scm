
; 2.3 - Symbolic Data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.1 Quotation                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Suppose we want to construct the list (a b).
; This cannot be accomplished with (list a b), because that will construct a
; list of the values of a and b

; In order to specify if we want the value or the symbol, we use a single
; quotation mark proceding the object

(define a 1)
(define b 2)

; (list a b)
; (1 2)

; (list 'a 'b)
; (a b)

; (list 'a b)
; (a 2)

; Quotation also allows us to type in compound objects.

; (car '(a b c))
; a

; (cdr '(a b c))
; (b c)

; Another additional primitive used in manipulating symbols is eq? which tests
; if two symbols are the same
; eq? can be used to implement a procedure called memq, which will check if
; a symbol is in a list. If it is, it returns the sublist that begins with the
; queried symbol

(define (memq item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

; (memq 'apple '(pear banana prune))
; #f

; (memq 'apple '(x (apple sauce) y apple pear))
; (apple pear)

; Exercise 2.53
; What would the interpreter print in response to evaluating each of the
; following expressions.

; (list 'a 'b 'c)
; (a b c)

; (list (list 'george))
; ((george))

; (cdr '((x1 x2) (y1 y2)))
; ((y1 y2))

; (cadr '((x1 x2) (y1 y2)))
; (y1 y2)

; (pair? (car '(a short list)))
; #f

; (memq 'red '((red shoes) (blue socks)))
; #f

; (memq 'red '(red shoes blue socks))
; (red shoes blue socks)

; Exercise 2.54
; Two lists are said to be equal? if they contain equal elements arranged in
; the same order. For example:

; (equal? '(this is a list) '(this is a list))
; #t

; (equal? '(this is a list) '(this (is a) list)
; #f

; equal? is defined recursively in terms of eq?

(define (equal? a b)
  (cond ((and (not (pair? a))
              (not (pair? b)))
         (eq? a b))
        ((and (pair? a)
              (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else #f)))

; Exercise 2.55
; The interpreter prints quote when given (car ''abracadabra). Explain why.

; (car '(quote abracadabra))
; quote


