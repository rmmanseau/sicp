
; 2.2 - Hierarchical Data and the Closure Property

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2.1 Representing Sequences                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; One useful structure we can build with pairs is a sequence: an ordered
; collection of data objects. There are many ways to represent a list, but
; the following is the straightforward way we're going to use.

; (cons 1
;       (cons 2
;             (cons 3
;                   (cons 4 nil))))

; Such a sequence of pairs is called a list, and scheme provides a primitive
; called list to help in constructing lists. The above sequences could be
; produced by

; (list 1 2 3 4)

; In the case of list, we can think of car as selecting the first item in
; the list, and cdr as selecting the sublist containing all but the first item

; (car (list 1 2 3 4))
; 1

; (cdr (list 1 2 3 4))
; (2 3 4)

; (car (cdr (list 1 2 3 4)))
; 2

; Because nested applications of car and cdr (as above) are so common, lisp
; dialects provide the abbreviations cadr, caddr, and cadddr.

; (caddr (list 1 2 3 4))
; 3

; You can also use cons to append elements to the beginning of a list

; (cons 5 (list 1 2 3 4))
; (5 1 2 3 4)

; List Operations
; lists come accompanied by conventional programming techniques for
; manipulating them. For example, the procedure list-ref takes a list and a
; number n as argument, and returns the nth element of the list. It is
; customary to number the elements of the list beginning with 0.

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

; (list-ref squares 3)
; 16

; Scheme includes a primitive null? that tests if its argument is nil, or
; an empty list. The procedure length makes use of null

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7 9))

; (length odds)
; 5

; This implementation of length is recursive. The iterative version is as
; follows

(define (legnth items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ count 1))))
  (length-iter items 0))

; Another useful procedure is append, which will combine two lists. 

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; Exercise 2.17
; Define a procedure last-pair that returns the list that contains only
; the last element of a given (nonempty) list:

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

; Exercise 2.18
; Define a procedure reverse that takes a list as argument and returns a list
; of the same elements in reverse order:

; Less than optimal first attempt:
; (define (reverse items)
;   (define (iter a n)
;     (let ((len (length items)))
;       (if (= n len)
;           a
;           (iter (cons (list-ref items n) a) (+ n 1)))))
;   (iter (list) 0))

(define (reverse items)
  (define (reverse-iter a items)
    (if (null? items)
        a
        (reverse-iter (cons (car items) a) (cdr items))))
  (reverse-iter (list) items))

; Exercise 2.19
; Consider the change-counting program of section 1.2.2. Now that we know
; about lists, we can update it to take a list of coin values.

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; We could then call cc as follows
; (cc 100 us-coins)
; 292

; This is our new cc procedure
; Define the procedures first-denomination, except-first-denomination,
; and no-more? in terms of primitive operations on list structures.

(define (cc amount coin-values)
  (define (first-denomination coin-values)
    (car coin-values))
  (define (except-first-denomination coin-values)
    (cdr coin-values))
  (define (no-more? coin-values)
    (null? coin-values))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                (except-first-denomination
                  coin-values))
             (cc (- amount
                    (first-denomination
                      coin-values))
                 coin-values)))))

; The order of the list coin-values does not affect the answer produced by
; cc because cc evaluates every possible combination.

; Exercise 2.20
; The procedures +, *, and list take arbitrary numbers of arguments. One way to
; define such procedures is to use define with dotted-tail notation.

; (define (f x y . z) <body>)

; In the above definition, x and y will be the first two arguments, and
; z will contain a list of the rest of the arguments. for example

; (f 1 2 3 4 5 6)
; x = 1
; y = 2
; z = (3 4 5 6)

; (define (g . w) <body>)

; In the above definition, g can be called with zero or more arguments

; Use this notation to write a procedure same-parity that takes
; one or more integers and returns a list of all the arguments that
; have the same even-odd parity as the first argument

(define (same-parity first . rest)
  (let ((parity (remainder first 2)))
    (define (iter new rest)
      (if (null? rest)
          new
          (iter (if (= parity (remainder (car rest) 2))
                    (append new (list (car rest)))
                    new)
                (cdr rest))))
    (iter (list first) rest)))

; Mapping Over Lists
; One extremely useful operation is to apply some transformation to each
; element in a list and generate the list of results, like the same-parity
; function above does.

(define (scale-list items factor)
  (if (null? items)
      (list)
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

; This operation is so useful that we will abstract it into a higher order
; procedure called map

(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items)))))

; We can now redefine scale-list in terms of our map function

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items)) 

; Exercise 2.21
; The procedure square-list takes a list of numbers as argument and returns
; a list of the squares of those numbers. Write the procedure first without
; map, and then with map

(define (square-list items)
  (if (null? items)
      (list)
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (square x))
       items))

; Exercise 2.22
; Rewriting the first square-list procedure from the last exercise so that it
; is iterative produces the answer list in the reverse order of the one desired.
; Why does that happen?

(define (square-list items)
  (define (iter items answer)
    (if (null? items)
        answer
        (iter (cdr items)
              (cons (square (car items))
                    answer))))
  (iter items (list)))

; Each iteration appends the next mapped item onto the front of the list, which
; ends up reversing the order.

; iter0
; items  - 1 2 3
; answer - 

; iter1
; items  - 2 3
; answer - 1

; iter2
; items  - 3
; answer - 4 1

; iter3
; items  - 
; answer - 9 4 1

; One might think the solution would be to switch the arguments of cons. 
; This doesn't work either. Explain.

(define (square-list items)
  (define (iter items answer)
    (if (null? items)
        answer
        (iter (cdr items)
              (cons answer
                    (square (car items))))))
  (iter items (list)))

; Doing this fixes the order, but does not generate the standard structure
; for a list. A list is scheme is defined as:
; (cons 1 (cons 4 (cons 9 (cons 16 nil))))

; This procedure generates the following structure:
; (cons (cons (cons (cons nil 1) 4) 9) 16)

; This can be seen by evaluating the above square-list procedure
; (square-list (list 1 2 3 4))
; ((((() . 1) . 4) . 9) . 16)

; Exercise 2.23
; The procedure for-each is similar to map. It takes as arguments a procedure
; and a list of elements. However, rather than returning a list, for-each
; just applies the procedure to each of the elements in turn, from left
; to right. This can be useful for procedures that perform an action, such
; as print

; (for-each (lambda (x) (newline) (display x))
;           (list 57 321 88))

; Give an implementation of for-each

(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

(define (print-list items)
 (for-each (lambda (x) (newline) (display x))
           items))


  


