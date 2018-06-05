
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2.3 Sequences as Conventional Interfaces                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following procedure

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)
          (if (odd? tree)
              (square tree) 0)))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

; On the surface, this procedure differs greatly from the following one,
; which constructs a list of all the even Fibonacci numbers Fib(k), where
; k is less than or equal to a given integer

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        (list)
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

; Despite the fact that these two procedures are structurally different,
; a more abstract description of the two computations reveals a great
; deal of similarity

; sum-of-squares
; enumerate   -> filter -> map    -> accumulate
; tree leaves    odd?      square    +, 0

; even-fibs
; enumerate   -> map    -> filter -> accumulate
; integers       fib       even?  -> cons, ()

; Both of these procedures can be though of in terms of the parts above,
; but their definitions fail to exhibit the signal-flow structure. For example,
; in sum-of-squares, enumeration is implemented partly by the null? and pair?
; tests and partly by the trees structure. Accumulation is found partly in the
; tests and partiy in the addition used in the recursion. The parts that make
; up the signal flow structure are not distinct

; Sequences Operations
; Programs can be organized into signal-flow structures by representing the
; signals as lists. We can then use list operations to implement the processing
; at each stage.

; We already have a map procedure

; (map square (list 1 2 3 4 5))
; (1 4 9 16 25)

; Now we need a procedure to filter and accumulate a list

(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; (filter odd? (list 1 2 3 4 5))
; (1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; (accumulate cons (list) (list 1 2 3 4 5))
; (1 2 3 4 5)

; (accumulate + 0 (list 1 2 3 4 5))
; 15

; (accumulate * 1 (list 1 2 3 4 5))
; 120

; Finally, we need procedures to enumerate the sequence of elements to be
; processed. For even-fibs, we need to generate a sequence of integers in
; a given range

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

; (enumerate-interval 2 7)
; (2 3 4 5 6 7)

; To enumerate the leaves of a tree, we can use the following (performs
; the same thing as the fringe function from exercise 2.28)

(define (enumerate-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; (enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; (1 2 3 4 5)

; We can now reformulate sum-odd-squares and even-fibs as in the signal-flow
; diagrams

(define (sum-odd-squares tree)
  (accumulate
    + 0 (map square (filter odd? (enumerate-tree tree)))))

(define x (list (list 1 (list 5 2)) 3 (list 4 (list 7 8) 9)))

; (sum-odd-squares x)
; 165

(define (even-fibs n)
  (accumulate
    cons
    (list)
    (filter even? (map fib (enumerate-interval 0 n)))))

; Modular construction is a powerful strategy for controlling complexity, as
; well as providing reusability. For instance, we can reuse pieces from
; sum-of-squares and even-fibs to create a program that constructs a list
; of the squares of the first n + 1 fibonacci numbers

(define (list-fib-squares n)
  (accumulate
    cons
    (list)
    (map square (map fib (enumerate-interval 0 n)))))

; (list-fib-squares 10)
; (0 1 1 4 9 25 64 169 441 1156 3025)

; We can rearrange the pieces and use them in computing the product
; of the squares of the odd integers in a sequence

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

; (product-of-squares-of-odd-elements (list 1 2 3 4 5))
; 225

; We can also formulate conventional data-processing applications in terms of
; sequence operations. Suppose we have a sequence of personnel records and
; we want to find the salary of the highest-paid programmer. Assume
; that we have a selector salary and a predicate programmer?

(define (salary-of-highest-paid-programmer records)
  (accumulate max 0 (map salary (filter programmer? records))))

; Exercise 2.33
; Fill in the missing expressions to complete the following definition of some
; basic list-manipulation operations as accumulations

(define (map33 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))

(define (append33 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length33 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; Exercise 2.34
; Evaluating a polynomial in x at a given value of x can be formulated as
; an accumulation. We evaluate the polynomial

; AnX^n + An-1X^n-1 + ... + A1X + A0

; Using Horner's rule, which structures the computation as

; (...(AnX + An-1)x + ... + A1)X + A0

; Fill in the following template to produce a procedure that evaluates a
; polynomial using Horner's rule. Assume the coefficients are arranged in a
; sequence from A0 to An

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
                      0
                      coefficient-sequence))

; 1 + 3x + 5x^3 + x^5 at x=2
; 1 + 3*2 + 5*8 + 32
; 1 + 6 + 40 + 32
; 79

; (horner-eval 2 (list 1 3 0 5 0 1))
; 79

; Exercise 2.35
; Redefine count-leaves from section 2.2.2 as an accumulation

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree t))))

; (count-leaves x)
; 8

; Exercise 2.36
; The procedure accumulate-n is similar to accumulate except that it takes as
; its third argument a sequence of sequences, which are all assumed to have the
; same number of elements. It then accumulates each cooresponding element in
; each sequence, returning another list. Fill in the missing expressions in
; the following definition

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Exercise 2.37
; Suppose we represent vectors v=(Vi) as sequences of numbers, and matrices
; m=(Mij) as sequences of vectors. For example

; [ 1 2 3 4 ]
; [ 4 5 6 6 ]
; [ 6 7 8 9 ]

; is represented as

; ((1 2 3 4) (4 5 6 6) (6 7 8 9))

; With this representation, we can use sequence operations to concisely
; express the basic matrix and vector operations

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for computing
; the other matrix operations.

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

; ((1 1 1)  * (1 2 3) = (6 12 18)
;  (2 2 2)
;  (3 3 3))

(define (transpose m)
  (accumulate-n cons (list) m))

; ((1 2 3) T = ((1 4 7)
;  (4 5 6)      (2 5 8)
;  (7 8 9))     (3 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row-m) (matrix-*-vector cols row-m)) m)))

; ((2 2 2)  * ((1 2 3)  = ((24 30 36)
;  (2 2 2)     (4 5 6)     (24 30 36)
;  (2 2 2))    (7 8 9))    (24 30 36))

; Exercise 2.38
; The accumulate procedure is also known as fold-right, because it combines
; the first element of the sequence with the result of combining all the
; elements to the right. There is also a fold-left that works in the opposite
; direction

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

; What are the values of:

; (fold-right / 1 (list 1 2 3))
; 1/(2/3) = 3/2

; (fold-left / 1 (list 1 2 3))
; (1/2)/3 = 1/6

; (fold-right list (list) (list 1 2 3))
; (list 1 (list 2 (list 3 (list))))
; (1 (2 (3 ())))

; (fold-left list (list) (list 1 2 3))
; (list (list (list (list) 1) 2) 3)
; (((() 1) 2) 3)

; Give a property that op should satisfy to guarantee that fold-right and
; fold-left will produce the same values for any sequence

; op should satisfy the commutitive property, for example

; 2 * 3  = 3 * 2, so * should produce the same values
; 2 / 3 != 3 / 2, so / should not produce the same values

; Exercise 2.39
; Complete the following definitions of reverse in terms of fold-right and
; fold left

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))

; Nested Mappings
; We can extend the sequence paradigm to include many computations that are
; commonly expressed using nested loops. Consider the problem: Given a positive
; integer n, find all ordered pairs of distinct positive integers i and j,
; where 1 <= j < i <= n, such that i + j is prime. For example, if n is 6

;  i  | 2  3  4  4  5  6  6  
;  j  | 1  2  1  3  2  1  5
; ----+---------------------
; i+j | 3  5  5  7  7  7  11

; First we need a procedure to generate all pairs of i and j for a given n

(define (unique-pairs n)
  (accumulate
    append (list) (map (lambda (i)
                         (map (lambda (j) (list i j))
                              (enumerate-interval 1 (- i 1))))
                       (enumerate-interval 1 n))))

; The combination of mapping and accumulating with append is so common in this
; sort of program that we will isolate it as a seperate procedure

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

; Now we need to filter the sequence of pairs to find those whose sum is prime.

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor (+ test-divisor 1)))))
    (find-divisor 2))
  (define (divides? a b) (= (remainder b a) 0))
  (= (smallest-divisor n) n))

; Finally, we need to gereate the sequence of results

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; Combining all the above steps yields a complete procedure

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; Nested mappings are also useful for sequences other than those that enumerate
; intervals. Suppose we wish to generate all the permutations of a set S.
; This is achieved by taking each item x in S and recursively generating the
; sequence of permutations of S - x, and adjoining x to the front of each one.

(define (permutations s)
  (if (null? s)
      (list ())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item list)
  (filter (lambda (x) (not (= x item)))
          list))

; Exercise 2.40
; Define a procedure unique-pairs that, given an integer n, generates the
; sequence of pairs (i, j) with 1 <= j < i <= n. Use it to simplify
; prime-sum-pairs

; I already did that during the section I think

; Exercise 2.41
; Write a procedure to find all ordered triples of distinct positive integers
; i, j, and k less than or equal to a given integer n that sum to a given
; integer s

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples-with-sum n s)
  (filter (lambda (triple) (= (+ (car triple)
                                 (cadr triple)
                                 (caddr triple))
                              s))
          (unique-triples n)))

; Exercise 2.42
; The eight queen puzzle asks how to place eight queens on an 8x8 chessboard so
; that no queen is in check from any other queen. We can generate all solutions
; to this puzzle recursively, by generating a sequence of all possible ways
; to place k-1 queens in the first k-1 columns on the board. Next we place
; a queen in each row of the kth column and filter these, keeping only the
; positions for which the queen in the kth column is safe.

; We implement this solution as a procedure queens, which returns a sequence of
; all solutions to the problem of placing n queens on an nxn chessboard.

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position
                        new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

; Complete the program by implementing the representation for sets of board
; positions, including the procedures:
; (adjoin-position) adjoins a new row-column position to a set of positions
; (empty-board)     represents an empty set of positions
; (safe?)           determines for a set of positions whether the queen in the
;                   kth column is safe with respect to the others

(define empty-board (list))

(define (adjoin-position row column positions)
  (append (list (list row column)) positions))

(define (queen-in-column column positions)
  (car (filter (lambda (position) (= (cadr position)
                                     column))
               positions)))

(define (queens-before-column column positions)
  (filter (lambda (position) (< (cadr position)
                                column))
          positions))

(define (queen-row queen) (car queen))
(define (queen-col queen) (cadr queen))

(define (safe? column positions)
  (let ((queen (queen-in-column column positions)))
    (null? (filter (lambda (other) (or (= (queen-row queen) (queen-row other))
                                       (= (abs (- (queen-row queen) (queen-row other)))
                                          (abs (- (queen-col queen) (queen-col other))))))
                   (queens-before-column column positions)))))

; (queens 4)
; (((3 4) (1 3) (4 2) (2 1)) ((2 4) (4 3) (1 2) (3 1)))

; Im gonna make a procedure that prints out the boards because its pretty hard
; to interpret the large list of pairs

(define (repeated-display message n)
  (cond ((> n 0)
          (display message)
          (repeated-display message (- n 1)))))

(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

(define (formatted-queens board-size)
  
  (define (display-row queen-pos)
    (repeated-display ". " (- queen-pos 1))
    (display "Q ")
    (repeated-display ". " (- board-size queen-pos)))
  
  (define (column-of-queen-in-row row positions)
    (cadar (filter (lambda (position) (= (car position)
                                         row))
                   positions)))
  
  (define (display-board positions)
    (define (iter row)
      (cond ((<= row board-size)
              (display-row (column-of-queen-in-row row positions))
              (newline)
              (iter (+ row 1)))))
    (iter 1))
  
  (for-each (lambda (board) (newline)
                            (display-board board))
            (queens board-size)))

; (formatted-queens 4)

; . . Q . 
; Q . . . 
; . . . Q 
; . Q . . 

; . Q . . 
; . . . Q 
; Q . . . 
; . . Q . 

; I can verify that my solution is correct by comparing to the online
; encyclopedia of integer sequences

; Number of ways of placing n nonattacking queens on an n X n board.
; 1, 0, 0, 2, 10, 4, 40, 92, 352, 724, ...

; (length (queens 1))
; 1
; (length (queens 2))
; 0
; (length (queens 3))
; 0
; (length (queens 4))
; 2
; (length (queens 5))
; 10
; (length (queens 6))
; 4
; (length (queens 7))
; 40
; (length (queens 8))
; 92
; (length (queens 9))
; 352
; (length (queens 10))
; 724

; Exercise 2.43
; Explain why interchanging the order of the nested mappings in the flatmap of
; queens causes the program to run much slower. How long will the interchanged
; program take to solve the 8 queen puzzle if the previous exercise takes T.

; (flatmap
;   (lambda (new-row)
;     (map (lambda (rest-of-queens)
;            (adjoin-position new-row k rest-of-queens))
;          (queen-cols (- k 1))))
;   (enumerate-interval 1 board-size))

; The reason it takes so long is because queen-cols is a very expensive
; procedure to run. In the last exercise, it is only called once per column.
; With the flatmap interchanged, queen-cols is called once for each row in
; each column. This creates a tree-recursive process, which we've learned
; grows exponentially.
; If the last exercise solution takes T time, this solution should take
; T^board-size time



