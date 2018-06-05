
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2.2 Hierarchical Structures                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The representation of sequences in terms of lists generalizes naturally
; to represent sequences who elements themselves may be sequences.
; For example:

; (cons (list 1 2) (list 3 4))
; ((1 2) 3 4)

; This is a list of three items, the first of which is itself a list, (1 2).
; Another way to think of a sequences of sequences is as tree. The elements
; of each sequence are the branches of the tree, and the elements
; that are themselves sequences are the subbranches of the tree

; ((1 2) 3 4)
;     / \
;    /   \____
; (1 2)   \   \
;  / \     3   4
; 1   2

; Recursion is a very natural tool for dealing with trees, since we can
; often reduce operations on branches into operations on sub branches, until
; we reach the leaves. For example, compare a list length procedure to
; a count leaves procedure

(define x (cons (list 1 2) (list 3 4)))

; (length x)
; 3

; (count-leaves x)
; 4

; (list x x)
; (((1 2) 3 4) ((1 2) 3 4))

; (length (list x x))
; 2

; (count-leaves (list x x))
; 8

; To implement the count leaves procedure we can use a primitive predicate
; called (pair?) to check if its argument is a pair or not.

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


; Exercise 2.24
; Evaluate (list 1 (list 2 (list 3 4)))
; Give the result printed by the interpreter, the box and pointer structure,
; and the interpretation as a tree

; (list 1 (list 2 (list 3 4)))
; (1 (2 (3 4)))

;        (1 (2 (3 4)))
;         ____/ \      
;        1       (2 (3 4))      
;              ____ / \     
;             2        (3 4)
;                       / \
;                      3   4

; Exercise 2.25
; Give combos of cars and cdrs that will pick 7 from each of the following
; lists

; (define x (list 1 3 (list 5 7) 9))
; (car (cdr (car (cdr (cdr x)))))
; 7

; (define x (list (list 7)))
; (car (car 7))
; 7

; (define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))
; 7

; Exercise 2.26
; Suppose we define x and y to be two lists

(define x (list 1 2 3))
(define y (list 4 5 6))

; What result is printed by the interpreter in response to evaluating each of
; the following:

; (append x y)
; (1 2 3 4 5 6)

; (cons x y)
; ((1 2 3) 4 5 6)

; (list x y)
; ((1 2 3) (4 5 6))

; Exercise 2.27
; Modify your reverse procedure from Exercise 2.18 to produce a deep-reverse
; procedure such that:

(define x (list (list 1 2) (list 3 4)))

; x
; ((1 2) (3 4))

; (reverse x)
; ((3 4) (1 2))

; (deep-reverse x)
; ((4 3) (2 1))

(define (deep-reverse items)
  (define (iter a items)
    (if (null? items)
        a
        (iter (cons (if (list? (car items))
                        (deep-reverse (car items))
                        (car items))
                    a)
              (cdr items))))
  (iter (list) items))

; Exercise 2.28
; Write a procedure fringe that takes as argument a tree (represented as a list)
; and returns a list whose elements are all the leaves of the tree arranged in
; left-to-right order. For example

(define x (list (list 1 2) (list 3 4)))

; (fringe x)
; (1 2 3 4)

(define (fringe items)
  (define (iter result items)
    (if (null? items)
        result
        (iter (append result
                      (if (not (pair? (car items)))
                          (list (car items))
                          (fringe (car items))))
              (cdr items))))
  (iter (list) items))

(define y (list 8 (list (list 7 6) 5) 4 (list 3 (list 2 1))))

; y
; (8 ((7 6) 5) 4 (3 (2 1)))

; (fringe (deep-reverse y))
; (1 2 3 4 5 6 7 8)

; Exercise 2.29
; A binary mobile consists of two branches, left and right. Each branch is a
; rod of a certain length from with hangs either a weight or another binary
; mobile. A can represent a mobile as two branches:

(define (make-mobile left right)
  (list left right))

; We can represent a branch as a length and a structure, which can be a number
; if it represents a weight, or another mobile

(define (make-branch length structure)
  (list length structure))

; Part A
; Write cooresponding selectors (left-branch) and (right-branch) which return
; the branches of a mobile, as well as (branch-length) and (branch-structure)
; for the components of the branch

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

; Part B
; Using your selectors, define a procedure total-weight that returns the total
; weight of the mobile

(define x (make-mobile
            (make-branch 4 (make-mobile
                             (make-branch 4 10)
                             (make-branch 6 5)))
            (make-branch 8 16)))

(define (weight? structure)
  (not (pair? structure)))

(define (total-weight structure)
  (if (weight? structure)
      structure
      (+ (total-weight (branch-structure (left-branch structure)))
         (total-weight (branch-structure (right-branch structure))))))

; Part C
; A mobile is said to be balanced if the torque applied by its top-left
; branch is equal to that applied by its top-right branch. torque is
; the length of the branch multiplied by the weight hanging from the rod.

(define (branch-torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? structure)
  (if (weight? structure)
      #t
      (let ((lb (left-branch structure))
            (rb (right-branch structure)))
        (and (= (branch-torque lb)
                (branch-torque rb))
             (balanced? (branch-structure lb))
             (balanced? (branch-structure rb))))))

; (balanced? x)
; #f

(define y (make-mobile
            (make-branch 6 (make-mobile
                             (make-branch 2 2)
                             (make-branch 2 2)))
            (make-branch 4 6)))

; (balanced? y)
; #t

; Part D
; Suppose we change the representation of mobiles so that the constructors are:

; (define (make-mobile left right) (cons left right))
; (define (make-branch length structure) (cons length structure))

; How much do you need to change your programs to convert to the new
; representation?

; I would only need to change (right-branch) and (branch-structure) so that
; they use (cdr) instead of (cadr). Because the rest of the procedures are in
; the terms of the selectors and constructors, everything else would work as is.

; Mapping over trees
; Just as map is useful for dealing with sequences, map paired with recursion is
; useful for dealing with trees. For instance, take a procedure that scales a
; tree

(define (scale-tree tree factor)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

; (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
; (10 (20 (30 40) 50) (60 70))

; Another way to implement this is to regard the tree as a sequence of
; sub-trees and use map.

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; Many tree operations can be implemented by similar combinations of
; sequence operations and recursion

; Exercise 2.30
; Define, both directly and via map/recursion, a square-tree procedure

(define tree30 (list 1
                     (list 2 (list 3 4) 5)
                     (list 6 7)))

(define (square-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

; (square-tree tree30)
; (1 (4 (9 16) 25) (36 49))

; Exercise 2.31
; Abstract your answer to the last exercise into a procedure tree-map, such
; that square-tree could be defined as:

; (define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

; (tree-map square tree30)
; (1 (4 (9 16) 25) (36 49))

; Exercise 2.32
; Given a set (1 2 3), the set of all of its subsets is
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
; Complete the following definition that generates subsets, and explain why
; it works.

; (define (subsets s)
;   (if (null? s)
;       (list (list))
;       (let ((rest (subsets (cdr s))))
;         (append rest (map <??> rest)))))

; This a bunch of set theory bs, another exercise thats more math than
; compsci











