
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.3 Example: Representing Sets                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In this example we are going to make a representation for a set. Simply put,
; a set is a collection of distinct objects. To give a more precise definition,
; we will specify the operations that are to be used on a set. These are:

; union-set         - a set containing each object in both of the sets
; intersection-set  - the set made up only of the objects in both sets
; element-of-set?   - checks if an object is an element of a set
; adjoin-set        - adds an object to a set

; Sets as unordered lists
; One way to represent a set is as a list of its elements in which no
; element appears more than once. An empty set is represented by an empty list

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; Using this procedure, we can write adjoin-set. If the object to be adjoined
; is already in the set, we just return the set. Otherwise, we use cons to add
; the object to the list that represents the set

(define (adjoin-set x set)
  (if   (element-of-set? x set)
        set
        (cons x set)))

; For intersection-set we can use a recursive strategy. If we know how to form
; the intersection of set2 and the cdr of set1, we only need to decide whether
; to include the car of set1 in this. But this depends on whether (car set1) is
; also in set2. Here is the resulting procedure.

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; One of the issues to be concerned with in designing a representation is
; efficiency. Consider the number of steps required by out set operations.
; They all use element-of-set? In a set with n elements, the worst case scenario
; is n steps. intersection-set does an element-of-set? on each element of set1,
; resulting in n^2 steps.

; Exercise 2.59
; Implement the union-set operation for the unordered-list representation of
; sets.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

; Exercise 2.60
; We specified that a set would be represented as a list with no duplicates.
; Now suppose we allow duplicates. For instance, the set '(1 2 3) could be
; represented as the list (2 3 2 1 3 2 2). 

; element-of-set? and intersection-set have the same implementation

(define (adjoin-set x set) (cons x set))
(define (union-set set1 set2) (append set1 set2))

; In this representation, element-of-set? and intersection-set have the same
; efficiency as they did in the no duplicate representation. adjoin-set and
; union-set are both more efficient in the steps they take, because they
; dont have to ensure a lack of duplicates. This representation might be useful
; in a situation where memory is abundant but speed is limited.

; Sets as ordered lists.
; 










