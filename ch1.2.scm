
; 1.2 - Procedures and the Processes They Generate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2.1 Linear Recursion and Iteration                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factorial-recursive n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-iterative n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))

; Exercise 1.9
; Are the following processes iterative or recursive?

; This process is recursive. Before the inc in the else clause can terminate,
; there is a recursive call, so a chain builds up. 
(define (ex1.9 a b)
  (if (= a 0)
      b
      (inc (ex1.9 (dec a) b))))

; This process is iterative. The else clause is a recursive call with each
; argument accounted for. 
(define (ex1.9 a b)
  (if (= a 0)
      b
      (ex1.9 (dec a) (dec b))))

; Exercise 1.10
; This is the acerkmann function

(define (Ack x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (Ack (- x 1)
                   (Ack x (- y 1))))))

; (Ack 1 10) -> 1024
; (Ack 2 4)  -> 65536
; (Ack 3 3)  -> 65536

; Give a concise mathematical definition for procedures computer by f g and h

(define (f n) (Ack 0 n)) ; 2*n

(define (g n) (Ack 1 n)) ; 2^n

(define (h n) (Ack 2 n)) ; 2^2^2^2^... n times


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2.2 Tree Recursion                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; While easier to understand and more natural to write, the tree recursive
; version of the fib sequence is grows in size exponentially. There are many
; redundant calculations
(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; The iterative version is linear in growth as well as steps, and is a much
; more efficient algorithm
(define (fib-itr n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))


; Counts how many ways change can be made out of a certain amount of coins.
; The algorithm can be easily understood as a tree recursive procedure, but
; converting it into an iterative procedure is much less intuitive
(define (count-change amount)
  
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denominations kinds-of-coins))
                       kinds-of-coins)))))
  
  (define (first-denominations kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  
  (cc amount 5))

; Exercise 1.11
; A function f is defined by the rule:
; f(n) = n if n < 3 and f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3
; Write two procedures, one that computes f recursively and one that computes
; f iteratively

(define (ex1.11 n)
  
  (define (f-rec n)
    (cond ((< n 3) n)
          (else (+ (f-rec (- n 1))
                   (* 2 (f-rec (- n 2)))
                   (* 3 (f-rec (- n 3)))))))
  
  (define (f-itr a b c count)
    (if (= count 0)
        c
        (f-itr (+ a (* 2 b) (* 3 c))
               a
               b
               (- count 1))))

  (display "f-rec: ") (display (f-rec n)) (newline)
  (display "f-itr: ") (display (f-itr 2 1 0 n)) (newline)
  (f-itr 2 1 0 n))

; Exercise 1.12
; Write a procedure that computes pascals triagle by means of a recursive
; process

(define (pascal n m)
  (if (or (<= m 1) (>= m n))
      1
      (+ (pascal (- n 1) m)
         (pascal (- n 1) (- m 1)))))

(define (pascals-triangle n)
  
  (define (iter n-count m-count)
    (display (pascal n-count m-count))
    (cond ((< m-count n-count)
            (display " ")
            (iter n-count (+ m-count 1)))
          ((< n-count n)
            (newline) (print-spaces (- n n-count))
            (iter (+ n-count 1) 1))))
  
  (define (print-spaces amount)
    (cond ((> amount 0)
            (display " ")
            (print-spaces (- amount 1)))))

  (iter 0 0))

; Exercise 1.13
; Prove Fib(n) is the closeset integer to a^n/sqrt(5) where a = (1 + sqrt(5))/2

; Too much math, no thanks

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2.3 Orders of Growth                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2.4 Exponentation                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To compute an exponential of a given number, we can use the recursive
; definition
; b^n = b * b^(n-1)
; b^0 = 1

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; We can easily convert this into a linear iteration

(define (expt b n)
  
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* b product))))
  
  (expt-iter n 1))

; This linear iteration is now constant in the space it uses, but it isn't
; as fast as it could be.

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

; fast-expt is now logarithmic in space and time, meaning that each time you
; double n, you linearly increase the space and time the algorithm takes

; Exercise 1.16
; Make an iterative version of fast-expt that is constant in the space it uses

(define (fast-expt b n)
  
  (define (fast-expt-iter a new-b new-n)
    (cond ((= new-n 0) a)
          ((even? new-n)
            (fast-expt-iter a
                            (square new-b)
                            (/ new-n 2)))
          (else
            (fast-expt-iter (* a new-b)
                            new-b
                            (- new-n 1)))))

  (fast-expt-iter 1 b n))

; Exercise 1.17
; The exponentation algorithms in this section are achieved by repeated
; multiplication. In a similar way, one can perform integer multiplication
; via repeated addition.

(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

; given the procedures double and half, create a fast version of mul
; that is analogous to the recursive version of fast-expt

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul a (half b))))
        (else (+ a (fast-mul a (- b 1))))))

(define (double n)
  (+ n n))

(define (half n)
  (/ n 2))

; Exercise 1.18
; Now create an iterative version of fast-mul analogous to fast-expt in
; exercise 1.16

(define (fast-mul a b)
  
  (define (iter x new-a new-b)
    (cond ((= new-b 0) x)
          ((even? new-b)
            (iter x (double new-a) (half new-b)))
          (else
            (iter (+ x new-a) new-a (- new-b 1)))))
  
  (iter 0 a b))

; Exercise 1.19
; Do math to figure out how to calculate fibonacci numbers in a logarithmic
; number of steps. Jk don't do that cuz fuck a math

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2.5 Greatest Common Divisors                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The GCD of two numbers, a and b, is the same as the GCD of b and the
; remainder of a/b. This can be applied to large number pairs to eventually
; find the gcd of any combo

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2.6 Example: Testing for Primality                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A number is prime if its smallest divisor is itself. 13 does not have any
; integers that divide into it without a remainder, except for 13. In order
; to check for this, we need a way to find a numbers smallest divisor.

(define (smallest-divisor n)
  
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (+ test-divisor 1)))))
  
  (find-divisor 2))

(define (divides? a b) (= (remainder b a) 0))

; Now that we can determine a numbers smallest divisor, we know the number
; is prime if its smallest divisor is itself.

(define (prime? n)
  (= (smallest-divisor n) n))

; The runtime of this is sqrt(n), but using something called Fermat's Little
; Theorem, we can achieve a runtime of log(n)

; Fermats Little Theorem:
; If n is prime and a is a positive integer less than n
; a^n is congruent to a%n

; If n is not prime, chances are that this test will not prove
; true. By running the test multiple times, you can assure yourself
; that a number is prime.

; This procedure computes the exponential of a number modulo another number

(define (expmod base exp m)
  (remainder (pow base exp) m))

; This one does it much faster

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                     m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Using our expmod procedure, we can now implement the fermat test, and by
; applying it a number of times we can be reasonably sure that a number is
; prime

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n)
  (define (fast-prime n times)
    (cond ((= times 0) true)
          ((= n 1) true)
          ((fermat-test n) (fast-prime n (- times 1)))
          (else false)))
  (fast-prime n 20))

; Exercise 1.21 
; Use smallest-divisor to find the divisors of 199, 1999, 19999.

; 199:   199
; 1999:  1999
; 19999: 7

; Exercise 1.22
; Test the amount of time it takes to run prime tests.
; Use this function to time the tests.

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (fast-prime? n)
      (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  (newline)
  (display n)
  (start-prime-test n (runtime)))

; Write a function, search-for-primes, that finds primes between
; a specified range

(define (search-for-primes from to)
  (cond ((even? from) (search-for-primes (+ 1 from) to))
        ((even? to) (search-for-primes from (- to 1)))
        ((<= from to) 
          (timed-prime-test from)
          (search-for-primes (+ from 2) to))))

; Being that the order of growth for (primes?) is O(sqrt n)
; the amount of time to calculate a prime for 100,000 and 10,000
; should be sqrt(10) times as long. Does this hold up?

; sqrt(10) = 3.16227766

; being that sicp was written in 1985, I had to use much
; larger numbers to notice a difference in speed.

; Primes directly after 1,000,000,000 took about .0400 seconds to calculate

; 1,000,000,007 *** .0400
; 1,000,000,021 *** .0399

; So primes directly after 10,000,000,000 should take
; .0400 * sqrt(10) to calulate, or .1265

; 10,000,000,019 *** .1099
; 10,000,000,033 *** .1200
; 10,000,000,061 *** .1299

; Further, primes directly after 100,000,000,000 should take
; .1265 * sqrt(10) to calculate, or .4000

; 100,000,000,003 *** .3200
; 100,000,000,019 *** .3900
; 100,000,000,057 *** .3700

; It absolutely does hold up. It's not perfect, but it is a
; fairly accurate estimation

; Exercise 1.23
; The current smallest-divisor function is ineffecient
; and testing more numbers than it needs to. After testing
; for 2, it doesn't need to test for any other even number.
; Implament a next function that returns 3 if 2 is entered,
; and its input plus 2 for everything else. Replace (+ test-devisor 1)
; with next

(define (next a)
  (if (= a 2) 3 (+ a 2)))

; Since we have effectively halved the work of the smallest-divisor
; algorithm, do the tests from the last exercise again. Do they take half
; the time? If not, how can this be explained?

; The new times are:
;                    new  / old
; 1,000,000,007 *** .0300 / .0400
; 1,000,000,021 *** .0300 / .0399

; 10,000,000,019 *** .0600 / .1099
; 10,000,000,033 *** .0900 / .1200
; 10,000,000,061 *** .1100 / .1299

; 100,000,000,003 *** .2200 / .3200
; 100,000,000,019 *** .2500 / .3900
; 100,000,000,057 *** .2400 / .3700

; It seems that instead of halving the speed, using next has
; cut it down to approximately 2/3 the orignal time.
; This can probably be explained by the extra if that
; next introduces.

; Exercise 1.24
; Modify timed-prime-test to use fast-prime? Due to it's O(log n) growth,
; how do you expect the time to test primes near 100,000,000,000 to compare
; to testing primes near 1,000,000,000?

; log n growth means that a number of twice as many digits will take 2 times
; as long to check for primality.

; 1000000000000000000000000000000061 *** .0300
; 1000000000000000000000000000000249 *** .0199
; 1000000000000000000000000000000283 *** .0200
; 1000000000000000000000000000000000000000000000000000000000000000049 *** .0500
; 1000000000000000000000000000000000000000000000000000000000000000187 *** .0400
; 1000000000000000000000000000000000000000000000000000000000000000513 *** .0600

; This roughly holds true, and therefor supports logarithmic growth

; Exercise 1.25
; Is it true that we could have saved ourselves a lot of work
; by defining expmod as

; (define (expmod base exp m)
;   (remainder (fast-expt base exp) m))

; Would this procedure work as well as the one we have in place?
; Why or why not?

; It would not, as it is a much slower procedure.
; This has to do with the fact that this version
; would be finding the remainder of an arbitrarily long number.
; This takes considerably longer than our version, which takes
; the remainder of a series of smaller numbers

; Exercise 1.26
; In expmod, using:
; (* (expmod base (/ exp 2) m)
;     expmod base (/ exp 2  m))

; Instead of:
; (square (expmod base (/ exp 2) m))

; results in a O(n) process instead of an O(log n) process
; Explain.

; Doing this results in a tree recursive procedure, which is O(c^n).
; Each tree itself has a growth rate of O(log n).
; This effectively results in a O(n) procedure.

; Exercise 1.27
; Demonstrate that the Carmichael numbers really do fool the fermat test.
; Write a procedure that takes an integer n and tests wether a^n is congruent
; to a modulo n for every a<n, and try your procedure on the given Carmichael
; numbers

(define (carmichael-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (iter it)
    (cond ((= it n) true)
          ((try-it it) (iter (+ it 1)))
          (else false)))
  (iter 1))

; This procedure does the fermat test for every number less than
; n, rather than some amount of random ones. It's slower, but more thorough.
; If a non prime passes this test, it has thoroughly tricked the fermat test.

; Some tests

(define (print-charmiael-test n)
  (display n) (display " is prime: ") (display (prime? n)) (newline)
  (display "carmichael-test: ") (display (carmichael-test n)) (newline) (newline))

(print-charmiael-test 561)
(print-charmiael-test 1105)
(print-charmiael-test 1729)
(print-charmiael-test 2465)
(print-charmiael-test 2821)
(print-charmiael-test 6601)

; 561 is prime: #f
; carmichael-test: #t

; 1105 is prime: #f
; carmichael-test: #t

; 1729 is prime: #f
; carmichael-test: #t

; 2465 is prime: #f
; carmichael-test: #t

; 2821 is prime: #f
; carmichael-test: #t

; 6601 is prime: #f
; carmichael-test: #t

; Exercise 1.28 ===============================================================

; This got copy pasted from the internet somewhere because at this point its
; just a math problem. I'm not here to learn math.

(define (miller-rabin-expmod base exp m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt x square)
      (if (and (= square 1)
               (not (= x 1))
               (not (= x (- m 1))))
          0
          square))
    (check-nontrivial-sqrt x (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp)
          (squaremod-with-check (miller-rabin-expmod base (/ exp 2) m)))
        (else
          (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                      m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0)) (= x 1)))
    (check-it (miller-rabin-expmod a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))















