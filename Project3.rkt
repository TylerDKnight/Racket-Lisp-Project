#lang racket

;;;;;;;;;;;;;;;;
;List Functions;
;;;;;;;;;;;;;;;;

;1. Append two lists
(define (append-me a b)
  (if (and (list? a) (list? b))
       (if (null? b)
           a
           (if (null? a)
               (cons (car b) (append-me '() (cdr b)))
               (cons (car a) (append-me (cdr a) b))))
       (print "Error: arguments not lists")))
(displayln (append '(1 3 x a) '(4 2 b)))

;2. Reverse a list
(define (reverse-me a)
  (if (list? a)
      (if (null? a)
          '()
          (append-me (reverse-me (cdr a)) (list (car a))))
      (print "Error: arguments not lists")))
(displayln (reverse '(a b c d)))

;3. Add to the end of list
(define (addtoend x lst)
  (if (list? lst)
      (reverse-me (append-me (list x) (reverse-me lst)))
      (print "Error: arguments are of the wrong type")))
(displayln (map add1 '(1 2 3 4)))

;====EC=====

;4. Index of
(define (indexof x list)
  (if (or (null? list) (eq? #f (member x list)))
      -1
      (if (equal? x (car list))
          0
          (+ 1 (indexof x (cdr list))))))

;5. Remove duplicates from a list
(define (nub a)
  (if (list? a)
    (if (null? a)
          '()
          (if (not (member (car a) (cdr a)))
              (append-me (list (car a)) (nub (cdr a)))
              (append-me '() (nub (cdr a)))))
    (print "Error: arguments not lists")))

;6. Remove all
(define (remove-all x alist)
  (if (list? alist)
    (if (null? alist)
          '()
          (if (not (eq? x (car alist)))
              (append-me (list (car alist)) (remove-all x (cdr alist)))
              (append-me '() (remove-all x (cdr alist)))))
    (print "Error: arguments not lists")))

;7. Merge two sorted lists
(define (merge a b)
  (if (or (null? a) (null? b))
      '()
      (append (list (car a)) (list (car b)) (merge (cdr a) (cdr b)))))

;8. Filter
(define (filter-me pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter-me pred (cdr lst)))
          (filter-me pred (cdr lst)))))


;;;;;;;;;;;;;;;
;Set Functions;
;;;;;;;;;;;;;;;

;1. Insert element into set
(define (insert x set)
  (nub (append-me set (list x))))
(displayln (insert 'a '(b c d)))

;2. Set Union
(define (union a b)
  (nub (append-me a b)))
(displayln (union '(a b c) '(a c d)))

;3. Cardinality
(define (cardinality a)
  (if (null? a)
      0
      (if (null? (cdr a))
          1
          (+ 1 (cardinality (cdr a))))))
(displayln (cardinality '(a b c)))

;====EC=====

;4. Member
(define (member-me x list)
  (if (null? list)
      #f
      (if (equal? x (car list))
               #t
               (member-me x (cdr list)))))

;5 Set intersection
(define (intersection a b)
  (if (null? a)
          '()
          (if (member (car a) b)
              (append-me (list (car a)) (cdr a))
              (append-me '() (cdr a)))))

;6. Set Difference
(define (difference a b)
  (if (null? a)
      '()
      (if (not (member (car a) b))
          (append-me (list (car a)) (difference (cdr a) b))
          (append-me '() (difference (cdr a) b)))))

;7. Check if subset or equal (⊆)
(define (subset? a b)
  (if (null? a)
      #t
      (if (not (member (car a) b))
          #f
          (subset? (cdr a) b))))

;8. Check if superset or equal (⊇)
(define (superset? a b)
  (subset? b a))


;;;;;;;;;;;;;;;;
;Math Functions;
;;;;;;;;;;;;;;;;

;1. Absolute value
(define (absolute-value n)
  (if (negative? n)
      (* -1 n)
      n))
(displayln (abs -7))

;2. Factorial
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))
(displayln (factorial 5))

;3. Check if 3 integers can be the lengths of the two sides and the hypoteneuse of a right triangle (in that order)
(define (right-tri x y z)
  (= (sqrt (+ (* x x) (* y y))) z))
(displayln (right-tri 3 4 5))

;====EC=====

;4. Test if a number is prime
(define (prime? n)
  (eq? 0 (cardinality (factors-of n))))

;5. Nth Fibonacci number
(define phi (/ 2 (- (sqrt 5) 1)))
(define (nth-fibo n) ;By Binet's formula
  (if (>= n 0)
      (inexact->exact (round (/ (expt phi n) (sqrt 5))))
      (if (even? n)
          (nth-fibo (abs n))
          (* -1 (nth-fibo (abs n))))))


;;;;;;;;;;;;;;;;;;;;
;Required Functions;
;;;;;;;;;;;;;;;;;;;;

;Auxillary functions
(define (divisible? n x) ;Simple divisibility test
  (integer? (/ n x)))

(define (factors-of-simple n) ;Trial division over range 2-sqrt(n)
  (let ([h (filter-me (lambda (x) (divisible? n x))
                      (range 2 (add1 (ceiling (sqrt n)))))])
    (nub (append-me h (reverse-me (map (lambda (y) (/ n y)) h)))))) ;Division to construct the other half

(define (factors-of n) ;Smart division over range 2-sqrt(n)
  (let ([half (vector->list (factors n
                                     (let ([s (list->vector (range 2 (add1 (ceiling (sqrt n)))))]) s)
                                     0))])
    (nub (append-me half (reverse-me (map (lambda (y) (/ n y)) half)))))) ;Division to construct the other half

(define (factors n s i) ;Helper function that takes in a sequential vector of potential factors and uses memoization to reduce repetitive divisibility checks
  (if (< i (vector-length s))
      (if (divisible? n (vector-ref s i))
          (factors n s (add1 i))
          (factors n (call-with-values (lambda () (values (set! s (vector-cull-step s i (vector-ref s i))))) (lambda (x) s)) i)) ;If n does not divide x, then all subsequent multiples of x do not need to be considered
      s))

(define (vector-cull-step s start step) ;Returns a vector with every nth element removed
    (let ([x
           (for ([i (range start (vector-length s) step)])
             (vector-set! s i '*))])
      (vector-filter number? s)))

(define (sum list) ;Sums a list for use in checking the sum of a number's factors
  (if (null? list)
      0
      (+ (car list) (sum (cdr list)))))

;1. Check if a number is perfect: a number is perfect if the sum of its factors other than itself is equal to itself.
(define (perfect? n)
  (= n (sum (append-me '(1) (factors-of n)))))
(displayln (perfect? 5))
(displayln (perfect? 6))

;2. Check if a number is abundant: an abundant number’s sum of factors other than itself is greater than the number.
(define (abundant? n)
  (> (sum (append-me '(1) (factors-of n))) n))
(displayln (abundant? 5))
(displayln (abundant? 12))

;3. Check if a number is deficient: a deficient number’s sum of factors is less than itself.
(define (deficient? n)
  (< (sum (append-me '(1) (factors-of n))) n))
(displayln (deficient? 5))
(displayln (deficient? 12))