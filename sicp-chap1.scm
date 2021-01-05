(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
1.17
(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))
(define (halve b)
  (/ b 2))
(define (double b)
  (* b 2))
1.18
(define (new-fast-* a b)
  (define (iter a b result)
    (cond ((= b 0) result)
          ((even? b) (iter (double a) (halve b) result))
          (else  (iter a (- b 1) (+ a result)))))
  (iter a b 0))


                                        ;1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   <??>;compute p'
                   <??>;compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

                                        ;1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)
(gcd 2 0)
(2)

(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40) (remainder 40 ((remainder 206 40))))
(gcd (remainder 40 ((remainder 206 40))) (remainder (remainder 40 ((remainder 206 40))) (remainder 40 ((remainder 206 40)))))


                                        ;1.21
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


                                        ;1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
                                        ;my solution
(define (search-for-primes n)
  (search-prime n 3))

(define (search-prime n total)
  (if (> total 0)
      (if  (prime? n)
           (begin (timed-prime-test n) (search-prime (+ n 2) (- total 1)))
           (search-prime (+ n 2) total))))
                                        ;1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

                                        ;1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
                                        ;1.27
(define (try-fermat a b)
  (= (expmod a b b) (remainder a b)))

(define (test-fermat m n)
  (cond ((= m 0) #t )
        ((not (try-fermat m n)) #f)
        (else (test-fermat (- m 1) n))
        ))

(define (test-the-fermat n)
  (test-fermat (- n 1) n))

                                        ;1.28
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (if (non-trivial? base m)
                      0
                      (expmod base (/ exp 2) m)))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (non-trivial? base m)
  (and (not  (= base 1))  (not (= base (- m 1))) (= (remainder (square base) m) 1)))

(define (miller-rabin n)
  (define (try-it a)
    (= (expmod a (- n 1) n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (try-fermat a b)
  (if (= (expmod a b b) 0)
      #f
      (= (expmod a b b) (remainder a b))))

(define (test-fermat m n)
  (cond ((= m 0) #t )
        ((not (try-fermat m n)) #f)
        (else (test-fermat (- m 1) n))
        ))

(define (test-the-fermat n)
  (test-fermat (- n 1) n))
                                        ;1.29
(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define (h a b n)
    (/ (- b a) n))
  (define (my-next x)
    (+ x (h a b n)))
  (define (my-sum term a next k)
    (if (< k 0)
        0
        (+ (if (or (= k 0) (= k n))
               (term a)
               (* (* 2 (+ (remainder k 2) 1)) (term a)))
           (my-sum term (next a) next (- k 1)))))
  (* (/ (h a b n) 3) (my-sum f a my-next n)))
                                        ;1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (inc n)
  (+ n 1))

(define (cube x)
  (* x x x))

                                        ;1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (self x)
  x)

(define (plus-2 x)
  (+ 2 x))

(define (add-1 x)
  (+ x 1))

(define (factorial a)
  (if (< x 1)
      1
      (product self 1 add-1 a)))

(define (approxi-pi x)
  (/ (* (product-iter self 2 plus-2 (- x 1) ) (product-iter self 4 plus-2 (+ x 1) ))
     (* (product-iter self 3 plus-2 x) (product-iter self 3 plus-2 x))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
                                        ;1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (acc-product term a next b)
  (accumulate * 1 term a next b))

(define (acc-sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
                                        ;1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (combiner null-value (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (any-one x)
  x)

(define (filtered-acc-sum term a next b)
  (filtered-accumulate any-one + 0 term a next b))

(define (add-prime a b)
  (filtered-accumulate prime? + 0 self a add-1 b))

(define (product-prime n)
  (define (relative-prime? x)
    (if (> (remainder n x) 0)
        x
        #f))
  (filtered-accumulate relative-prime? * 1 self 1 add-1 n))
                                        ;1.34
(define (f g) (g 2))


;; 1.3.3
(define (search f neg-point post-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative b-value) (positive? a-value))
           (search f b a))
          (else
           (error "values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

                                        ;1.35
(custom-fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)

                                        ;1.36
(define (average x1 x2)
  (/ (+ x1 x2) 2))

(define (custom-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (display v1)
    (newline)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
                                        ;1.37
(define (cont-frac n d k)
  (define (frac up down count)
    (if (< count k)
        (/ (up count) (+ (down count) (frac up down (+ count 1))))
        (/ (up count) (down count))))
  (frac n d 1))

(define (my-frac k)
  (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k ))

(custom-fixed-point (lambda (x) (- (/ 1 x) 1)) 1)

(define (get-k)
  (define (close-enough? v1 v2)
    (display v1)
    (newline)
    (display v2)
    (newline)
    (let ((tolerance 0.0001))
      (< (abs (- v1 v2))
         tolerance)))
  (define (try k)
    (let ((test (my-frac (+ k 1)))
          (golden (my-frac k)))
      (if (close-enough? golden test)   ;golden做成使用k计算的临时变量
          k
          (try (+ k 1)))))
  (try 1))

(define (try )
  (let ((golden (custom-fixed-point (lambda (x) (+ 1 (/ 1 x))) 1) ) )
    (if (close-enough? (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)
                       golden)
        (display k)
        ())))

(define (cont-frac2 n d k)
  (define (frac n up d down count)
    (if (> count 1)
        (frac n
              (n (- count 1))
              d
              (+ (d (- count 1))
                 (/ up down))
              (- count 1))
        (/ up down)))
  (frac n 1 d 1 k))

(define (my-frac2 k)
  (cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) k ))

                                        ;1.38
(define (dfc k)
  (define (d count)
    (let ((rem (remainder count 3)))
      (cond ((or (= rem 1) (= rem 0)) 1)
            (else  (* 2 (+ (quotient count 3) 1))))))
  (+ (cont-frac (lambda (i) 1) d k) 2.0))

(define (dfc1 k)
  (define (d count)
    (if (= (remainder count 3) 2)
        (/ (+ count 1) 1.5)
        1))
  (+ (cont-frac (lambda (i) 1) d k) 2.0))
                                        ;1.39
(define (tan-cf x k)
  (define (n k)
    (cond ((= k 1.0) x)
          (else (- (* x x)))))
  (define (d k)
    (- (* 2.0 k) 1))
  (cont-frac n d k))

                                        ;1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))


                                        ;1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
                                        ;1.41

(define (inc x)
  (+ x 1))

(define (double proc)
  (lambda (x)
    (proc (proc x))))
                                        ;1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

                                        ;1.43
(define (repeated f n)
  (if (> n 1)
      (repeated (lambda (x) (f (f x)))
                (- n 1))
      f))

(define (repeated f n)
  (if (> n 1)
      (repeated (compose f f)
                (- n 1))
      f))
                                        ;1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
       (f x)
       (f (+ x dx)))
       3)))

(define (fold-smooth f n)
  ((repeated smooth n) f))

                                        ;1.45
(define (log2 x)
  (/ (log x) (log 2)))

(define (pow b p)
   (define (even? x)
     (= (remainder x 2) 0))

(define (fixed-point-n n x)
  (fixed-point ((repeated average-damp (floor (log2 n)))
                 (lambda (y) (/ x (pow y (- n 1)))))
                1.0))
                                        ;1.46
(define (iterative-improve f g)
  (define (iter guess)
    (if (f guess)
        guess
        (iter (g guess))))
   iter)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess)))
       tolerance))
  ((iterative-improve good-enough? f) first-guess))
