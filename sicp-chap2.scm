                                        ;2.1
(define (number x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((numer (/ n g))
          (denom (/ d g)))
      (if  (< (* numer denom) 0)
           (cons (- (abs numer)) (abs denom))
           (cons (abs numer) (abs denom))))))
                                        ;2.2
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (define (middle x y)
    (/ (+ x y) 2))
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (middle (x-point start) (x-point end))
                (middle (y-point start) (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
                                        ;2.3
(define (make-rect first-segment second-segment)
  (cons first-segment second-segment))

(define (first-segment rect)
  (car rect))

(define (second-segment rect)
  (cdr rect))

(define (perimeter rect)
  (* 2 (+ (first-segment rect) (second-segment rect))))

(define (area rect)
  (* (first-segment rect) (second-segment rect)))

(define (make-rect2 left-top right-top left-bottom right-bottom)
  (cons (cons left-top right-top) (left-bottom right-bottom)))

(define (first-segment2 rect)
  (make-segment (car (car rect)) (cdr (car rect))))

(define (second-segment2 rect)
  (make-segment (car (cdr rect)) (cdr (cdr rect))))
                                        ;2.4
(define (cdr z)
  (z (lambda (p q) q)))
                                        ;2.5
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "arg" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))

(define (cons a b)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (count-d z d r)
  (if (= (remainder z d) 0)
      (count-d (/ z d) d (+ r 1))
      r))

(define (car z)
  (count-d z 2 0));一直除以2直到不能整除，同时计数，返回的是a

(define (cdr z)
  (count-d z 3 0))
                                        ;2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add m n)
  (lambda (f) ((lambda (x) ((m f) ((n f) x))))))
                                        ;2.7
(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min  (car interval) (cdr interval)))
                                        ;2.8
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (let ((add-interval x y) sum)
    (make-interval 0 ((upper-bound sum) (lower-bound sum)))))

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (upper-bound y))
                 (- (lower-bound x) (lower-bound y))))

(define (display-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

                                        ;2.9
(define (width x)
  (- (upper-bound x) (lower-bound x)))

(width   (make-interval (+ (lower-bound x) (lower-bound y))
                        (+ (upper-bound x) (upper-bound y))))
(- (upper-bound (make-interval (+ (lower-bound x) (lower-bound y))
                               (+ (upper-bound x) (upper-bound y))))
   (lower-bound (make-interval (+ (lower-bound x) (lower-bound y))
                               (+ (upper-bound x) (upper-bound y)))))

(- (- (upper-bound x) (lower-bound x))
   (- (upper-bound y) (lower-bound y)))
                                        ;2.17
(define (last-pair a-list)
  (if (> (length a-list) 1)
      (last-pair (cdr a-list))
      a-list))
                                        ;2.18
(define (reverse a-list)
  (if (<= (length a-list) 1)
      a-list
      (append (reverse (cdr a-list)) (list (car a-list)))))
                                        ;2.19

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

                                        ;2.20
(define (same-parity x . y)
  (define (same-parity-recursive x y res)
    (let ((parity (remainder x 2)))
      (if (null? y)
          res
          (if (= parity (remainder (car y) 2))
              (same-parity-recursive x (cdr y) (append res (list (car y)))) ;需要使用遍历或递归的手段，把后续满足条件的元素放入res，返回出来
              (same-parity-recursive x (cdr y) res)))))
  (same-parity-recursive x y (list x)))
                                        ;2.21
(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))
                                        ;2.22
(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list-4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

(define (square-list-5 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items '()))
                                        ;2.23
(define (for-each f list)
  (cond ((null? list) '())
        (else
         (f (car list))
         (for-each f (cdr list)))))
                                        ;2.27
(define (count-leaves x)
  (cond ((null? x) x)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (deep-reverse a-list)
  (cond ((null? a-list) '())
        ((not (pair? a-list)) a-list)
        (else (append (deep-reverse (cdr a-list)) (list (deep-reverse (car a-list)))))))
                                        ;2.28
(define (fringe tree)
  (cond ((number? tree) (list tree))
        ((= (length tree) 1)
         (fringe (car tree)))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))
                                        ;2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (make-mobile left right) (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  (let (((branch-structure (left-branch mobile)) left-weight)
        ((branch-structure (right-branch mobile)) right-weight))
    (+ (if (number? left-weight)
           left-weight
           (total-weight left-weight))
       (if (number? left-weight)
           right-weight
           (right-branch mobile)))))

(define (total-weight mobile)
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))

(define (weight structure)
  (if (number? structure)
      structure
      (+ (weight (branch-structure (left-branch structure)))
         (weight (branch-structure (right-branch structure))))))

(define (balanced? mobile)
  (= (* (branch-length (left-branch mobile)) (weight (branch-structure (left-branch mobile))))
     (* (branch-length (right-branch mobile)) (weight (branch-structure (right-branch mobile))))))
                                        ;2.30
(define (square-tree-1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else
         (cons (square-tree-1 (car tree)) (square-tree-1 (cdr tree))))))

(define (square-tree-2 items)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (square sub-tree)))
       items))
                                        ;2.31
(define (tree-map proc tree)
  (define (inner tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (inner sub-tree)
               (proc sub-tree)))
         tree))
  (inner tree))

(define (tree-map-2 proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
                                        ;2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (subset)
                        (cons (car s) subset))
                      rest)))))
                                        ;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (my-append seq1 seq2)
  (accumulate cons '() (enumerate-tree (list seq1 seq2))))

(define (my-length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0 sequence))
                                        ;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higgher-terms)
                (+ (* higgher-terms x) this-coeff))
              0
              coefficient-sequence))
                                        ;2.35
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves-2 t)
  (accumulate (lambda (x y)
                (+ (length x) y))
              0
              (map enumerate-tree
                   t)))
                                        ;2.36
(define (accumulate-n-2 op init seqs)
  (cond ((null? seqs) '())
        ((null? (car seqs)) '())
        (else (cons (accumulate op init (car seqs))
              (accumulate-n-2 op init (cdr seqs))))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
                                        ;2.37
(define test-mat (list (list 1 2 3) (list  2  3 4)))

(define test-v (list 2 3 4))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (accumulate + 0 (map * row v))) m))

(define (transpose mat)
  (accumulate-n (lambda (init col)
                  (cons init col)) '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row)) m)))
                                        ;2.38
3/2
1/6
(list 1 2 3)
                                        ;2.39
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define test-sequence (list 2 3 4 5))

(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
                                        ;2.40
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
        (map make-pair-sum
             (filter prime-sum? (flatmap
                                 (lambda (i)
                                   (map (lambda (j) (list i j))
                                        (enumerate-interval i (- i 1))))
                                 (enumerate-interval 1 n)))))

(define (permutations s)
  (if (nulls s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
                                        ; answer
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs-v2 n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
                                        ;2.41
(define (all-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
            (map (lambda (k)
                   (list i j k))
                 (enumerate-interval 1 n)))
          (enumerate-interval 1 n)))
   (enumerate-interval 1 n)))

(define (find-the-s n s)
  (define (equals-s? triple)
    (= (accumulate + 0 triple) s))
  (filter equals-s? (all-triples n)))
                                        ;2.42
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (adjoin-position
         new-row k rest-of-queens)
  (append rest-of-queens (list (cons new-row k))))

(define empty-board '())

(define (safe? k positions)
  (define (last_element l)
    (cond ((null? (cdr l)) (car l))
          (else (last_element (cdr l)))))
  (accumulate (lambda (x y)
                (and x y))
              #t
              (let ((last (last_element positions)))
                (map ;把每个position映射成#t或#f，#t表示这个位置与k列的position不冲突。注意每一列只会有一个position，最后一个position的列是k
                 (lambda (position)
                   (if (= (cdr position) k)
                       #t
                       (not
                        (or (= (car position)
                              (car last))
                           (= (abs (- (car position)
                                      (car last)))
                              (abs (- (cdr position)
                                      (cdr last))))))))
                 positions))))

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
                                        ;2.43
(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
          (adjoin-position
           new-row k rest-of-queens))
        (enumerate-interval 1 board-size)))
 (queen-cols (- k 1)))

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens)
          )
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))
                                        ;2.44
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1)))))
      (beside painter (below smaller smaller))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1)))))
      (below painter (beside smaller smaller))))
                                        ;2.45
(define (split first-proc second-proc)
  (define (inner painter n)
    (if (= n 0)
        painter
        (let ((smaller (inner painter (- n 1)))))
        (first-proc painter (second-proc smaller smaller))))
  inner)
                                        ;2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1)
                (xcor-vect vect2))
             (+ (ycor-vect vect1)
                (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1)
                (xcor-vect vect2))
             (-  (ycor-vect vect1)
                 (ycor-vect vect2))))

(define (scale-vect scale vect)
  (make-vect (* scale (xcor-vect vect))
             (* scale (ycor-vect vect))))
                                        ;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))
                                        ;2.48
(define (make-segment vec1 vec2)
  (cons vec1 vec2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
                                        ;2.49
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define (outline-painter frame)
  (segments->painter (let ((origin (origin-frame frame))
                           (edge1 (edge1-frame frame))
                           (edge2 (edge2-frame frame)))
                       (let ((lt (add-vect origin edge1))
                             (rb (add-vect origin edge2))
                             (lb origin)
                             (rt (add-vect (add-vect origin edge1) edge2)))
                         (list
                          (make-segment (make-vect lt) (make-vect rt))
                          (make-segment (make-vect rt) (make-vect rb))
                          (make-segment (make-vect lb) (make-vect lt))
                          (make-segment (make-vect lb) (make-vect rb)))))))


(define (x-painter frame)
  (segments->painter (let ((origin (origin-frame frame))
                           (edge1 (edge1-frame frame))
                           (edge2 (edge2-frame frame)))
                       (let ((lt (add-vect origin edge1))
                             (rb (add-vect origin edge2))
                             (lb origin)
                             (rt (add-vect (add-vect origin edge1) edge2)))
                         (list
                          (make-segment (make-vect lt) (make-vect rb))
                          (make-segment (make-vect lb) (make-vect rt)))))))

(define (diamond-painter frame)
  (segments->painter (let ((origin (origin-frame frame))
                           (edge1 (edge1-frame frame))
                           (edge2 (edge2-frame frame)))
                       (let ((lm (add-vect origin (scale-vect 0.5 edge1)))
                             (bm (add-vect origin (scale-vect 0.5 edge2)))
                             (tm (add-vect (add-vect origin edge1) (scale-vect 0.5 edge2)))
                             (rm (add-vect (add-vect origin edge2) (scale-vect 0.5 edge1))))
                         (list
                          (make-segment (make-vect lm) (make-vect bm))
                          (make-segment (make-vect bm) (make-vect rm))
                          (make-segment (make-vect rm) (make-vect tm))
                          (make-segment (make-vect tm) (make-vect lm)))))))

(define (wave-painter frame)
  (segments->painter ((let ((origin (origin-frame frame))
                            (edge1 (edge1-frame frame))
                            (edge2 (edge2-frame frame)))
                        (let ((lm (add-vect origin (scale-vect 0.5 edge1)))
                              (bm (add-vect origin (scale-vect 0.5 edge2)))
                              (tm (add-vect (add-vect origin edge1) (scale-vect 0.5 edge2)))
                              (rm (add-vect (add-vect origin edge2) (scale-vect 0.5 edge1))))
                          (list
                           (make-segment (make-vect lm) (make-vect bm))
                           (make-segment (make-vect bm) (make-vect rm))
                           (make-segment (make-vect rm) (make-vect tm))
                           (make-segment (make-vect tm) (make-vect lm))))))))
                                        ;2.50
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (tranform-painter painter
                    (make-vect 1.0 0.0)
                    (make-vect 0.0 0.0)
                    (make-vect 1.0 1.0)))

(define (flip-180 painter)
  (tranform-painter painter
                    (make-vect 1.0 1.0)
                    (make-vect 1.0 1.0)
                    (make-vect 0.0 1.0)))

(define (flip-270 painter)
  (tranform-painter painter
                    (make-vect 0.0 1.0)
                    (make-vect 0.0 0.0)
                    (make-vect 1.0 1.0)))

                                        ;2.51
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0))
        (let ((paint-left
               (transform-painter
                painter1
                (make-vect 0.0 0.0)
                split-point
                (make-vect 0.0 1.0)))
              (paint-right
               (transform-painter
                painter2
                split-point
                (make-vect 1.0 0.0)
                (make-vect 0.5 1.0)))))
        (lambda (frame)
          (paint-left frame)
          (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5))
        (let ((paint-bottom
               (transform-painter
                painter1
                (make-vect 0.0 0.0)
                (make-vect 1.0 0.0)
                split-point))
              (paint-up
               (transform-painter
                painter2
                split-point
                (make-vect 1.0 0.5)
                (make-vect 0.0 1.0)))))
        (lambda (frame)
          (paint-bottom frame)
          (paint-up frame)))))

(define (below painter1 painter2)
  (flip-vert
   (beside (flip-270 painter1)
           (flip-270 painter2))))
                                        ;2.52
(define wave
  (segments->painter (list
                      (make-segment
                       (make-vect 0.44 0.7)
                       (make-vect 0.51 0.7)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (beside
       (below painter (up-split painter (- n 1)))
       (below (right-split painter (- n 1))
              (corner-split painter (- n 1))))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))
                                        ;2.53
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
                                        ;2.54
(define (my-equal? a b)
  (cond
   ((and (not (pair? a)) (not (pair? b))) (eq? a b))
   ((and (pair? a) (pair? b)) (and (my-equal? (car a)
                                              (car b))
                                   (my-equal? (cdr a)
                                              (cdr b))))
   (else #f)))
                                        ;2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp)
                                (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression" exp))))


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))
                                        ;2.57
                                        ;old
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

                                        ;new
(define (make-sum a1 . a2)
  (cond ((null? a2) a1)
        ((=number? a1 0) (apply make-sum a2))
        ((=number? (apply make-sum a2) 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 (apply make-sum a2)))))

(define (make-product m1 . m2)
;  (display m1)
  (cond ((null? m2) m1)
        ((or (=number? m1 0) (=number? (apply make-product m2) 0)) 0)
        ((=number? m1 1) (apply make-product m2))
        ((=number? (apply make-product m2) 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 (apply make-product m2)))))

                                        ;old

(define (augend s) (caddr s))

(define (multiplicand p) (caddr p))

(define (augend s) (apply  make-sum (cdr (cdr s))))

(define (multiplicand p)  (apply make-product (cdr  (cdr p)))))

(define (augend s) 
(let ((other (cddr s))) 
  (if (= (length other) 1) 
      (car other) 
      (cons '+ other)))) 

(define (multiplicand p) 
(let ((other (cddr p))) 
  (if (= (length other) 1) 
      (car other) 
      (cons '* other)))) 

(define (augend s) 
(if (null? (cdddr s)) 
    (caddr s) 
    (cons '+ (cddr s)))) 

(define (multiplicand p) 
(if (null? (cdddr p)) 
    (caddr p) 
    (cons '* (cddr p)))) 
                                        ;2.58
                                        ;a
(define (make-sum a1 a2)
(cond ((=number? a1 0) a2)
      ((=number? a2 0) a1)
      ((and (number? a1) (number? a2))
       (+ a1 a2))
      (else (list a1 '+  a2))))

(define (=number? exp num)
(and (number? exp)
     (= exp num)))

(define (make-product m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0)) 0)
      ((=number? m1 1) m2)
      ((=number? m2 1) m1)
      ((and (number? m1) (number? m2)) (* m1 m2))
      (else (list m1 '* m2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))
                                        ;b
                                        ;done
(define (product? x) (and (pair? x) (accumulate (lambda (a b) (or a b))
                                                #f
                                                (map (lambda (e)
                                                       (eq? e '*))
                                                     x))))
(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (contains list x)
(cond
 ((null? list) #f)
 ((eq? (car list) x) #t)
 (else (contains (cdr list) x))))

(define (sum? x) (and (pair? x) (contains x '+)))

(define (addend s) (car s))

                                        ;简单测试通过了

(define (augend s) (let ((second (cddr s)))
                     (cond
                      ((product? second) (make-product second))
                      ((sum? second) (make-sum second))
                      (else (car second)))))
(define (make-sum a1 . a2)
  (cond ((null? a2) a1)
        ((=number? a1 0) (apply make-sum a2))
        ((=number? (apply make-sum a2) 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ (apply make-sum a2)))))

(define (multiplicand p) (caddr p))

(define (make-product m1 . m2)
  (define (multiplier p) (car p))
                                        ;  (display m1)
  (cond ((null? m2) m1)
        ((or (=number? m1 0) (=number? (apply make-product m2) 0)) 0)
        ((=number? m1 1) (apply make-product m2))
        ((=number? (apply make-product m2) 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 (apply make-product m2)))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression" exp))))
                                        ;undone
;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;;         ((=number? a2 0) a1)
;;         ((and (number? a1) (number? a2))
;;          (+ a1 a2))
;;         (else (list a1 '+  a2))))

;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;;         ((=number? m1 1) m2)
;;         ((=number? m2 1) m1)
;;         ((and (number? m1) (number? m2)) (* m1 m2))
;;         (else (list m1 '* m2))))

(define (accumulate op initial sequence)
(if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
()

                                        ;2.59
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))
                                        ;2.60
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (intersection-set-strict set1 set2) 
  (cond ((or (null? set1) (null? set2)) '()) 
        ((element-of-set? (car set1) set2)         
         (cons (car set1) 
               (intersection-set-strict (cdr set1) set2))) 
        (else (intersection-set-strict (cdr set1) set2))))

(define (intersection-set set1 set2) 
  (let ((inter (intersection-set-strict set1 set2)) 
        (union (union-set set1 set2))) 
    (filter (lambda (x) (element-of-set? x inter)) 
            union)))
()

                                        ;2.61
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (define (adjoin-former-x-latter former x latter)
    (cond
     ((null? latter) (append former (list x)))
     ((< x (car latter))  (append (append former (list x)) latter))
     ((= x (car latter)) (append former latter))
     (else (adjoin-former-x-latter (append former (list (car latter))) x (cdr latter)))))
  (adjoin-former-x-latter '() x set))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))
                                        ;2.62
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2 )
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2 (union-set (set1 (cdr set2))))))))))

                                        ;2.63
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))
                                        ;2.63
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))
                                        ;2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
                    (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))


                                        ;2.65
(define (merge-list list1 list2)
  (define (iter list1 list2 result)
    (cond ((null? list1) result)
          ((null? list2) result)
          ((= (car list1)  (car list2))
           (iter (cdr list1) (cdr list2) (append result  (list (car list2)))))
          ((< (car list1) (car list2)) (iter (cdr list1) list2 result))
          ((< (car list1) (car list2)) (iter list2 (cdr list2) result))))
  (iter list1 list2 '()))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (merge-list list1 list2))))

(define (merge2-list list1 list2)
  (define (iter list1 list2 result)
    (cond ((null? list1) (append result list2))
          ((null? list2) (append result list1))
          ((= (car list1)  (car list2))
           (iter (cdr list1) (cdr list2) (append result  (list (car list2)))))
          ((< (car list1) (car list2)) (iter (cdr list1) list2 (append result (list (car list1)))))
          ((< (car list1) (car list2)) (iter list2 (cdr list2) (append result (list (car list2)))))))
  (iter list1 list2 '()))

(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (merge2-list list1 list2))))

(define (intersection-set set1 set2)
   (if (or (null? set1) (null? set2))
       '()
       (define (find-equal set1 set2)
         (cond ((= (entry set1) (entry set2))
             (entry set1))
             ((> (entry set1) (entry set2)) (find-equal (left-branch set1)
                                                        set2))
             ((< (entry set1) (entry set2)) (find-equal set1 (left-branch set2)))))
       (let ((x1 (entry set1))
             (x2 (entry set2))
             (set1-left-branch (left-branch set1))
             (set2-left-branch (left-branch set2))
             (set1-right-branch (right-branch set1))
             (set2-right-branch (right-branch set2)))
         (cond ((= x1 x2)
                (make-tree x1
                           (intersection-set set1-left-branch set2-left-branch)
                           (intersection-set set1-right-branch set2-right-branch)))
               ((< x1 x2)
                (intersection-set set1-right-branch set2))
               ((< x2 x1)
                (intersection-set set1 set2-right-branch))))))

 (define (union-set set1 set2)
   (cond ((null? set1) set2)
         ((null? set2) set1)
         (else
          (let ((x1 (car set1))
                (x2 (car set2))
                (set1-left-branch (left-branch set1))
                (set2-left-branch (left-branch set2))
                (set1-right-branch (right-branch set1))
                (set2-right-branch (right-branch set2)))
            (cond ((= x1 x2 )
p                   (make-tree x1
                              (union-set set1-left-branch set2-left-branch)
                              (union-set set2-right-branch set2-right-branch)))
                  ((< x1 x2)
                   (make-tree x1 (union-set set1-left-branch (union-set set1-right-branch set2))))
                  ((< x2 x1)
                   (make-tree x2 (union-set set1 set2-right-branch))))))))

                                        ;2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         ((lookup given-key (left-branch set-of-records)))))
        (else (lookup given-key (right-branch set-of-records))))

                                        ;2.67
(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit"))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

                                        ;2.68
(define (encode-symbol symbol tree)
  (define (iter symbol tree result)
    (cond
     ((leaf? tree)
      (if (eq? (symbol-leaf tree) symbol)
          result)
      result)
     ((contains (symbols (left-branch tree)) symbol)
      (iter symbol (left-branch tree) (append result (list '0))))
     ((contains (symbols (right-branch tree)) symbol)
      (iter symbol (right-branch tree) (append result (list '1))))
     (else           (error "error symbol"))))
  (iter symbol tree '()))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

                                        ;2.69
(define (adjoin-set x set)
(cond ((null? set) (list x))
      ((< (weight x) (weight (car set))) (cons x set))
      (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cdr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
(successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (= (length set) 1)
      (car set)
      (let ((first (car set))
            (second (cadr set)))
        (let ((new-node (if (>= (weight first) (weight second))
                            (make-code-tree second first)
                            (make-code-tree first second ))))
          (successive-merge (cons new-node (cddr set)))))))
                                        ;2.70
(define lyrics-pairs (list (cons 'a 2)
                      (cons 'get 2)
                      (cons 'sha 3)
                      (cons 'wah 1)
                      (cons 'boom 1)
                      (cons 'job 2)
                      (cons 'na 16)
                      (cons 'yip 9)))

(define lyrics-tree (generate-huffman-tree lyrics-pairs))

(define lyrics-message '(get a job
                             sha na na na na na na na na
                             get a job
                             sha na na na na na na na na
                             wah yip yip yip yip yip yip yip yip yip
                             sha boom))
                                        ;2.71
                                        ;2.72
                                        ;2.73
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
                                        ;interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (deriv exp var)
(cond ((number? exp) 0)
      ((variable? exp)
       (if (same-variable? exp var) 1 0))
      ((sum? exp)
       (make-sum (deriv (addend exp) var)
                 (deriv (augend exp) var)))
      ((product? exp)
       (make-sum (make-product
                  (multiplier exp)
                  (deriv (multiplicand exp) var))
                 (make-product
                  (deriv (multiplier exp) var)
                  (multiplicand exp))))
      (else (error "unknown expression type: DERIV" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (opeartor exp) (car exp))
(define (operands exp) (cdr exp))
                                        ;
                                        ;table
(define table ())

                                        ;put into tablen, done
(define (put op type item)
(append table (list op type item)))

                                        ;get from table, not done
(define (get op type)
(let (first (car table))
  (if (and (eq (car first))
           (eq (cadr first)))
      (caddr first)
                                        ;递归查找接下来的元素，可能要建个iter过程
      )))


                                        ;基本完成
(define *the-table* (make-hash))
(define (put key1 key2 value) (hash-set! *the-table* (list key1 kye2) value))
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f))

(define (install-sum-deriv-pkg)
  (define (addend s) (cadr s))
  (define (augend s)
    (let ((cs (cdr s)))
      (if (null? (cdr cs))
          (car cs)
          (cons '+ cs))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (deriv-inner operands var) (make-sum (deriv (addend operands) var)
                                               (deriv (augend operands) var)))
  (put 'deriv '+ deriv-inner)
  'done)

(define (install-product-deriv-pkg)
  (define (multiplier p) (car p))
  (define (multiplicand p)
    (let ((cs (cdr p)))
      (if (null? (cdr cs))
          (car cs)
          (cons '* cs))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0)
               (=number? m2 0))
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else (list '* m1 m2))))
  (define (deriv-inner operands var)
    (make-sum (make-product
               (multiplier operands)
               (deriv (multiplicand operands) var))
              (make-product
               (deriv (multiplier operands) var)
               (multiplicand operands))))
                                        ;interface to the rest of the system
  (put 'deriv '* deriv-inner)
  'done)

(define (install-exponent-deriv-pkg)
  (define base car p)
  (define exponent cadr)
  (define (make-exponentiation base exponent)
    (cond ((=number? base 0) 0)
          ((=number? exponent 1) base)
          ((=number? exponent 0) 1)
          ((and (number? base) (number? exponent))
           (expt base exponent))
          (else (list '** base exponent))))
  (define (deriv-inner operands var)
    (make-product (exponent operands)
              (make-product
               (make-exponentiation (base operands) (- (exponent operands) 1))
               (deriv (base operands) var))))
                                        ;interface to the rest of the system
  (put 'deriv '** deriv-inner)
  'done)
                                        ;2.74
(define (get-type file))

(define (get-record name file)
  ((get-record-proc (get-type file)) name file))

(define (get-salary name file)
  ((get-salary-proc (get-type file)) name file))

(define (find-employee-record name div-file-list)
  (define (exists? name div-file)
    (if (get-record name div-file)
        #t
        #f))
  (if (not (null? div-file-list))
      (let (div-file (car div-file-list))
        (if (exists? name div-file)
            #t
            (find-employee-record name (cdr div-file-list))))
      #f))
                                        ;2.75
(define (make-from-real-imag x y)
(define (dispatch op)
  (cond ((eq? op 'real-part) x)
        ((eq? op 'imag-part) y)
        ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
        ((eq? op 'angle) (atan y x))
        (else (error ""))))
dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error ""))))
  dispatch)
                                        ;2.76
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number x)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (x)
         (= x 0)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (define (number x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (number x) (denom y))
              (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (= (* (numer x) (denom y)) (* (numer y) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x)
         (and
          (= (nomer x) 0))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
         (and (= (real-part z1) (real-part z1))
              (= (imag-part z1) (imag-part z1)))))
  (put '=zero? '(complex)
       (lambda (z1)
         (= (real-part z1) (imag-part z1) 0)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-complex-from-mag-ang 'complex) r a))

                                        ;2.78
(define (install-scheme-number-package)
                                        ;type-tag contents attach-tag
  (define (attach-tag type-tag contents)
    (if (eq? type-tag 'scheme-number)
        contents
        (cons type-tag contents)))
  (define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (if (number? datum)
            'scheme-number
            (error))))
  (define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (if (number? datum)
            datum
            (error))))

  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number x)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)
                                        ;2.81
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (= type1 type2)
                    (error "")
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else (error "no mothod for these types"
                                         (list op type-tags)))))))
              (error "no method for these types"
                     (list op type-tags)))))))
                                        ;2.81
(define (scheme-number->scheme-number n) n)

(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

(put-coercion 'complex 'complex complex->complex)
                                        ;2.82
(define (apply-generic op . args)
  (define (trans-type a . args)
    (let (a-type (type-tag a))
      (if (null? args)
          '()
          (cons ((get->coercion a-type (car args)) (car args)) (trans-type a (cdr args))))))
  (define (try-proc args)
   (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          #t
          #f))))
  (define (apply-args args)
    (let ((type-tags (map type-tag args)))
     (let ((proc (get op type-tags)))
      (apply proc (map contents args)))))
  (define (run tmp-args args)
    (if (try-proc (trans-type (car tmp-args)) args)
        (apply-args args)
        (run  (cdr tmp-args) args)))
  (run (args) args))

(define (apply-generic op . args)
(define (trans-type a args)
  "把args中的所有参数的类型转换到a的类型"
  (let (a-type (type-tag a))
    (if (null? args)
        '()
        (cons (let (coercor (get-coercion a-type (type-tag (car args))))
                (if coercor
                    (coercor (car args))
                    (car args)))
              (trans-type a (cdr args))))))
(define (try-proc args)
  "测试有无适用于args中的所有参数的类型的操作"
  (if (get op (map type-tag args))
      #t
      #f))
(define (apply-args args)
  "将适用的操作应用于args"
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (apply proc (map contents args)))))
(define (run tmp-args args)
  "主方法"
  (if (null? tmp-args)
      (error "no applicable type")
      (if (try-proc (trans-type (car tmp-args)) args)
          (apply-args args)
          (run  (cdr tmp-args) args))))
(run args args))

(define (apply-generic op . args)
  (define (type-tags args)
    (map type-tag args))
  (define (try-coerce-to target)
    (map (lambda (x)
           (let ((coercor (get-coercion (type-tag x) (type-tag target))))
             (if coercor
                 (coercor x)
                 x)))
         args))
  (define (iterate next)
    (if (null? next)
        (error)
        (let ((coerced (try-coerce-to (car next))))
          (let ((proc (get op (type-tags coerced))))
            (if proc
                (apply proc (map contents coerced))
                (iterate (cdr next)))))))
  (let ((proc (get op (type-tags args))))
    (if proc
        (apply proc (map contents args))
        (iterate args))))
                                        ;2.83
(define (raise x) (apply-generic 'raise x))
(put 'raise 'integer
     (lamda (x) (make-rational x 1)))
(put 'raise 'rational (lambda (x) (make-real (/ (number x) (denom x)))))
(put 'raise 'real
     (lambda (x) (make-from-real-imag x 0)))
                                        ;another solution
(define (integer->rational integer)
  (make-rational integer 1))

(define (rational->real rational)
  (define (integer->floating-point integer)
    (* integer 1.0))
  (make-real (/ (integer->floating-point (numer rational)))
             (denom rational)))
(define (real->complex real)
  (make-complex-from-real-imag real 0))

(put-coersion 'integer 'rational integer->rational)
(put-coersion 'rational 'real  rational->real)
(put-coersion 'real 'complex real->complex)

(define (raise number)
  (define tower '(integer rational real complex))
  (define (try tower)
    (if (< (length tower) 2)
        (error "couldn't raise type" number)
        (let ((current-type (car tower))
              (next-types (cdr tower))
              (next-type (car next-types)))
          (if (eq? (type-tag number) current-type)
              ((get-coercion current-type next-type) number)
              (try next-types)))))
  (try tower))

                                        ;2.84
(define (apply-generic op . args)
  (define (higher type1 type2)
    (define tower '())
    (define (iter tower)
      (cond ((= type1 (car tower)) type1)
            ((= type2 (car tower)) type2)
            (else (iter (cdr tower)))))
    (iter tower))
  (define (type-tags args)
    (map type-tag args))
  (define (try-coerce-to target)
    (map (lambda (x)
           (if (= (higher target x) (type-tag target))
               ((get 'raise (list (type-tag x))) x)
               x))
         args))
  (define (iterate next)
    (if (null? next)
        (error)
        (let ((coerced (try-coerce-to (car next))))
          (let ((proc (get op (type-tags coerced))))
            (if proc
                (apply proc (map contents coerced))
                (iterate (cdr next)))))))
  (let ((proc (get op (type-tags args))))
    (if proc
        (apply proc (map contents args))
        (iterate args))))
                                        ;2.85
(put 'project 'complex                     ;complex
 (lambda (x) (make-real (real-part x))))

(put 'project 'real ;real
     (lambda (x)
       (let ((rat (rationalize (inexact-exat x) 1/100)))
         (make-rational
          (numberator rat)
          (denominator rat)))))

(put 'project 'rational;rational
(lambda (x) (make-scheme-number (round (/ (number x) (denom x))))))

(define (drop x)
  (let ((project (get 'project (type-tag x))))
    (if project
        (if (equ? (project x) x)
            (drop (project x))
            x)
        x)))

(define (apply-generic op . args)
  (define (higher type1 type2)
    (define tower '())
    (define (iter tower)
      (cond ((= type1 (car tower)) type1)
            ((= type2 (car tower)) type2)
            (else (iter (cdr tower)))))
    (iter tower))
  (define (type-tags args)
    (map type-tag args))
  (define (try-coerce-to target)
    (map (lambda (x)
           (if (= (higher target x) (type-tag target))
               ((get 'raise (list (type-tag x))) x)
               x))
         args))
  (define (iterate next)
    (if (null? next)
        (error)
        (let ((coerced (try-coerce-to (car next))))
          (let ((proc (get op (type-tags coerced))))
            (if proc
                (apply proc (map contents coerced))
                (iterate (cdr next)))))))
  (let ((proc (get op (type-tags args))))
    (if proc
        (drop (apply proc (map contents args)))
        (iterate args))))
                                        ;2.86
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
         (and (= (real-part z1) (real-part z1))
              (= (imag-part z1) (imag-part z1)))))
  (put '=zero? '(complex)
       (lambda (z1)
         (= (real-part z1) (imag-part z1) 0)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (/ ((apply-generic 'sin y x) (apply-generic 'cos y x)))
           (else (error ""))))
    dispatch))

(define (sine x) (apply-generic 'sine x))

(define (cosine x) (apply-generic 'cosine x))

(put 'sine 'scheme-number
     (lambda (x) (tag (sin x))))

(put 'cosine 'scheme-number
     (lambda (x) (tag (cos x))))

(put 'sine 'rational
     (lambda (x) (tag (sin x))))

(put 'cosine 'rational
     (lambda (x) (tag (cos x))))
()
                                        ;
(define (add-poly p1 p2)
  (if (same-varialbe? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in save var")))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var")))
n
(define (install-polynomial-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define add-poly add-poly)
  (define mul-poly mul-poly)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomail
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms rest-terms L1) L2))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-terms-by-all-terms (first-term L1) L2)
                 (mul-terms-by-all-terms (rest-terms L1) L2))))

(define (mul-terms-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-terms-by-all-terms t1 (rest-terms L))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empt-termlist) '())

(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

                                        ;2.87
(put '=zero 'polynomial =zero?)

(define (=zero? coeff)
  (let ((terms (terms-list coeff)))
    (cond ((empty-termlist? terms) #t)
          ((=zero? (coeff (first-term terms))) (=zero? (rest-terms terms)))
          (else #f))))
                                        ;2.88
(define (sub-poly p1 p2)
  (if (same-varialbe? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1) (term-list p2)))
      (error "Polys not in save var")))

(define (sub-terms L1 L2)
  (define (neg-termlist termlist)
    (if (empty-termlist? termlist)
        termlist
        (list
         (make-term (order (first-term termlist))(- (coeff (first-term termlist))))
         (neg-termlist (rest-terms termlist)))))
  (add-terms L1 (net-termlist L2)))

(put 'negate 'scheme-number
     (lambda (n) (tag (- n))))

(put 'negate 'rational
     (lambda (rat) (make-rational (- (numer rat)) (denom rat))))

(put 'negate 'complex
     (lambda (z) (make-from-real-imag (- (real-part z))
                                      (- (imag-part z)))))

(define (negate-terms termlist)
  (if (empty-termlist? termlist)
      the-empt-termlist
      (let ((t (first-term termlist)))
        (adjoin-term (make-term (order t) (negate (coeff t)))
                     (negate-terms (rest-terms termlist))))))

(put 'negate 'polynomial
     (lambda (poly) (make-polynomial (variable poly)
                                     (negate-terms (term-list poly)))))

(put 'sub '(polynomial polynomial)
     (lambda (x y) (tag (add-poly x (negate y)))))
                                        ;2.89
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (let (order (order term))
        (map (lambda (term)
               (if (= (order term) order)
                   (make-term order (+ (coeff ) (coeff term)))
                   term))
             term-list))))

(define (make-term order coeff) coeff)


(define (first-term term-list)
  (make-term (- (len term-list) 1) (car term-list)))

(define (adjoin-term term term-list)
  (cond ((=zero? term) term-list)
        ((=equ? (order term) (length term-list)) (cons (coeff term) term-list))
        (else (adjoin-term term (cons 0 term-list)))))
                                        ;2.90
(define (install-sparse-term-list)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons (term term-list))))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (tag term-list) (attach-tag 'sparse term-list))
  (put 'adjoin-term 'sparse adjoin-term)
  (put 'first-term '(sparse)
       (lambda (term-list) (first-term term-list)))
  (put 'rest-term '(sparse)
       (lambda (term-list) (tag (rest-terms term-list))))
  'done)

(define (install-dense-term-list)
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term))
           term-list)
          ((=equ? (order term) (length term-list)) (cons (coeff term) (term-list)))
          (else (adjoin-term term (cons 0 term-list)))))
  (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (tag term-list) (attach-tag 'dense term-list))
  (put 'adjoin-term 'dense adjoin-term)
  (put 'first-term '(dense)
       (lambda (term-list) (first-term term-list)))
  (put 'rest-term '(dense)
       (lambda (term-list) (tag (rest-terms term-list))))
  'done)

(define (adjoin-term term term-list)
  ((get 'adjoin-term (type-tag term-list)) term term-list))

(define (first-term term-list) (apply-generci 'first-term term-list))
(define (rest-term term-list) (apply-generci 'rest-term term-list))
                                        ;2.91
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms (add-terms L1 (mul-terms L2 (make-term (- new-o) new-c)))
                                L2)))   ;compute rest of resulbt recursively
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))))))));form complete result

(define (div-poly poly1 poly2)
  (if (not (eq? (variable poly1) (variable poly2)))
      (error "")
      (let ((ls (div-terms poly1 poly2)))
        (let ((quotient-terms (car ls))
              (remainder-terms (cadr ls))
              (var (variable poly1)))
          (list (make-polynomial quotient-terms var) (make-polynomial remainder-terms var))))))
                                        ;2.92
(define (install-polynomial-packages)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (polynomial? p)
    (eq? 'polynomial (car p)))
  (define (variable p) (car p))
  (define (term-list p) (car p))
  (define (variable? x)
    (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (let ((ordered-poys (order-polys p1 p2)))
          ))))
