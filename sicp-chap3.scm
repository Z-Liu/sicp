(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "insufficient"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insuff"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unknown" m))))
  dispatch)
                                        ;3.1
(define (make-accumulator init)
  (lambda (accu arg)
    (set! init (+ init arg))
    init))
                                        ;3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (sym)
      (cond ((eq? sym 'how-many-calls?)
             count)
            ((eq? sym 'reset-count)
             (set! count 0))
            (else (begin
                    (set! count (+ count 1))
                    (f sym)))))))
                                        ;3.3
(define (make-account1 balance sym)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insuff"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch s m)
    (if (not (eq? s sym))
        (error "incorrect password")
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "unknown" m)))))
  dispatch)
                                        ;3.4
(define (make-account2 balance sym)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insuff"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define times 0)
  (define (dispatch s m)
    (if (not (eq? s sym))
        (begin
          (set! times (+ times 1))
          (if (> times 3)
              (error "call the cops")
              (error "incorrect password" times "time")))
        (begin
          (set! times 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "unknown" m))))))
  dispatch)
                                        ;
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (montecarlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaing 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trails-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trails initial-x)
  (define (iter tirals-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))
                                        ;3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trails)
  (define (experiment)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (let ((area (* (- y1 x1) (- y2 x2))))
    (* area (monte-carlo trials experiment))))
                                        ;3.6

(define random-init 1)

(define rand-2
  (let ((x random-init))
    (lambda (sym)
      (cond ((eq? sym 'generate)
             (set! x (rand-update x))
             x)
            ((eq? sym 'reset)
             (lambda (new-value)
               (set! x new-value)))
            (else (error "error")))
      )))


(define rand-3 
  (let ((x random-init)) 
    (define (dispatch message) 
      (cond ((eq? message 'generate) 
             (set! x (rand-update x)) 
             x) 
            ((eq? message 'reset) 
             (lambda (new-value) (set! x new-value))))) 
    dispatch)) 
                                        ;3.7
(define (make-account2 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insuff"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (custom-cond s method)
    (cond ((eq? method 'withdraw) withdraw)
          ((eq? method 'deposit) deposit)
          ((eq? method 'alias) alias)
          (else (error "unknown" method))))
  (define (alias new-password)
    (define (inner-dispatch s method)
      (if (not (eq? s new-password))
          (error "incorrect password")
          (custom-cond s method)))
    inner-dispatch)
  (define (dispatch s method)
    (if (not (eq? s password))
        (error "incorrect password")
        (custom-cond s method)))
  dispatch)

(define (make-joint account old-password new-password)
  ((account old-password 'alias) new-password))
                                        ;3.8
(define status 2)

(define (f arg)
  (if (= status 2)
      (set! status arg))
  status)
                                        ;3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
                                        ;3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
                                        ;3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
                                        ;3.17
                                        ;working version
(define (count-pairs x)
  (let ((record '(t)))
    (define (in-record? x record)
      (cond ((eq? record '()) 'f)
            ((eq? (car record) x) 't)
            (else
             (in-record? x (cdr record)))))

    (define (append-record x)
      (set-cdr! record x))

    (cond ((not (pair? x)) 0)
          (else
           (display "x: ")
           (display x)
           (newline)

           (display "in-record?: ")
           (display (in-record? x record))
           (newline)

           (display "before append-record: ")
           (display record)
           (newline)

           (append-record x)

           (display "after append-record: ")
           (display record)
           (newline)

           (+ (count-pairs (car x))
              (count-pairs (cdr x))
              1)))))

(define (count-pairs2 x)
  (let ((record '(tmp)))
    (define (in-record? x)
      (define (inner x record)
        (cond ((eq? record '()) #f)
              ((eq? (car record) x) #t)
              (else
               (inner x (cdr record)))))
      (inner x record))

    (define (last-pair x)
      (if (null? (cdr x))
          x
          (last-pair (cdr x))))
;it seems append! append the last pair of y?
    (define (append! x y)
      (display "x y")
      (newline)
      (display x)
      (newline)
      (display y)
      (newline)

      (set-cdr! (last-pair x) y)
      x)

    (define (append-record x)
      (begin
        (display "before append-record: ")
        (display record)
        (newline)

        (append! record (list x))

        (display "after append-record: ")
        (display record)
        (newline)))

    (display "x: ")
    (display x)
    (newline)

    (if (not (pair? x))
        (begin
          (display "not pair")
          (newline)
          0)
        (if (begin
              (display "in-record?: ")
              (display (in-record? x))
              (newline)

              (in-record? x))
            0
            (begin
              (append-record x)
              (+ (count-pairs2 (car x))
                 (count-pairs2 (cdr x))
                 1))))))

(define (count-pairs x)
  (let ((counted '()))
    (define (uncounted? x)
      (if (memq x counted)
          0
          (begin
            (set! counted (cons x counted))
            1)))
    (define (count x)
      (if (not (pair? x))
          0
          (+ (count (car x))
             (count (cdr x))
             (uncounted? x))))
    (count x)))
                                        ;3.18
(define (exam-outer l)
  (define (lst-in? lst records) 
    (cond ((null? records) #f) 
          ((eq? (car records) lst) #t) 
          (else (lst-in? lst (cdr records)))))
  (let ((counted '()))
    (define (exam l)
      (if (not (pair? l))
          #f
          (let ((x (car l)))
            (cond ((lst-in? x counted) #t)
                  (else (begin
                          (set! counted (cons x counted))
                          (or
                           (exam (car l))
                           (exam (cdr l)))))))))
    (exam l)))

(define (has-cycle? seq) 
  
  (define (lst-in? lst records) 
    (cond ((null? records) false) 
          ((eq? (car records) lst) true) 
          (else (lst-in? lst (cdr records))))) 
  
  (define (has-cycle-1? processed lst) 
    (cond ((not (pair? lst)) false) 
          ((lst-in? lst processed) true) 
          (else 
           (or (has-cycle-1? (cons lst processed) (car lst)) 
               (has-cycle-1? (cons lst processed) (cdr lst)))))) 
  
  (has-cycle-1? '() seq)) 

                                        ;3.19
(define (exam-outer l)
  (define (lst-in? lst records) 
    (cond ((null? records) #f) 
          ((eq? (car records) lst) #t) 
          (else (lst-in? lst (cdr records)))))
  (define (remove-unnecc x counted)

    (let ((filterd-counted ()))
;filter out impossibly making-circle node
     (cons x filterd-counted)))

  (let ((counted '()))
    (define (exam l)
      (if (not (pair? l))
          #f
          (let ((x (car l)))
            (cond ((lst-in? x counted) #t)
                  (else (begin
                          (set! counted (remove-unnecc x counted))
                          (or
                           (exam (car l))
                           (exam (cdr l)))))))))
    (exam l)))

(define (contains-cycle? lst)
  (define (safe-cdr l)
    (if (pair? l)
        (cdr l)
        '()))
  (define (iter a b)
    (cond ((not (pair? a)) #f)
          ((not (pair? b)) #f)
          ((eq? a b) #t)
          ((eq? a (safe-cdr b)) #t)
          (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))
                                        ;3.21
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "")
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error ""))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define (print-queue queue)
  (car queue))

(define (print-queue queue) (map display (front-ptr queue)))
                                        ;3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue )
      (if (empty-queue?)
          (error "")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error ""))
            (else (set-front-ptr! (cdr front-ptr))
                  dispatch)))
    (define (print-queue)
      (map display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue?) font-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)))
    dispatch)))
(
                                        ;3.23
(define (make-deque) (cons '() '()))

(define (empty-deque? queue)
  (null? (front-ptr queue)))

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (front-deque queue)
  (if (empty-queue? queue)
      (error "")
      (car (front-ptr queue))))

(define (rear-deque queue)
  (if (empty-queue? queue)
      (error "")
      (car (rear-ptr queue))))


(define (front-insert-deque! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (front-ptr queue) new-pair)
           (set-front-ptr! queue new-pair)
           queue))))

(define (rear-insert-deque! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (front-delete-deque! queue)
  (cond ((empty-queue? queue)
         (error ""))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define (rear-delete-deque! queue)
  (cond ((empty-queue? queue)
         (error ""))
        (else (set-rear-ptr! queue (cdr (rear-ptr queue)))
              queue)))

                                        ;
(define (make-decell left value right)
  (cons (cons value left) right))
(define (val-decell decell) (caar decell))
(define (left-decell decell)
  (if (not (null? (cdr (car decell))))
      ((cdr (car)))
      '()))
(define (right-decell decell) (cdr decell))
(define (set-right-decell! decell right-decell)
  (set-cdr! decell right-decell))
(define (set-left-decell! decell left-decell)
  (set-cdr! (car decell)
            (lambda () left-decell)))

(define (connect-decell! l-decell r-decell)
  (set-left-decell! r-decell l-decell)
  (set-right-decell! l-decell r-decell))

(define (front-dptr deque) (car deque))
(define (rear-dptr deque) (cdr deque))
(define (set-front-dptr! deque decell) (set-car! deque decell))
(define (set-rear-dptr! deque decell) (set-cdr! deque decell))

(define (make-deque) (cons '() '()))

(define (empty-deque? deque)
  (or (null? (front-dptr deque))
      (null? (rear-dptr deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error)
      (val-decell (front-dptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error)
      (val-decell (rear-dptr deque))))

(define (set-first-deque! deque decell)
  (set-front-dptr! deque decell)
  (set-rear-dptr! deque decell))

(define (front-insert-deque! deque item)
  (let ((decell (make-decell '() item '())))
    (cond ((empty-deque? deque)
           (set-first-deque! deque decell))
          (else
           (connect-decell! decell (front-dptr deque))
           (set-rear-dptr! deque decell)))
    deque))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error))
        (else
         (set-front-dptr! deque (right-decell (front-dptr deque)))
         (if (not (empty-deque? deque))
             (set-left-decell! (front-dptr deque) '()))
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error))
        (else
         (set-rear-dptr! deque (left-decell (rear-dptr deque)))
         (if (not (empty-deque? deque))
             (set-right-decell! (rear-ptr deque) '()))
         deque)))

(define (deque->list deque)
  (define (iter decell)
    (if (null? decell)
        '()
        (cons (val-decell decell) (iter (right-decell decell)))))
  (if (empty-deque? decell)
      '()
      (iter (front-dptr deque))))
                                        ;
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))
                                        ;
(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtalbe (assoc key-1 (cdr table))))
    (if subtalbe
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)
                                        ;
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "unknown" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
                                        ;3.24
(define (same-key? key1 key2)
  (if ()
      #t
      #f))

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key?? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "unknown" m))))
    dispatch))
                                        ;3.25
(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup . keys table)
      (if (not (pair? (cdr table)))
          false
          (let ((subtable
                 (assoc (car keys) (cdr table))))
            (if subtable
                (cond 
                 ((not (pair? (cdr keys)))
                  (cdr subtable))
                 (else
                  (iter (cdr keys) (cdr table))))
                false)))
;可能无用
      (define (iter . keys table)
        (if (null? keys)
            false
                                        ;   ((not (assoc (car keys) table)) false)
                                        ;  ((pair? table) (cdr (assoc (car keys) table)))
))
      (iter keys local-table))

    (define (insert! . keys value)
      (define (iter! .keys value table)
        (if (not (pair? (cdr keys)))
            (set-cdr! table value)
            (let ((subtable
                   (assoc (car keys) (cdr local-table))))
              (if subtable
                  (set-cdr! subtable
                            (cons (iter! (cdr keys) value (cdr table))))
                  (set-cdr! table
                            (cons (list (iter! (cdr keys) value (cdr table))))))))

                                        ;可能无用
        (if (null? keys)

                                        ;            ((not (assoc (car keys) table)) (set-cdr! (cons (list (car keys) (insert! (cdr keys))) ) value))
                                        ;           (else)
))
      (iter keys value local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "unknown" m))))
    dispatch))
                                        ;
(define (make-table)
  (let ((local-table (list '*table* nil)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? records '(())) false)
            ((equal? key (car (car records))) (car records))
            (else (assoc key (cdr records)))))
    (define (find subtable keys)
      (let ((record (assoc (car keys) (cdr (cdr subtable)))))
        (if record
            (if (null? (cdr keys))
                (list nil record)
                (find record (cdr keys)))
            (list keys subtable))))
    (define (new-branch! table keys value)
      (define (recurse keys value)
        (if (null? (cdr keys))
            (cons (car keys) (list value))
            (cons (car keys) (list nil (recurse (cdr keys) value)))))
      (if (not (pair? keys))
          #f
          (set-cdr! (cdr table) (cons (recurse keys value) (cdr (cdr table))))))
    (define (display)
      (draw local-table))
    (define (insert! keys value)
      (let ((find-result (find local-table keys)))
        (let ((subkeys (car find-result))
              (subtable (car (cdr find-result))))
          (if (null? subkeys)
              (set-car! (cdr subtable) value)
              (new-branch! subtable subkeys value))))
      'ok)
    (define (lookup keys)
      (let ((find-result (find local-table keys)))
        (let ((subkeys (car find-result))
              (subtable (car (cdr find-result)))))
        (if (null? subkeys)
            (let ((value (car (cdr subtable))))
              (if (equal? value nil)
                  #f
                  value))
            #f)))))
                                        ;3.26
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-bracnh tree) (caddr tree))

(define (make-tree entry left)
  (list entry left entry))

(defie (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (car x) (car (entry set))) set)
        ((< (car x) (car (entry set)))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> (car x) (car (entry set)))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-bracnch set))))))

(define (make-table)
  (let ((local-table '()))
    (define (lookup key records)
      (cond ((null? records) #f)
            ((= key (car (entry records))) (entry records))
            ((< key (car (entry records))) (lookup key (left-branch records)))
            ((> key (car (entry records))) (lookup key (right-branch records)))))
    (define (insert! key value)
      (let ((record (lookup key local-table)))
        (if record
            (set-cdr! record value)
            (set! local-table (adjoin-set (cons key value) local-table)))))
    (define (get key)
      (lookup key local-table))
    (define (dispatch m)
      (cond ((eq? m 'get-proc) get)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'print) local-table)
            (else (error "" m))))
    dispatch))
                                        ;
(define a (make-wire))
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input inverter-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((or (= s1 0) (= s2 0)) 0)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error))))

                                        ;3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error))))

                                        ;3.29
(define (or-gate a1 a2 output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire))
        (f (make-wire))
        (g (make-wire)))
    (and-gate a1 a1 d)
    (and-gate a1 a2 e)
    (inverter d f)
    (inverter e g)
    (and-gate f g c)
    (inverter c output)
    'ok))
                                        ;3.30
(define (ripple-carry-adder .a .b .s c)
  (define (iter .a .b .s c-in c-out)
    (cond
     ((null? a) s)
     (else (full-adder (car a) (car b) c-in (car s) c-out)
           (iter (cdr a) (cdr b) (cdr s) c-out (make-wire)))))
  (iter .a .b .s c (make-wire)))

(define (ripple-carry-adder a-list b-list s-list c)
  (let ((c-list (map (lambda (x) (make-wire)) (cdr a-list)))
        (c-0 (make-wire)))
    (map full-adder
         a-list
         b-list
         (append c-list (list c-0))
         s-list
         (cons c c-list))
    'ok))
                                        ;
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-singal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error ""))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))
                                        ;
(make-agenda)

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
                                        ;3.32
(define (make-time-segment time queue)
  (cons time queue))

(define (setment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segements agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-tie (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (elongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "empty")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
                                        ;
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value2 sum))
           (set-value! a2
                       (- (get-value sum) (ge-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "unknown request" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "unkown"))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (prcess-forget-value))
          (else (error "Unknown" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "constradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor infrom-about-no-value constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if inform-about-value true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "unknow" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop iterms)
    (cond ((null? iterms) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car iterms))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
                                        ;3.33
(define (averager a b c)
  (let ((x (make-connector))
        (z (make-connector)))
    (adder a b x)
    (constant 2 z)
    (multiplier z c x)
    'ok))
                                        ;3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
                                        ;<alternative1>
            (set-value! a
                        (sqrt (get-value b))
                        me))
                                        ;<alternative2>
        (if (has-value? a)
            (set-value! b
                        (square (get-value a))
                        me))))
  (define (process-forget-value)
    ;<body1>
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    ;<body2>
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (eror "Unknown" request))))
                                        ;<rest of definition>
  (connect a me)
  (connect b me)
  me)
                                        ;3.37
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv v)
  (let ((z (make-connector)))
    (constant v z)
    z))
                                        ;chap3.4
(define (withdraw amount)
  (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "insufficient funds"))

                                        ;3.39

101
121
110!
11!
100
                                        ;3.40
                                        ;
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (with draw amount)
    (if (>= balance amount)
        (begin (set! balanace (- balance))
               balance)
        "insufficient"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))
                                        ;implementing serializers
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutext 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin
        (set-car! cell true)
        false)))
                                        ;3.47
(define (make-semaphore n)
  (define (iter l m)
    (if (= m n)
        l
        (iter (append l  (make-mutex)) (+ m 1))))
  (let ((cells (iter '() 0)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set-sem! cells)
                 (the-semaphore 'acquire)))
            ((eq? m 'release) (clear-sem! cells))))
    the-semaphore))


(define (clear-sem! cells)
  (define (iter cells)
    (let (cell (car cells))
      (if  (= (car cell) true)
           (clear! cell )
           (iter (cdr cells)))))
  (iter cells))

(define (test-and-set-sem! cells)
  (define (iter cells)
    (let (cell (car cells))
      (if (not (test-and-set! cell))
          (iter (cdr cells)))))
  (iter cells))


(define (make-semaphore n)
  (let ((lock (mutex (make-mutex)))
        (resources n))
    (define )))
