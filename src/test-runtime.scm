

(load "runtime.scm")


(define-syntax test
  (syntax-rules ()
    [(_ e val printed)
     (begin
       (initialize!)
       (init-library!)
       (init-runtime!)
       (let* ([ret #f]
              [p (with-output-to-string
                   (lambda ()
                     (set! ret (run (meaning-toplevel 'e)))))])
         (unless (and (equal? ret val)
                      (equal? (string-trim p)
                              (string-trim printed)))
           (display 'e)
           (display "\n---------- expected:\n")
           (write val)
           (display "\n---------- got:\n")
           (write ret)
           (display "\n---------- expected printing:\n")
           (display printed)
           (display "\n---------- got printing:\n")
           (display p)
           (error 'test "test failed"))))]
    [(_ e val)
     (test e val "")]))


;; quote

(test (#t) #t)
(test (#f) #f)
(test ('()) '())
(test (0) 0)
(test (1) 1)
(test (2) 2)
(test (#\a) #\a)
(test ("Hello") "Hello")
(test ('#t) #t)
(test ('0) 0)
(test ('2) 2)
(test ('"Hello") "Hello")
(test ('#\a) #\a)

;; quasiquote

(test (`x)
      'x)
(test (`())
      '())
(test (`"abc")
      "abc")
(test (`(list ,(+ 1 2) 4))
      '(list 3 4))
(test ((let ([name 'a])
         `(list ,name ',name)))
      '(list a 'a))
(test (`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
      '(a 3 4 5 6 b))
(test (`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
      '((foo 7) . cons))
(test (`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
      '#(10 5 2 4 3 8))
(test (`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
      '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
(test ((let ([name1 'x]
             [name2 'y])
         `(a `(b ,,name1 ,',name2 d) e)))
      '(a `(b ,x ,'y d) e))
(test (`,@x)
      ',@x)

;; recursion

(test ((define (fact x)
         (if (= 0 x)
             1
             (* x (fact (- x 1)))))
       (define (print x)
         (display x)
         (display " "))
       (print (fact 1))
       (print (fact 2))
       (print (fact 3))
       (print (fact 4))
       (print (fact 5))
       (print (fact 6))
       (print (fact 7))

       (define (fib x)
         (cond
          [(= x 0) 0]
          [(= x 1) 1]
          [else (+ (fib (- x 1))
                   (fib (- x 2)))]))
       (display ", ")
       (print (fib 0))
       (print (fib 1))
       (print (fib 2))
       (print (fib 3))
       (print (fib 4))
       (print (fib 5))
       (print (fib 6))
       (print (fib 7))
       (print (fib 8))
       (print (fib 9))
       (fib 10)
       )
      55
      "1 2 6 24 120 720 5040 , 0 1 1 2 3 5 8 13 21 34")

(test ((define (fact/k x k)
         (if (= 0 x)
             (k 1)
             (fact/k (- x 1)
                     (lambda (n)
                       (k (* n x))))))
       (define (print x)
         (display x)
         (display " "))
       (print (fact/k 1 (lambda (x) x)))
       (print (fact/k 2 (lambda (x) x)))
       (print (fact/k 3 (lambda (x) x)))
       (print (fact/k 4 (lambda (x) x)))
       (print (fact/k 5 (lambda (x) x)))
       (print (fact/k 6 (lambda (x) x)))
       (print (fact/k 7 (lambda (x) x)))

       (define (fib/k x k)
         (cond
          [(= x 0) (k 0)]
          [(= x 1) (k 1)]
          [else
           (fib/k (- x 1)
                  (lambda (n1)
                    (fib/k (- x 2)
                           (lambda (n2)
                             (k (+ n1 n2))))))]))
       (display ", ")
       (print (fib/k 0 (lambda (x) x)))
       (print (fib/k 1 (lambda (x) x)))
       (print (fib/k 2 (lambda (x) x)))
       (print (fib/k 3 (lambda (x) x)))
       (print (fib/k 4 (lambda (x) x)))
       (print (fib/k 5 (lambda (x) x)))
       (print (fib/k 6 (lambda (x) x)))
       (print (fib/k 7 (lambda (x) x)))
       (print (fib/k 8 (lambda (x) x)))
       (print (fib/k 9 (lambda (x) x)))
       (fib/k 10 (lambda (x) (+ x 1)))
       )
      56
      "1 2 6 24 120 720 5040 , 0 1 1 2 3 5 8 13 21 34")

;; apply

(test ((list (apply * '())
             (apply - '(6 2))
             (apply - 6 '(2))
             (apply - 6 2 '())
             (apply - 10 2 '(3 2))
             (apply list '(a b c))
             (apply list 'a 'b '(c d))))
      '(1 4 4 4 3 (a b c) (a b c d)))

;; append

(test ((list (append '(a b) (list 'c))
             (append)
             (append '(a b))
             (append '(a b) 'c)
             (append 'a)
             (append '() 'a)
             (append '() '() 'a)))
      '((a b c)
        ()
        (a b)
        (a b . c)
        a
        a
        a))

;; map

(test ((list (map - '(1 3 4))
             (map (lambda (x) (* x x))
                  '(1 3 -4))
             (map + '(1 2 3 4) '(-1 3 4 5))
             (map - '())
             (map - '(1 2 3 4) '(-1 3 4 5) '(-10 0 2 1))))
      '((-1 -3 -4)
        (1 9 16)
        (0 5 7 9)
        ()
        (12 -1 -3 -2)))

;; eval

(test ((define (print x)
         (display x)
         (display " "))
       (define x #f)
       (define y #f)
       (set! x 1)
       (print x)
       (eval '(set! x (+ x 1)))
       (print x)
       (set! x (* x 2))
       (eval '(print x))
       (eval '(begin
                (print (cons x y))
                (list 'a 'b))))
      '(a b)
      "1 2 4 (4 . #f)")

;; call/cc

(test ((((call/cc call/cc) (lambda (x) x)) 'abc))
      'abc)

(test ((define (prod . ls)
         (call/cc
          (lambda (break)
            (letrec ([f (lambda (ls)
                          (cond
                           [(null? ls) 1]
                           [(= 0 (car ls))
                            (break 0)]
                           [else (* (car ls)
                                    (f (cdr ls)))]))])
              (f ls)))))
       (list (prod 2 3 1 4 5)
             (prod 7 3 2 -3)
             (prod)
             (prod 1 1 1 1 1)
             (prod 3 4 5 6 7 8 0 2)
             (prod 0 1 2 3 4)
             (prod 2 3 4 0 7 8 2)))
      '(120 -126 1 1 0 0 0))

(test ((define (fail v)
         (exit (list "amb failed")))

       (define (amb . ls)
         (call/cc
          (lambda (return)
            (let ([fail-save fail])
              (map
               (lambda (x)
                 (call/cc
                  (lambda (backtrack)
                    (set! fail backtrack)
                    (return x))))
               ls)
              (set! fail fail-save)
              (fail #f)))))

       (define (print x)
         (display x)
         (display " "))

       (let ([x (amb 1 2 3)]
             [y (amb 4 5 6)])
         (if (= 15 (* x y))
             (print (list x y))
             (amb)))

       (define (joins? a b)
         (equal? (string-ref b 0)
                 (string-ref a (- (string-length a) 1))))

       (let ([w1 (amb "the" "that" "a")]
             [w2 (amb "frog" "elephant" "thing")]
             [w3 (amb "walked" "treaded" "grows")]
             [w4 (amb "slowly" "quickly")])
         (if (and (joins? w1 w2)
                  (joins? w2 w3)
                  (joins? w3 w4))
             (list w1 w2 w3 w4)
             (amb)))
       )
      '("that" "thing" "grows" "slowly")
      "(3 5)")

(test ((define (make-box value)
         (let ([box (call/cc
                     (lambda (exit)
                       (letrec ([behavior
                                 (call/cc
                                  (lambda (store)
                                    (exit
                                     (lambda (msg . new)
                                       (call/cc
                                        (lambda (caller)
                                          (cond
                                           [(eq? 'get msg)
                                            (store (cons (car behavior)
                                                         caller))]
                                           [(eq? 'set msg)
                                            (store (cons (car new)
                                                         caller))])))))))])
                         ((cdr behavior) (car behavior)))))])
           (box 'set value)
           box))
       (define box1 (make-box 33))
       (let ([v1 (box1 'get)])
         (list v1
               (begin
                 (box1 'set 44)
                 (box1 'get))))
       )
      '(33 44))

