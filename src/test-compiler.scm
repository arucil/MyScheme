
(load "compiler.scm")

(define-syntax test
  (syntax-rules ()
    [(_ expr val)
     (begin
       (initialize!)
       (let ([d (call-with-string-output-port
                  (lambda (p)
                    (disassemble (meaning-toplevel 'expr) 0 p)))])
         (unless (equal? (string-trim d)
                         (string-trim val))
           (display 'expr)
           (display "\n--------- got:\n")
           (display d)
           (display "--------- expected:\n")
           (display val)
           (error 'test "test failed"))))]))

;;; tests

(test ()
      "
     0: return
      ")

;; quote & atom

(test (1)
      "
     0: const/1
     1: return
      ")

(test (#t #f '() 1 2 '1 '2 '(+ 1 2) "abC")
      "
     0: const/true
     1: const/false
     2: const/null
     3: const/1
     4: const           0  ; 2
     6: const/1
     7: const           0  ; 2
     9: const           1  ; (+ 1 2)
     B: const           2  ; \"abC\"
     D: return
      "
      )

;; if

(test ((if '(a) 3 1))
      "
     0: const           0  ; (a)
     2: goto-if-false   A
     5: const           1  ; 3
     7: goto            B
     A: const/1
     B: return
      ")

;; begin

(test ((begin 1 2 3 '(a)) -1)
      "
     0: const/1
     1: const           0  ; 2
     3: const           1  ; 3
     5: const           2  ; (a)
     7: const           3  ; -1
     9: return
      "
      )

;; lambda

(test ((lambda (x) 'x x))
      "
     0: closure         A
     3: func            1
     5: const           0  ; x
     7: shallow-ref     0
     9: return
     A: return
      "
      )

;; variable

(test ((lambda (x)
         x y car
         (lambda (y z)
           (lambda x x)
           (lambda (x y . z) z 1)
           x y z car))
       x
       cdr)
      "
     0: closure         2C
     3: func            1
     5: shallow-ref     0
     7: global-ref      46  ; y
     9: global-ref      0  ; car
     B: closure         2B
     E: func            2
    10: closure         18
    13: varfunc         0
    15: shallow-ref     0
    17: return
    18: closure         21
    1B: varfunc         2
    1D: shallow-ref     2
    1F: const/1
    20: return
    21: deep-ref        1 0
    24: shallow-ref     0
    26: shallow-ref     1
    28: global-ref      0  ; car
    2A: return
    2B: return
    2C: global-ref      47  ; x
    2E: global-ref      1  ; cdr
    30: return
      "
      )

;; set!

(test ((lambda (x y z)
         (lambda (a b)
           (set! a 0)
           (set! z #f))
         (set! y 1)
         (set! car "CAR"))
       (set! x 3)
       (set! cdr "CDR"))
      "
     0: closure         1A
     3: func            3
     5: closure         12
     8: func            2
     A: const/0
     B: shallow-set     0
     D: const/false
     E: deep-set        1 2
    11: return
    12: const/1
    13: shallow-set     1
    15: const           0  ; \"CAR\"
    17: global-set      0  ; car
    19: return
    1A: const           1  ; 3
    1C: global-set      46  ; x
    1E: const           2  ; \"CDR\"
    20: global-set      1  ; cdr
    22: return
      "
      )

;; define

(test (1
       (define x "12A")
       (set! y (begin 3 2))
       z
       (define z '(a . b)))
      "
     0: const/1
     1: const           0  ; \"12A\"
     3: global-set      46  ; x
     5: const           1  ; 3
     7: const           2  ; 2
     9: global-set      47  ; y
     B: global-ref      48  ; z
     D: const           3  ; (a . b)
     F: global-set      48  ; z
    11: return
      ")

(test ((define foo (lambda (x) x x))
       (define (bar) car 'x)
       f
       (define (f x) x))
      "
     0: closure         A
     3: func            1
     5: shallow-ref     0
     7: shallow-ref     0
     9: return
     A: global-set      46  ; foo
     C: closure         16
     F: func            0
    11: global-ref      0  ; car
    13: const           0  ; x
    15: return
    16: global-set      47  ; bar
    18: global-ref      48  ; f
    1A: closure         22
    1D: func            1
    1F: shallow-ref     0
    21: return
    22: global-set      48  ; f
    24: return
      ")

(test ((define (f x y) y x f)
       (define (g . x) x)
       (define (g x y . z) y z x))
      "
     0: closure         C
     3: func            2
     5: shallow-ref     1
     7: shallow-ref     0
     9: global-ref      46  ; f
     B: return
     C: global-set      46  ; f
     E: closure         16
    11: varfunc         0
    13: shallow-ref     0
    15: return
    16: global-set      47  ; g
    18: closure         24
    1B: varfunc         2
    1D: shallow-ref     1
    1F: shallow-ref     2
    21: shallow-ref     0
    23: return
    24: global-set      47  ; g
    26: return
      ")

;; application

(test ((car)
       (car '())
       (car 0 1))
      "
     0: global-ref      0  ; car
     2: call            0
     4: const/null
     5: push
     6: global-ref      0  ; car
     8: call            1
     A: const/0
     B: push
     C: const/1
     D: push
     E: global-ref      0  ; car
    10: tail-call       2
    12: return
      "
      )

(test ((begin (car 1)))
      "
     0: const/1
     1: push
     2: global-ref      0  ; car
     4: tail-call       1
     6: return
      ")

(test ((begin (car 1) (car #t)))
      "
     0: const/1
     1: push
     2: global-ref      0  ; car
     4: call            1
     6: const/true
     7: push
     8: global-ref      0  ; car
     A: tail-call       1
     C: return
      ")

(test ((if (car #t) 1 (car #f)))
      "
     0: const/true
     1: push
     2: global-ref      0  ; car
     4: call            1
     6: goto-if-false   D
     9: const/1
     A: goto            13
     D: const/false
     E: push
     F: global-ref      0  ; car
    11: tail-call       1
    13: return
      ")

(test (((lambda x x) 0 1))
      "
     0: const/0
     1: push
     2: const/1
     3: push
     4: extend-env      2
     6: varfunc         0
     8: shallow-ref     0
     A: return
      ")

(test (((lambda (x y) (cons y x)) 0 1)
       ((lambda () 1) #t)
       ((lambda (x) (x))))
      "
     0: const/0
     1: push
     2: const/1
     3: push
     4: extend-env      2
     6: func            2
     8: shallow-ref     1
     A: push
     B: shallow-ref     0
     D: push
     E: global-ref      2  ; cons
    10: call            2
    12: shrink-env
    13: const/true
    14: push
    15: extend-env      1
    17: func            0
    19: const/1
    1A: shrink-env
    1B: extend-env      0
    1D: func            1
    1F: shallow-ref     0
    21: tail-call       0
    23: return
      ")
