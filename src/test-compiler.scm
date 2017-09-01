
(load "compiler.scm")

(define (string-trim s)
  (do ([i 0 (+ i 1)])
    [(not (char-whitespace? (string-ref s i)))
     (do ([j (- (string-length s) 1) (- j 1)])
       [(not (char-whitespace? (string-ref s j)))
        (substring s i (+ j 1))
        ])]))

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
           (error 'test "test failed"
                  'expr
                  d val))))]))

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

;; and

(test ((and) (and 1) (and '() 0) (and 1 2 '()))
      "
     0: const/true
     1: const/1
     2: const/null
     3: goto-if-false   7
     6: const/0
     7: const/1
     8: goto-if-false   11
     B: const           0  ; 2
     D: goto-if-false   11
    10: const/null
    11: return
      "
      )

;; or

(test ((or) (or 1) (or '() 0) (or 1 2 '()))
      "
     0: const/false
     1: const/1
     2: const/null
     3: goto-if-true    7
     6: const/0
     7: const/1
     8: goto-if-true    11
     B: const           0  ; 2
     D: goto-if-true    11
    10: const/null
    11: return
      "
      )

;; cond

(test ((cond [#t 1]))
      "
     0: const/true
     1: goto-if-false   8
     4: const/1
     5: goto            9
     8: const/false
     9: return
      "
      )

(test ((cond [else 0]))
      "
     0: const/0
     1: return
      "
      )

(test ((cond [#t 1] [else 0])
       (cond [#t 1] ['() 2 3 4] [#f 2 3]))
      "
     0: const/true
     1: goto-if-false   8
     4: const/1
     5: goto            9
     8: const/0
     9: const/true
     A: goto-if-false   11
     D: const/1
     E: goto            2A
    11: const/null
    12: goto-if-false   1E
    15: const           0  ; 2
    17: const           1  ; 3
    19: const           2  ; 4
    1B: goto            2A
    1E: const/false
    1F: goto-if-false   29
    22: const           0  ; 2
    24: const           1  ; 3
    26: goto            2A
    29: const/false
    2A: return
      "
      )

(test ((cond [#t 1] ['() 2 3] [#f 3 2] [else 3 4])
       (cond [else #t] [else #f]))
      "
     0: const/true
     1: goto-if-false   8
     4: const/1
     5: goto            22
     8: const/null
     9: goto-if-false   13
     C: const           0  ; 2
     E: const           1  ; 3
    10: goto            22
    13: const/false
    14: goto-if-false   1E
    17: const           1  ; 3
    19: const           0  ; 2
    1B: goto            22
    1E: const           1  ; 3
    20: const           2  ; 4
    22: global-ref      22  ; else
    24: goto-if-false   2B
    27: const/true
    28: goto            2C
    2B: const/false
    2C: return
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
     7: global-ref      22  ; y
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
    2C: global-ref      23  ; x
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
     5: closure          12
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
    1C: global-set      22  ; x
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
     3: global-set      22  ; x
     5: const           1  ; 3
     7: const           2  ; 2
     9: global-set      23  ; y
     B: global-ref      24  ; z
     D: const           3  ; (a . b)
     F: global-set      24  ; z
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
     A: global-set      22  ; foo
     C: closure         16
     F: func            0
    11: global-ref      0  ; car
    13: const           0  ; x
    15: return
    16: global-set      23  ; bar
    18: global-ref      24  ; f
    1A: closure         22
    1D: func            1
    1F: shallow-ref     0
    21: return
    22: global-set      24  ; f
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
     9: global-ref      22  ; f
     B: return
     C: global-set      22  ; f
     E: closure         16
    11: varfunc         0
    13: shallow-ref     0
    15: return
    16: global-set      23  ; g
    18: closure         24
    1B: varfunc         2
    1D: shallow-ref     1
    1F: shallow-ref     2
    21: shallow-ref     0
    23: return
    24: global-set      23  ; g
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
