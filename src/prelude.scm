
(define-syntax let
  (syntax-rules ()
    [(_ ([name exp] ...) e1 e2 ...)
     ((lambda (name ...) e1 e2 ...) exp ...)]
    [(_ proc ([name exp] ...) e1 e2 ...)
     ((letrec ([proc (lambda (name ...)
                       e1 e2 ...)])
        proc)
      exp ...)]))

(define-syntax let*
  (syntax-rules ()
    [(_ () e1 e2 ...)
     (begin e1 e2 ...)]
    [(_ ([name1 exp1] [name2 exp2] ...) e1 e2 ...)
     (let ([name1 exp1])
       (let* ([name2 exp2] ...)
         e1 e2 ...))]))

(define-syntax letrec
  (syntax-rules ()
    [(_ ([name exp] ...) e1 e2 ...)
     (let ([name #f] ...)
       (set! name exp) ...
       e1 e2 ...)]))

(define-syntax and
  (syntax-rules ()
    [(_) #t]
    [(_ e1) e1]
    [(_ e1 e2 ...)
     (if e1
         (and e2 ...)
         #f)]))

(define-syntax or
  (syntax-rules ()
    [(_) #f]
    [(_ e1) e1]
    [(_ e1 e2 ...)
     (let ([t e1])
       (if t
           t
           (or e2 ...)))]))

(define-syntax cond
  (syntax-rules (else =>)
    [(_ [else e1 e2 ...])
     (begin e1 e2 ...)]
    [(_ [test])
     (let ([t test])
       (if t
           t))]
    [(_ [test] c1 c2 ...)
     (let ([t test])
       (if t
           t
           (cond c1 c2 ...)))]
    [(_ [test => e])
     (let ([t test])
       (if t
           (e t)))]
    [(_ [test => e] c1 c2 ...)
     (let ([t test])
       (if t
           (e t)
           (cond c1 c2 ...)))]
    [(_ [test e1 e2 ...])
     (if test
       (begin e1 e2 ...))]
    [(_ [test e1 e2 ...] c1 c2 ...)
     (if test
         (begin e1 e2 ...)
         (cond c1 c2 ...))]))

(define-syntax case
  (syntax-rules (else)
    [(_ (e1 ...) clause ...) ;; apply application first
     (let ([atom-key (e1 ...)])
       (case atom-key clause ...))]
    [(_ key [else e1 e2 ...])
     (begin e1 e2 ...)]
    [(_ key [(d ...) e1 e2 ...])
     (if (memv key '(d ...))
         (begin e1 e2 ...))]
    [(_ key [(d ...) e1 e2 ...] clause ...)
     (if (memv key '(d ...))
         (begin e1 e2 ...)
         (case key clause ...))]))

(define-syntax do
  (syntax-rules ()
    [(_ ([var init step ...] ...)
        [test r1 ...]
        e1 ...)
     (let loop ([var init] ...)
       (if test
           (begin
             #f
             r1 ...)
           (begin
             e1 ...
             (loop (do-step var step ...) ...))))]))

(define-syntax do-step
  (syntax-rules ()
    [(_ x y)
     y]
    [(_ x)
     x]))

(define-syntax quasiquote
  (syntax-rules ()
    [(_ e) (qq-expand e)]))

(define-syntax qq-expand
  (syntax-rules (quasiquote unquote unquote-splicing)
    [(_ `x . lv)
     (list 'quasiquote
           (qq-expand x #f . lv))]
    [(_ ,x)
     x]
    [(_ ,x #f . lv)
     (list 'unquote
           (qq-expand x . lv))]
    [(_ (,@x . y))
     (append x
             (qq-expand y))]
    [(_ (,@x . y) #f . lv)
     (cons (list 'unquote-splicing
                 (qq-expand x . lv))
           (qq-expand y #f . lv))]
    [(_ #(x ...) . lv)
     (list->vector (qq-expand (x ...) . lv))]
    [(_ (x . y) . lv)
     (cons (qq-expand x . lv)
           (qq-expand y . lv))]
    [(_ x . lv) 'x]))


(define map
  (letrec ([map1 (lambda (f ls)
                   (if (null? ls)
                       '()
                       (cons (f (car ls))
                             (map1 f (cdr ls)))))])
    (lambda (f . lss)
      (if (null? (car lss))
          '()
          (cons (apply f (map1 car lss))
                (apply map f (map1 cdr lss)))))))

(define (not val)
  (if val #f #t))

(define length
  (letrec ([f (lambda (ls len)
                (if (null? ls)
                    len
                    (f (cdr ls) (+ len 1))))])
    (lambda (ls)
      (f ls 0))))

(define (list . x)
  x)

(define append
  (letrec ([f (lambda (ls1 ls*)
                (cond
                 [(null? ls*) ls1]
                 [(null? ls1) (f (car ls*)
                                 (cdr ls*))]
                 [else        (cons (car ls1)
                                    (f (cdr ls1) ls*))]))])
    (lambda lss
      (if (null? lss)
          '()
          (f (car lss)
             (cdr lss))))))


(define memq #f)
(define memv #f)
(define member #f)

(let ()
  (define (make-member-proc pred)
    (letrec ([f (lambda (x ls)
                  (cond
                   [(null? ls) #f]
                   [(pred x (car ls)) ls]
                   [else (f x (cdr ls))]))])
      f))
  (set! memq (make-member-proc eq?))
  (set! memv (make-member-proc eqv?))
  (set! member (make-member-proc equal?)))


(define assq #f)
(define assv #f)
(define assoc #f)

(let ()
  (define (make-assoc-proc pred)
    (letrec ([f (lambda (x ls)
                  (cond
                   [(null? ls) #f]
                   [(pred x (caar ls)) (car ls)]
                   [else (f x (cdr ls))]))])
      f))
  (set! assq (make-assoc-proc eq?))
  (set! assv (make-assoc-proc eqv?))
  (set! assoc (make-assoc-proc equal?)))


;; multiple values

(define values #f)
(define call-with-values #f)

(let ([multiple-value-tag (cons 'multiple 'values)])
  (define (multiple-values? p)
    (and (pair? p) (eq? multiple-value-tag (car p))))

  (set! values
    (lambda args
      (if (and (not (null? args))
               (null? (cdr args)))
          (car args)
          (cons multiple-value-tag args))))

  (set! call-with-values
    (lambda (producer consumer)
      (let ([v (producer)])
        (if (multiple-values? v)
            (apply consumer (cdr v))
            (consumer v)))))

  (set! call/cc
    (let ([primitive-call/cc call/cc])
      (lambda (p)
        (primitive-call/cc
         (lambda (k)
           (p (lambda args
                (k (apply values args))))))))))
