
(load "define.scm")

(define (compile-error . args)
  (error 'compile "compile error" args)
  'stub)

;;;;;;;;;;;;;;;;;;;;        compile        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (meaning-toplevel e*)
  (define (a-meaning-toplevel e tail?)
    (if (and (pair? e) (eq? (car e) 'define))
      (meaning-define e tail?)
      (meaning e cenv tail?)))
  (let rec ([e* e*])
    (if (null? (cdr e*))
      (a-meaning-toplevel (car e*) #t)
      (cons (a-meaning-toplevel (car e*) #f)
            (rec (cdr e*))))))

(define (meaning e cenv tail?)
  (cond
   [(or (boolean? e)
        (number? e)
        (string? e)
        (char? e))
    (meaning-quote e cenv tail?)]
   [(symbol? e)
    (meaning-reference e cenv tail?)]
   [else
    (if (pair? e)
        (case (car e)
          [(quote)  (meaning-quote (cadr e) cenv tail?)]
          [(if)     (meaning-if (cadr e) (caddr e) (cadddr e) cenv tail?)]
          [(set!)   (meaning-set (cadr e) (caddr e) cenv tail?)]
          [(define) (compile-error "define only allowed at toplevel")]
          [(lambda) (meaning-lambda (cadr e) (cddr e) cenv tail?)]
          [(begin)  (meaning-sequence (cdr e) cenv tail?)]
          [(and)    (meaning-and/or (cdr e) cenv tail? #t)]
          [(or)     (meaning-and/or (cdr e) cenv tail? #f)]
          [(cond)   (meaning-cond (cdr e) cenv tail?)]
          [(let)    (meaning-let (cadr e) (cddr e) cenv tail?)]
          [else     (meaning-application e cenv tail?)])
        (compile-error "Invalid syntax" e))]))

(define (meaning-quote c cenv tail?)
  (case c
    [(#f) (instruction-encode 'const-false)]
    [(#t) (instruction-encode 'const-true)]
    [(()) (instruction-encode 'const-null)]
    [(0)  (instruction-encode 'const-0)]
    [(1)  (instruction-encode 'const-1)]
    [else (instruction-encode 'const (get-constant-index c))]))

(define (meaning-if e1 e2 e3 cenv tail?)
  (let* ([m3 (meaning e3 cenv tail?)]
         [m2 (append (meaning e2 cenv tail?)
                    (gen-goto 'goto (length m3)))]
         [m1 (append (meaning e1 cenv #f)
                    (gen-goto 'goto-if-false (length m2)))])
    (append m1 m2 m3)))

(define (meaning-define e tail?)
  (if (list? (cadr e))
    ;; (define (f . args) . body)  =>  (set! f (lambda args) . body)
    (meaning `(set! ,(caadr e)
                (lambda ,(cdadr e)
                  . ,(cddr e)))
             (init-cenv) tail?)
    ;; (define x e)  =>  (set! x e)
    (meaning `(set! ,(cadr e)
                ,(caddr e))
             (init-cenv) tail?)))

(define (meaning-set name e cenv tail?)
  (append
    (meaning e cenv #f)
    (let ([addr (get-variable-address name cenv)])
      (case (car addr)
        [(local)
         (if (zero? (cadr addr))
           (instruction-encode 'shallow-set (cddr addr))
           (instruction-encode 'deep-set (cadr addr) (cddr addr)))]
        [(global)
         (instruction-encode 'global-set (cdr addr))]
        [else (error 'meaning-set "unreachable")]))))

(define (meaning-reference name cenv tail?)
  (let ([addr (get-variable-address name cenv)])
    (case (car addr)
      [(local)
       (if (zero? (cadr addr))
           (instruction-encode 'shallow-ref (cddr addr))
           (instruction-encode 'deep-ref (cadr addr) (cddr addr)))]
      [(global)
       (instruction-encode 'global-ref (cdr addr))]
      [else (error 'meaing-reference "unreachable")])))

(define (meaning-sequence e+ cenv tail?)
  (let loop ([e+ e+])
    (if (null? (cdr e+))
      (meaning (car e+) cenv tail?)
      (append
        (meaning (car e+) cenv #f)
        (loop (cdr e+))))))

(define (meaning-cond e+ cenv tail?)
  ;; (cond [e1 . body] ...)
  ;; =>
  ;; (if e1 (begin . body) . ...)
  (letrec ([cvt (lambda (e+)
                  (cond
                    [(null? e+) #f]
                    [(and (null? (cdr e+))
                          (eq? (caar e+) 'else))
                     `(begin . ,(cdar e+))]
                    [else
                      `(if ,(caar e+)
                         (begin . ,(cdar e+))
                         ,(cvt (cdr e+)))]))])
    (meaning (cvt e+) cenv tail?)))

(define (meaning-let vv* body cenv tail?)
  ;; (let ([name value] ...) . body)
  ;; =>
  ;; ((lambda (name ...) . body) value ...)
  (meaning `((lambda ,(map car vv*) . ,body)
             ,(map cadr vv*))
           cenv tail?))

(define (meaning-and/or e* cenv tail? is-and?)
  (if (null? e*)
    (meaning is-and? cenv tail?)
    (let rec ([e+ e*])
      (if (null? (cdr e+))
        (meaning (car e+) cenv tail?)
        (let ([m1* (rec (cdr e+))])
          (append (meaning (car e+) cenv #f)
                  (gen-goto (if is-and?
                              'goto-if-false
                              'goto-if-true)
                            (length m1*))
                  m1*))))))

(define (meaning-lambda args body cenv tail?)
  (let* ([m1 (meaning-sequence body
                               (extend-cenv args cenv)
                               #t)]
         [size (length m1)]
         [n (variadic? args)])
    (append (instruction-encode 'closure
                                (modulo size 256)
                                (quotient size 256))
            (if n
                (instruction-encode 'varfunc n)
                (instruction-encode 'func (length args)))
            m1)))

(define (meaning-application e+ cenv tail?)
  (define (chain-params ls)
    (if (null? ls)
        '()
        (cons (f (car ls))
              (map-lr f (cdr ls)))))
  (display "here")
  (append (append (map-lr (lambda (e)
                            (append (meaning e cenv #f)
                                    (instruction-encode 'push)))
                          (cdr e+)))
          (meaning (car e+) cenv #f)
          (instruction-encode (if tail? 'tail-call 'call) (length (cdr e+)))))

;;;;;;;;;;;;;;;;;;  auxiliary functions

(define (gen-goto code offset)
  (if (> offset 65535)
    (compile-error "too long jump" offset)
    (instruction-encode code
                        (modulo offset 256)
                        (quotient offset 256))))

(define (get-variable-address name cenv)
  (let loop ([cenv cenv] [i 0])
    (if (null? cenv)
       `(global . ,(get-global-index name))
       (let loop2 ([rib (car cenv)] [j 0])
         (cond
           [(null? rib)
            (loop (cdr cenv) (+ i 1))]
           [(eq? (car rib) name)
            `(local ,i . ,j)]
           [else (loop2 (cdr rib) (+ j 1))])))))

(define (init-cenv) '())

(define (extend-cenv v* cenv)
  (letrec ([f (lambda (v*)
                (if (and (pair? v*)
                         (not (null? (cdr v*))))
                  (set-cdr! v* (cons (cdr v*) '()))))])
    (cons (f v*) cenv)))

(define (variadic? v*)
  (let loop ([v* v*] [i 0])
    (cond
     [(null? v*) #f]
     [(symbol? v*) i]
     [else (loop (cdr v*) (+ i 1))])))
