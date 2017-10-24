
(define *macros* #f)


(define (init-macros!)
  (set! *macros* '()))


;; works for both lists and improper lists
(define (for-all* p ls)
  (cond
   [(null? ls) #t]
   [(pair? ls) (and (p (car ls))
                    (for-all* p (cdr ls)))]
   [else (p ls)]))

;; works for both lists and improper lists
(define (map* f ls)
  (cond
   [(null? ls) '()]
   [(pair? ls) (cons (f (car ls))
                     (map* f (cdr ls)))]
   [else (f ls)]))

;; take first n items from list ls
(define (take ls n)
  (cond
   [(or (null? ls)
        (zero? n)) '()]
   [else (cons (car ls)
               (take (cdr ls) (- n 1)))]))

(define (indv x ls)
  (let f ([ls ls] [i 0])
    (cond
     [(null? ls) #f]
     [(eqv? (car ls) x) i]
     [else (f (cdr ls) (+ i 1))])))


;;;;;;;;;;;;;;;;;;;;;;          marking        ;;;;;;;;;;;;;;;;;;;;;;;;

(define new-mark #f)

(define (init-mark!)
  (set! new-mark
    (let ([i 0])
      (lambda ()
        (set! i (+ i 1))
        i))))

(define (mark-identifier name mark)
  (string->symbol
   (string-append
    (symbol->string name)
    "$"
    (number->string mark))))

(define (marked-identifier? name)
  (let* ([ls (reverse
              (string->list
               (symbol->string name)))]
         [i (indv #\$ ls)])
    (and i
         (string->number
          (list->string
           (take ls i))))))

(define (unmark e env)
  (let rec ([e e])
    (cond
     [(symbol? e)
      (if (marked-identifier? e)
          (apply-env env e)
          e)]
     [(pair? e)
      (cons (rec (car e))
            (rec (cdr e)))]
     [(vector? e)
      (vector-map rec e)]
     [else e])))

;;;;;;;;;;;;;;;;;;;;           env              ;;;;;;;;;;;;;;;;;;;

(define (init-env) '())

(define (apply-env env var)
  (cond
   [(assq var env) => cdr]
   [else var]))

(define (extend-env var val env)
  (cond
   [(null? var)
    env]
   [(pair? var)
    (extend-env (cdr var)
                (cdr val)
                (cons (cons (car var)
                            (car val))
                      env))]
   [else
    (cons (cons var val)
          env)]))

(define macro? procedure?)

(define identifier? symbol?)

;;;;;;;;;;;;;;;;;;;;;           expander      ;;;;;;;;;;;;;;;;;;;;;;;

(define (macroexpand e)
  (init-mark!)
  (expand e (init-env) #t))

(define (expand e env toplevel?)
  (cond
   [(symbol? e)
    (apply-env env e)]
   [(pair? e)
    (case (apply-env env e)
      [(if) (expand-if e env toplevel?)]
      [(set!) (expand-set! e env toplevel?)]
      [(begin) (expand-begin e env toplevel?)]
      [(lambda) (expand-lambda e env toplevel?)]
      [(quote) (expand-quote e env toplevel?)]
      [(define) (if toplevel?
                    (cond
                     ;; (define x e)
                     [(and (list? e)
                           (= (length e) 3)
                           (symbol? (cadr e)))
                      (expand `(set! ,(cadr e)
                                 ,(caddr e))
                              env)]
                     ;; (define (x ...) e ...)
                     [(and (list? e)
                           (> (length e) 2)
                           (pair? (cadr e))
                           (for-all* symbol? (cadr e)))
                      (expand `(set! ,(caadr e)
                                 (lambda ,(cdadr e)
                                   . ,(cddr e)))
                              env)]
                     [else
                      (compile-error "Invalid define" e)])
                    (compile-error "Misplaced define" e))])]
   [(or (boolean? e)
        (number? e)
        (string? e)
        (char? e))
    e]
   [else (compile-error "Invalid syntax" e)]))

(define (expand-if e env toplevel?)
  (if (and (list? e)
           (= (length e) 4))
      `(if ,(expand (cadr e) env toplevel?)
           ,(expand (caddr e) env toplevel?)
           ,(expand (cadddr e) env toplevel?))
      (compile-error "Invalid if" e)))

(define (expand-set! e env toplevel?)
  (if (and (list? e)
           (= (length e) 3)
           (identifier? (apply-env env (cadr e))))
      `(set! ,(cadr e)
         ,(expand (caddr e) env toplevel?))
      (compile-error "Invalid set!" e)))

(define (expand-begin e env toplevel?)
  (if (and (list? e)
           (> (length e) 1))
      `(begin . ,(map (lambda (e)
                        (expand e env toplevel?))
                      ,(cdr e)))
      (compile-error "Invalid begin" e)))

(define (expand-lambda e env toplevel?)
  (if (and (list? e)
           (> (length e) 2)
           (for-all* symbol? (cadr e)))
      (let* ([mark (new-mark)]
             [new-vars (map* (lambda (x)
                               (mark-identifier x mark))
                             (cadr e))]
             [env (extend-env (cadr e)
                              new-vars
                              env)])
        ;;;TODO: convert define & define-syntax
        `(lambda ,new-vars
           . ,(map (lambda (e)
                     (expand e env #f))
                   (cddr e))))
      (compile-error "Invalid lambda" e)))

(define (expand-quote e env toplevel?)
  (if (and (list? e)
           (= (length e) 2))
      `',(unmark (cadr e))
      (compile-error "Invalid quote" e)))
