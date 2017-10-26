
;;;;;;;;;;;;;;;;;;;;;           expander      ;;;;;;;;;;;;;;;;;;;;;;;

(define (expand e env toplevel?)
  (cond
   [(symbol? e)
    (let ([den (apply-denv env e)])
      (if (symbol? den)
          den
          (compile-error "Misplaced syntax" e)))]
   [(pair? e)
    (let ([den (apply-denv env (car e))])
      (case den
        [(if)
         (expand-if e env toplevel?)]
        [(set!)
         (expand-set! e env toplevel?)]
        [(begin)
         (expand-begin e env toplevel?)]
        [(lambda)
         (expand-lambda e env toplevel?)]
        [(quote)
         (expand-quote e env toplevel?)]
        [(define)
         (expand-define e env toplevel?)]
        [(define-syntax)
         (expand-define-syntax e env toplevel?)]
        [(syntax-rules)
         (compile-error "Misplaced syntax-rules" e)]
        [else
         (if (macro? den)
             (let-values ([(e env)
                           (den e env)])
               (pretty-print e)
               (expand e env toplevel?))
             (if (list? e)
                 (map (lambda (e)
                        (expand e env toplevel?))
                      e)
                 (compile-error "Invalid application" e)))]))]
   [(or (boolean? e)
        (number? e)
        (string? e)
        (char? e))
    e]
   [else
    (compile-error "Invalid syntax" e)]))

(define (macroexpand e)
  (init-mark!)
  (expand e (init-denv) #t))

(define (expand-if e env toplevel?)
  (if (and (list? e)
           (= (length e) 4))
      `(if ,(expand (cadr e) env toplevel?)
           ,(expand (caddr e) env toplevel?)
           ,(expand (cadddr e) env toplevel?))
      (compile-error "Invalid if" e)))

(define (expand-set! e env toplevel?)
  (if (and (list? e)
           (= (length e) 3))
      (if (symbol? (apply-denv env (cadr e)))
          `(set! ,(apply-denv env (cadr e))
             ,(expand (caddr e) env toplevel?))
          (compile-error "Invalid variable in set!" e))
      (compile-error "Invalid set!" e)))

(define (expand-begin e env toplevel?)
  (if (and (list? e)
           (> (length e) 1))
      `(begin
         . ,(map (lambda (e)
                   (expand e env toplevel?))
                 (cdr e)))
      (compile-error "Invalid begin" e)))

(define (expand-define e env toplevel?)
  (if toplevel?
      (cond
       ;; (define x e)
       [(and (list? e)
             (= (length e) 3)
             (symbol? (cadr e)))
        (expand `(set! ,(cadr e)
                   ,(caddr e))
                env
                toplevel?)]
       ;; (define (x ...) e ...)
       [(and (list? e)
             (> (length e) 2)
             (pair? (cadr e))
             (for-all* symbol? (cadr e)))
        (expand `(set! ,(caadr e)
                   (lambda ,(cdadr e)
                     . ,(cddr e)))
                env
                toplevel?)]
       [else
        (compile-error "Invalid define" e)])
      (compile-error "Misplaced define" e)))

(define (expand-lambda e env toplevel?)
  (if (and (list? e)
           (> (length e) 2)
           (for-all* symbol? (cadr e)))
      (let* ([mark (new-mark)]
             [new-vars (map1* (lambda (x)
                               (mark-identifier
                                ;; macro expansion may introduce marked formal arguments,
                                ;; unmark them first
                                (unmark x env)
                                mark))
                             (cadr e))]
             [env (extend-denv (cadr e)
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
           (= (length e) 2)
           (let ([c (cadr e)])
             (or (boolean? c)
                 (number? c)
                 (pair? c)
                 (null? c)
                 (vector? c)
                 (string? c)
                 (char? c)
                 (symbol? c))))
      `',(unmark (cadr e) env)
      (compile-error "Invalid quote" e)))

(define (expand-define-syntax e env toplevel?)
  (if toplevel?
      (if (and (list? e)
               (= (length e) 3)
               (symbol? (cadr e)))
          (begin
            (add-macro! (cadr e)
                        (make-macro-transformer (caddr e) env))
            #f)
          (compile-error "Invalid define-syntax" e))
      (compile-error "Misplaced define-syntax" e)))

(define (make-macro-transformer e def-env)
  (if (and (list? e)
           (> (length e) 1)
           (eq? 'syntax-rules
                (apply-denv def-env (car e)))
           (list? (cadr e))
           (for-all symbol? (cadr e))
           (for-all (lambda (rule)
                      (and (list? rule)
                           (= (length rule) 2)
                           (pair? (car rule))
                           (symbol? (caar rule))))
                    (cddr e)))
      (let ([literals (cadr e)]
            [rules    (cddr e)])
        (lambda (e use-env)
          (let loop ([rules rules])
            (if (null? rules)
                (compile-error "Invalid macro" e)
                (let* ([pattern (caar rules)]
                       [template (cadar rules)]
                       [bindings (match e pattern literals def-env use-env)])
                  (if bindings
                      (transcribe (map (lambda (p lv)
                                         (cons (car p)
                                               (cons lv
                                                     (cdr p))))
                                       bindings
                                       (get-levels pattern literals))
                                  template
                                  literals
                                  def-env
                                  use-env)
                      (loop (cdr rules))))))))
      (compile-error "Invalid macro transformer" e)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (match e p literals def-env use-env)
  (let rec ([e (cdr e)]
            [p (cdr p)]
            [b '()])
    (cond
     [(symbol? p)
      (if (memq p literals)
          ;; literal should have the same denotation
          (if (eq? (apply-denv def-env p)
                   (apply-denv use-env e))
              b
              #f)
          (cons (cons p e) b))]
     [(vector? p)
      (if (vector? e)
          (rec (vector->list e)
               (vector->list p)
               b)
          #f)]
     [(pair? p)
      (if (has-ellipsis? p)
          (let loop ([e e] [sb* '()])
            (let ([sb (if (pair? e)
                          (rec (car e) (car p) '())
                          #f)])
              (if sb
                  (loop (cdr e)
                        (cons (map cdr sb)
                              sb*))
                  (let ([b1 (rec e (cddr p) b)])
                    (if b1
                        (append (apply map list
                                       (get-pattern-variables (car p) literals)
                                       (reverse sb*))
                                b1)
                        #f)))))
          (if (pair? e)
              (let ([b1 (rec (cdr e)
                             (cdr p)
                             b)])
                (if b1
                    (rec (car e)
                         (car p)
                         b1)
                    #f))
              #f))]
     [else
      (if (equal? p e)
          b
          #f)])))

(define (has-ellipsis? p)
  (and (pair? (cdr p))
       (eq? '... (cadr p))))

(define (get-pattern-variables p literals)
  (let rec ([p p] [names '()])
    (cond
     [(symbol? p)
      (if (memq p literals)
          names
          (cons p names))]
     [(pair? p)
      (rec (car p)
           (rec
            ((if (has-ellipsis? p)
                 cddr
                 cdr)
             p)
            names))]
     [(vector? p)
      (rec (vector->list p) names)]
     [else names])))

(define (get-levels p literals)
  (let rec ([p (cdr p)] [lv 0] [lv* '()])
    (cond
     [(symbol? p)
      (if (memq p literals)
          lv*
          (cons lv lv*))]
     [(vector? p)
      (rec (vector->list p) lv lv*)]
     [(pair? p)
      (if (has-ellipsis? p)
          (rec (car p)
               (+ lv 1)
               (rec (cddr p)
                    lv
                    lv*))
          (rec (car p)
               lv
               (rec (cdr p)
                    lv
                    lv*)))]
     [else lv*])))

(define (transcribe bindings template literals def-env use-env)
  (letrec ([rec (lambda (tmpl b/lv0 b/lv+)
                  (cond
                   [(symbol? tmpl)
                    (cond
                     [(assq tmpl b/lv0) => cddr]
                     [(assq tmpl b/lv+)
                      (compile-error "Too few ellipses with" tmpl)]
                     [else
                      (let ([new-var (mark-identifier tmpl mark)]
                            [old-den (apply-denv def-env tmpl)])
                        (set! use-env
                          (extend-denv new-var
                                       (if (macro? old-den)
                                           tmpl
                                           old-den)
                                       use-env))
                        new-var)])]
                   [(vector? tmpl)
                    (list->vector
                     (rec (vector->list tmpl)
                          b/lv0
                          b/lv+))]
                   [(pair? tmpl)
                    (if (has-ellipsis? tmpl)
                        (let* ([free-variables (get-pattern-variables (car tmpl) literals)]
                               [new-b/lv+ (filter (lambda (x)
                                                    (memq (car x) free-variables))
                                                  b/lv+)])
                          (if (null? new-b/lv+)
                              (compile-error "Too many ellipses")
                              (let ([names  (map car new-b/lv+)]
                                    [levels (map (lambda (x)
                                                   (- (cadr x) 1))
                                                 new-b/lv+)]
                                    [sexpss (map cddr new-b/lv+)])
                                (if (apply = (map length sexpss))
                                    (let ([b* (apply map
                                                     (lambda sexps
                                                       (map (lambda (name level sexp)
                                                              (cons name
                                                                    (cons level sexp)))
                                                            names levels sexps))
                                                     sexpss)])
                                      (append (map (lambda (b)
                                                     (let-values ([(new-b/lv0 b/lv+)
                                                                   (split-bindings b)])
                                                       (rec (car tmpl)
                                                            (append new-b/lv0 b/lv0)
                                                            b/lv+)))
                                                   b*)
                                              (rec (cddr tmpl) b/lv0 b/lv+)))
                                    (compile-error "Unequal length")))))
                        (cons (rec (car tmpl) b/lv0 b/lv+)
                              (rec (cdr tmpl) b/lv0 b/lv+)))]
                   [else tmpl]))]
           [mark (new-mark)])
    (let-values ([(b/lv0 b/lv+)
                  (split-bindings bindings)])
      (let ([e (rec template b/lv0 b/lv+)])
        (values e use-env)))))

;; return bindings/lv0 and bindings/lv+ (level above 0)
(define (split-bindings b)
  (partition (lambda (p)
               (zero? (cadr p)))
             b))

;;;;;;;;;;;;;;;;;;;;;      global macros         ;;;;;;;;;;;;;;;;;;;;;

(define *macros* #f)


(define (init-macros!)
  (set! *macros* '()))

(define (add-macro! name x)
  (set! *macros*
    (cons (cons name x)
          *macros*)))

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
          (apply-denv env e)
          e)]
     [(pair? e)
      (cons (rec (car e))
            (rec (cdr e)))]
     [(vector? e)
      (vector-map rec e)]
     [else e])))

;;;;;;;;;;;;;;;;;;;;           denotation environment  ;;;;;;;;;;;;;;;;;;;

(define (init-denv) '())

(define (apply-denv env var)
  (cond
   [(assq var env) => cdr]
   [(assq var *macros*) => cdr]
   [else var]))

(define (extend-denv var val env)
  (cond
   [(null? var)
    env]
   [(pair? var)
    (extend-denv (cdr var)
                (cdr val)
                (cons (cons (car var)
                            (car val))
                      env))]
   [else
    (cons (cons var val)
          env)]))

(define macro? procedure?)


;;;;;;;;;;;;;;;;;;;;;;;;        helper functions        ;;;;;;;;;;;;;;;;;;;;

;; works for both lists and improper lists
(define (for-all* p ls)
  (cond
   [(null? ls) #t]
   [(pair? ls) (and (p (car ls))
                    (for-all* p (cdr ls)))]
   [else (p ls)]))

;; works for both lists and improper lists
(define (map1* f ls)
  (cond
   [(null? ls) '()]
   [(pair? ls) (cons (f (car ls))
                     (map1* f (cdr ls)))]
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
