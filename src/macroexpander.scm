
;;;;;;;;;;;;;;;;;;;;           denotation environment  ;;;;;;;;;;;;;;;;;;;

;; denotation: (name . value) or name

;; name: identifier
;; value: identifier or procedure

;; local variable denotation: (original-name . marked-name)
;; global variable:           name
;; macro denotation:          (name . macro-transformer)

(define *macros* #f)

(define (init-macroexpander!)
  (set! *macros* '()))

(define (add-macro! name transformer)
  (set! *macros*
    (cons (cons name transformer)
          *macros*)))

(define (init-denv) '())

(define (apply-denv env var)
  (cond
   [(assq var env) => cdr]
   [(assq var *macros*)]
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

(define (identifier? den)
  (or (symbol? den)
      (symbol? (cdr den))))

(define (macro? den)
  (and (pair? den)
       (procedure? (cdr den))))

(define (invoke-macro den e env)
  ((denotation-value den) e env))

(define (make-denotation name value)
  (cons name value))

(define (denotation-name den)
  (if (pair? den)
      (car den)
      den))

(define (denotation-value den)
  (if (pair? den)
      (cdr den)
      den))

(define (denotation-value-set! den v)
  (set-cdr! den v))

;;;;;;;;;;;;;;;;;;;;;           expander      ;;;;;;;;;;;;;;;;;;;;;;;

(define (expand e env toplevel?)
  (cond
   [(symbol? e)
    (let ([den (apply-denv env e)])
      (if (identifier? den)
          (denotation-value den)
          (compile-error "Misplaced syntax" e env)))]
   [(pair? e)
    (let ([den (apply-denv env (car e))])
      (case (denotation-value den)
        [(if)            (expand-if e env toplevel?)]
        [(set!)          (expand-set! e env toplevel?)]
        [(begin)         (expand-begin e env toplevel?)]
        [(lambda)        (expand-lambda e env toplevel?)]
        [(quote)         (expand-quote e env toplevel?)]
        [(define)        (expand (expand-define e env toplevel?)
                                 env toplevel?)]
        [(define-syntax) (expand-define-syntax e env toplevel?)]
        [(let-syntax)    (expand-let-syntax e env toplevel?)]
        [(letrec-syntax) (expand-letrec-syntax e env toplevel?)]
        [(syntax-rules)  (compile-error "Misplaced syntax-rules" e)]
        [else
         (if (macro? den)
             (let-values ([(e env)
                           (invoke-macro den e env)])
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
           (or (= (length e) 3)
               (= (length e) 4)))
      `(if . ,(map (lambda (e)
                     (expand e env toplevel?))
                   (cdr e)))
      (compile-error "Invalid if" e)))

(define (expand-set! e env toplevel?)
  (if (and (list? e)
           (= (length e) 3))
      (if (identifier? (apply-denv env (cadr e)))
          `(set! ,(denotation-value
                   (apply-denv env (cadr e)))
             ,(expand (caddr e) env toplevel?))
          (compile-error "Invalid variable in set!" e))
      (compile-error "Invalid set!" e)))

(define (verify-begin e)
  (unless (and (list? e)
               (> (length e) 1))
    (compile-error "Invalid begin" e)))

(define (expand-begin e env toplevel?)
  (verify-begin e)
  `(begin
     . ,(map (lambda (e)
               (expand e env toplevel?))
             (cdr e))))

(define (expand-define e env toplevel?)
  (if toplevel?
      (cond
       ;; (define x e)
       [(and (list? e)
             (= (length e) 3)
             (symbol? (cadr e)))
        `(set! ,(cadr e)
           ,(caddr e))]
       ;; (define (x ...) e ...)
       [(and (list? e)
             (> (length e) 2)
             (pair? (cadr e))
             (for-all* symbol? (cadr e)))
        `(set! ,(caadr e)
           (lambda ,(cdadr e)
             . ,(cddr e)))]
       [else
        (compile-error "Invalid define" e)])
      (compile-error "Misplaced define" e)))

(define (expand-lambda e env toplevel?)
  (if (and (list? e)
           (> (length e) 2)
           (for-all* symbol? (cadr e)))
      (let* ([mark (new-mark)]
             [vars (cadr e)]
             [new-vars (map1* (lambda (x)
                                (mark-identifier
                                 ;; macro expansion may introduce marked formal arguments,
                                 ;; unmark them first
                                 (unmark x env)
                                 mark))
                              vars)]
             [env (extend-denv vars
                               (map* make-denotation vars new-vars)
                               env)])
        (let loop ([body     (cddr e)]
                   [defs     '()]
                   [stxs     '()])
          (if (null? body)
              (compile-error "Invalid lambda body" e)
              (let ([e1 (car body)])
                (cond
                 [(and (pair? e1)
                       (memq (denotation-value (apply-denv env (car e1)))
                             '(begin define define-syntax)))
                  =>
                  (lambda (p)
                    (case (car p)
                      [(begin)
                       (verify-begin e1)
                       (loop (append (cdr e1) (cdr body))
                             defs
                             stxs)]
                      [(define)
                       (loop (cdr body)
                             (cons (cdr
                                    ;; convert define to set! form
                                    (expand-define e1 env #t))
                                   defs)
                             stxs)]
                      [(define-syntax)
                       (verify-define-syntax e1)
                       (loop (cdr body)
                             defs
                             (cons (cdr e1) stxs))]))]
                 [else
                  (if (null? defs)
                      `(lambda ,new-vars
                         ,(expand
                           `(letrec-syntax ,stxs
                              . ,body)
                           env #f))
                      `(lambda ,new-vars
                         ,(expand
                           `(letrec ,defs
                              (letrec-syntax ,stxs
                                . ,body))
                           env #f)))])))))
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

(define (verify-define-syntax e)
  (unless (and (list? e)
               (= (length e) 3)
               (symbol? (cadr e)))
    (compile-error "Invalid define-syntax" e)))

(define (expand-define-syntax e env toplevel?)
  (if toplevel?
      (begin
        (verify-define-syntax e)
        (add-macro!
         (cadr e)
         (make-macro-transformer (caddr e) env))
        #f)
      (compile-error "Misplaced define-syntax" e)))

(define (expand-let-syntax e env toplevel?)
  (if (and (list? e)
           (> (length e) 2)
           (list? (cadr e))
           (for-all (lambda (e)
                      (and (list? e)
                           (= (length e) 2)
                           (symbol? (car e))))
                    (cadr e)))
      (expand `(begin
                 . ,(cddr e))
              (extend-denv (map car (cadr e))
                           (map (lambda (e)
                                  (make-denotation
                                   (car e)
                                   (make-macro-transformer (cadr e) env)))
                                (cadr e))
                           env)
              toplevel?)
      (compile-error "Invalid let-syntax" e)))

(define (expand-letrec-syntax e env toplevel?)
  (if (and (list? e)
           (> (length e) 2)
           (list? (cadr e))
           (for-all (lambda (e)
                      (and (list? e)
                           (= (length e) 2)
                           (symbol? (car e))))
                    (cadr e)))
      (let* ([vars (map car (cadr e))]
             [env (extend-denv vars
                               (map (lambda (e)
                                      (make-denotation e #f))
                                    vars)
                               env)])
        (for-each (lambda (e)
                    (denotation-value-set!
                     (apply-denv env (car e))
                     (make-macro-transformer (cadr e) env)))
                  (cadr e))
        (expand `(begin
                   . ,(cddr e))
                env
                toplevel?))
      (compile-error "Invalid letrec-syntax" e)))

(define (make-macro-transformer e def-env)
  (if (and (list? e)
           (> (length e) 1)
           (eq? 'syntax-rules
                (denotation-value
                 (apply-denv def-env (car e))))
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
      (if (eqv? p e)
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
                      (let ([new-var (mark-identifier tmpl mark)])
                        (set! use-env
                          (extend-denv new-var
                                       (apply-denv def-env tmpl)
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

(define (unmark e env)
  (let rec ([e e])
    (cond
     [(symbol? e)
      (denotation-name
       (apply-denv env e))]
     [(pair? e)
      (cons (rec (car e))
            (rec (cdr e)))]
     [(vector? e)
      (vector-map rec e)]
     [else e])))

;;;;;;;;;;;;;;;;;;;;;;;;        helper functions        ;;;;;;;;;;;;;;;;;;;;

;; works for both lists and improper lists
(define (for-all* p ls)
  (cond
   [(null? ls) #t]
   [(pair? ls) (and (p (car ls))
                    (for-all* p (cdr ls)))]
   [else (p ls)]))

(define (dotted->list ls)
  (cond
   [(null? ls) '()]
   [(pair? ls) (cons (car ls)
                     (dotted->list (cdr ls)))]
   [else       (cons ls '())]))

;; works for both lists and improper lists
(define (map1* f ls)
  (cond
   [(null? ls) '()]
   [(pair? ls) (cons (f (car ls))
                     (map1* f (cdr ls)))]
   [else       (f ls)]))

(define (map* f . lss)
  (cond
   [(null? (car lss))
    '()]
   [(pair? (car lss))
    (cons (apply f (map car lss))
          (apply map* f (map cdr lss)))]
   [else
    (apply f lss)]))
