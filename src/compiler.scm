
(load "define.scm")

;;;;;;;;;;;;;;;;;;;;    compile-time env  ;;;;;;;;;;;;;;;;;;;;;

(define-record-type variable-frame (fields next slist))

(define-record-type rename-frame (fields
                                  next
                                  name       ;; renamed
                                  denotation ;; a list-tail in a variable frame,
                                             ;; or a pair in macro frame / global env,
                                             ;; whose car is the original name
                                  ))

(define-record-type macro-frame (fields next alist))


(define (empty-cenv) #f)

(define (extend-cenv-var v* cenv)
  (letrec ([f (lambda (v*)
                (cond
                 [(null? v*) '()]
                 [(symbol? v*)
                  (cons v* '())]
                 [else
                  (cons (car v*)
                        (f (cdr v*)))]))])
    (make-variable-frame cenv (f v*))))

(define (extend-cenv-macro v* cenv)
  (letrec ([f (lambda (v*)
                (cond
                 [(null? v*) '()]
                 [(symbol? v*)
                  (cons (cons v* #f) '())]
                 [else
                  (cons (cons (car v*) #f)
                        (f (cdr v*)))]))])
    (make-macro-frame cenv (f v*))))

(define (extend-cenv-rename name den cenv)
  (make-rename-frame cenv name denv))


(define (get-denotation name)
  (let loop ([cenv cenv])
    (if (not cenv)
        (get-global name)
        (cond
         [(variable-frame? cenv)
          (let ([p (memq name variable-frame-slist)])
            (if p
                p
                (loop (variable-frame-next cenv))))]
         [(macro-frame? cenv)
          (let ([p (assq name variable-frame-alist)])
            (if p
                p
                (loop (macro-frame-next cenv))))]
         [(rename-frame? cenv)
          (if (eq? name (rename-frame-name cenv))
              (rename-frame-denotation cenv)
              (loop (rename-frame-next cenv)))]
         [else (error 'get-denotation "Unreachable")]))))

(define (is-macro? den)
  (macro? (cdr den)))

(define (special-form? name sym)
  (let ([den (get-denotation name)])
    (and (macro? (cdr den))
         (eq? sym (macro-handler (cdr den))))))


(define-record-type local-address (fields depth index))
(define-record-type global-address (fields index))

(define (get-variable-address den cenv)
  (let loop ([cenv cenv] [i 0])
    (if (not cenv)
        (make-global-address (get-global-index (car den)))
        (cond
         [(variable-frame? cenv)
          (let f ([slist (variable-frame-slist cenv)] [j 0])
            (cond
             [(null? slist)
              (loop (variable-frame-next cenv) (+ i 1))]
             [(eq? slist den)
              (make-local-address i j)]
             [else
              (f (cdr slist) (+ j 1))]))]
         [(macro-frame? cenv)
          (loop (macro-frame-next cenv) i)]
         [(rename-frame? cenv)
          (loop (rename-frame-next cenv) i)]
         [else (error 'get-variable-address "Unreachable")]))))



;;;;;;;;;;;;;;;;;;;;        compile        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (meaning-toplevel e*)
  (define (a-meaning-toplevel e tail?)
    (if (and (pair? e)
             (special-form? (car e) 'define))
      (meaning-define e tail?)
      (meaning e (init-cenv) tail?)))

  (if (null? e*)
    (instruction-encode 'return)
    (let rec ([e* e*])
      (if (null? (cdr e*))
        (append
          (a-meaning-toplevel (car e*) #t)
          (instruction-encode 'return))
        (let ([m1 (a-meaning-toplevel (car e*) #f)])
          (append m1 (rec (cdr e*))))))))

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
        (if (and (symbol? (car e))
                 (let ([addr (get-variable-address (car e) cenv)])
                   (and (global-address? addr)
                        (macro? (cdr (get-global
                                      (global-address-index addr)))))))
            (let ([mac (cdr (get-global
                             (global-address-index
                              (get-variable-address (car e) cenv))))])
              (if (builtin-special-form? mac)
                  (case (special-form-symbol mac)
                    [(quote)        (meaning-quote (cadr e) cenv tail?)]
                    [(if)           (meaning-if (cadr e) (cddr e) cenv tail?)]
                    [(set!)         (meaning-set (cadr e) (caddr e) cenv tail?)]
                    [(define)       (compile-error "misplaced define")]
                    [(lambda)       (meaning-lambda (cadr e) (cddr e) cenv tail?)]
                    [(begin)        (meaning-sequence (cdr e) cenv tail?)]
                    [(syntax-rules) (compile-error "misplaced syntax-rules")]
                    [else           (error 'meaning "Unreachable" e)])
                  (meaning (expand-macro mac e cenv) cenv tail?)))
            (if (list? e)
                (meaning-application e cenv tail?)
                (compile-error "Invalid application" e)))
        (compile-error "Invalid syntax" e))]))

(define (expand-macro mac e cenv)
  ((macro-handler mac) e cenv))

(define (meaning-quote c cenv tail?)
  (if (or (boolean? c)
          (number? c)
          (pair? c)
          (null? c)
          (vector? c)
          (string? c)
          (char? c)
          (symbol? c))
        (case c
          [(#f) (instruction-encode 'const/false)]
          [(#t) (instruction-encode 'const/true)]
          [(()) (instruction-encode 'const/null)]
          [(0)  (instruction-encode 'const/0)]
          [(1)  (instruction-encode 'const/1)]
          [else (instruction-encode 'const (get-constant-index c))])
        (compile-error "Invalid quotation" c)))

(define (meaning-if e1 e* cenv tail?)
  (let* ([m1 (meaning e1 cenv #f)]
         [m2 (meaning (car e*) cenv tail?)]
         [m3 (meaning (if (pair? (cdr e*))
                          (cadr e*)
                          #f)
                      cenv tail?)]
         [m2/goto (append m2 (gen-goto 'goto (length m3)))])
    (append m1 (gen-goto 'goto-if-false (length m2/goto))
            m2/goto
            m3)))

(define (meaning-define e tail?)
  (if (pair? (cadr e))
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
      (cond
        [(local-address? addr)
         (if (zero? (local-address-depth addr))
             (instruction-encode 'shallow-set
                                 (local-address-index addr))
             (instruction-encode 'deep-set
                                 (local-address-depth addr)
                                 (local-address-index addr)))]
        [(global-address? addr)
         (instruction-encode 'global-set
                             (global-address-index addr))]
        [else
         (error 'meaning-set "Unreachable")]))))

(define (meaning-reference name cenv tail?)
  (let ([addr (get-variable-address name cenv)])
    (cond
      [(local-address? addr)
       (if (zero? (local-address-depth addr))
           (instruction-encode 'shallow-ref
                               (local-address-index addr))
           (instruction-encode 'deep-ref
                               (local-address-depth addr)
                               (local-address-index addr)))]
      [(global-address? addr)
       (instruction-encode 'global-ref
                           (global-address-index addr))]
      [else
       (error 'meaing-reference "Unreachable")])))

(define (meaning-sequence e+ cenv tail?)
  (let loop ([e+ e+])
    (if (null? (cdr e+))
      (meaning (car e+) cenv tail?)
      (let ([m1 (meaning (car e+) cenv #f)])
        (append m1 (loop (cdr e+)))))))

(define (meaning-lambda args body cenv tail?)
  (let* ([m (append (meaning-lambda-body args body cenv #t)
                    (instruction-encode 'return))]
         [size (length m)])
    (append (instruction-encode 'closure
                                (modulo size 256)
                                (quotient size 256))
            m)))

(define (meaning-lambda-body args body cenv tail?)
  (let ([n (improper-list-length? args)])
    (append
      (if n
        (instruction-encode 'varfunc n)
        (instruction-encode 'func (length args)))
      (meaning-sequence
        body
        (extend-cenv-var args cenv)
        tail?))))

(define (meaning-application e+ cenv tail?)
  (let loop ([e* (cdr e+)])
    (if (null? e*)
      (if (and (pair? (car e+))
               (eq? 'lambda (caar e+)))
        (meaning-closed-application (car e+) (length (cdr e+)) cenv tail?)
        (append (meaning (car e+) cenv #f)
                (instruction-encode (if tail? 'tail-call 'call)
                                    (length (cdr e+)))))
      (append (meaning (car e*) cenv #f)
              (instruction-encode 'push)
              (loop (cdr e*))))))

(define (meaning-closed-application e argc cenv tail?)
  (append (instruction-encode 'extend-env argc)
          (meaning-lambda-body (cadr e) (cddr e) cenv tail?)
          (if tail?
            '()
            (instruction-encode 'shrink-env))))

;;;;;;;;;;;;;;;;;     macro


(define (make-macro-transformer-proc e def-env)
  (let ([literals (cadr e)]
        [rules (cddr e)])

    ;; mbe

    (define (match-pattern pattern e)
      (if (symbol? (car pattern))
        (let ([bindings (match (cdr pattern) (cdr e) '())])
          (if bindings
            (map (lambda (lv p)
                   (cons (car p)
                         (cons lv (cdr p))))
                 (get-levels (cdr pattern) 0 '())
                 bindings)
            #f))
        (compile-error "Invalid pattern" pattern)))

    (define (has-ellipsis? p)
      (and (pair? (cdr p))
           (eq? '... (cadr p))))

    (define (literal? s)
      (memq s literals))

    (define (get-levels p lv lv*)
      (cond
       [(symbol? p)
        (if (literal? p)
            lv*
            (cons lv lv*))]
       [(pair? p)
        (if (has-ellipsis? p)
            (get-levels (car p) (+ lv 1)
                        (get-levels (cddr p) lv lv*))
            (get-levels (car p) lv
                        (get-levels (cdr p) lv lv*)))]
       [(vector? p)
        (get-levels (vector->list p) lv lv*)]
       [else lv*]))

    (define (extract-names p names)
      (cond
       [(symbol? p)
        (if (literal? p)
            names
            (cons p names))]
       [(pair? p)
        (extract-names
         (car p)
         (extract-names
          ((if (has-ellipsis? p)
               cddr
               cdr)
           p)
          names))]
       [(vector? p)
        (extract-names (vector->list p) names)]
       [else names]))

    (define (match pattern e bindings)
      (cond
       [(symbol? pattern)
        (if (literal? pattern)
            (if (eq? pattern e)
			    bindings
				#f)
            (cons (cons pattern e)
                  bindings))]
       [(pair? pattern)
        (if (has-ellipsis? pattern)
            (let loop ([e e] [sub-bindings* '()])
              (let ([sub-bindings (if (pair? e)
                                      (match (car pattern) (car e) '())
                                      #f)])
                (if sub-bindings
                    (loop (cdr e)
                          (cons (map cdr sub-bindings)
                                sub-bindings*))
                    (let ([bindings1 (match (cddr pattern) e bindings)])
                      (if bindings1
                          (append (apply map list
                                         (extract-names (car pattern) '())
                                         (reverse sub-bindings*))
                                  bindings1)
                          #f)))))
            (if (pair? e)
                (let ([bindings1 (match (cdr pattern) (cdr e) bindings)])
                  (if bindings1
                      (match (car pattern) (car e) bindings1)
                      #f))
                #f))]
       [(vector? pattern)
        (if (vector? e)
            (match (vector->list pattern)
              (vector->list e)
              bindings)
            #f)]
       [else
        (if (equal? pattern e)
            bindings
            #f)]))

    (define (extract-template-fv tmpl fv*)
      (cond
       [(symbol? tmpl)
        (cons tmpl fv*)]
       [(pair? tmpl)
        (extract-template-fv
         (car tmpl)
         (extract-template-fv (cdr tmpl) fv*))]
       [(vector? tmpl)
        (extract-template-fv
         (vector->list tmpl)
         fv*)]
       [else fv*]))

    (define (instantiate tmpl b)
      (cond
       [(symbol? tmpl)
        (let ([x (assq tmpl b)])
          (if x
              (if (zero? (cadr x))
                  (cddr x)
                  (compile-error "Too few ..." tmpl))
              tmpl))]
       [(pair? tmpl)
        (if (has-ellipsis? tmpl)
            (let* ([fv* (extract-template-fv (car tmpl) '())]
                   [new-b (filter
                           (lambda (x)
                             (memq (car x) fv*))
                           b)])
              (let-values ([(new-b-lv0
                             new-b-lv+)
                            (partition
                             (lambda (x)
                               (zero? (cadr x)))
                             new-b)])
                (if (null? new-b-lv+)
                    (compile-error "Too many ..." tmpl)
                    (append
                     (map
                      (lambda (b)
                        (instantiate (car tmpl) b))
                      (decompose new-b-lv0 new-b-lv+))
                     (instantiate (cddr tmpl) b)))))
            (cons (instantiate (car tmpl) b)
                  (instantiate (cdr tmpl) b)))]
       [(vector? tmpl)
        (list->vector (f (vector->list tmpl) b))]
       [else tmpl]))

    (define (decompose b-lv0 b-lv+)
      (if (apply = (map (lambda (x) (length (cddr x))) b-lv+)) ;; equal lengths
          (let loop ([i (- (length (cddar b-lv+)) 1)]
                     [b* '()])
            (if (negative? i)
                b*
                (loop (- i 1)
                      (cons (append b-lv0
                                    (map
                                     (lambda (x)
                                       (cons (car x)
                                             (cons (- (cadr x) 1)
                                                   (list-ref (cddr x) i))))
                                     b-lv+))
                            b*))))
          (compile-error "Length unequal" tmpl)))


    (lambda (e use-env)
      (let loop ([rules rules])
        (if (null? rules)
            (compile-error "Invalid syntax" e)
            (let* ([pattern (caar rules)]
                   [template (cadar rules)]
                   [bindings (match-pattern pattern e)])
              (if bindings
                  (instantiate template bindings)
                  (loop (cdr rules)))))))))

;;;;;;;;;;;;;;;;;;  auxiliary functions

(define (gen-goto code offset)
  (if (> offset 65535)
    (compile-error "Too long jump" offset)
    (instruction-encode code
                        (modulo offset 256)
                        (quotient offset 256))))

;; if proper list, return #f
(define (improper-list-length? v*)
  (let loop ([v* v*] [i 0])
    (cond
     [(null? v*) #f]
     [(atom? v*) i]
     [else (loop (cdr v*) (+ i 1))])))


;;;;;;;;;;;;;;;;            macro                ;;;;;;;;;;;;;;;;;;;;;;

;; handler: procedure | symbol
(define-record-type macro (fields handler))

(define (add-macros!)
  (add-global! quote (make-macro 'quote))
  (add-global! if (make-macro 'if))
  (add-global! set! (make-macro 'set!))
  (add-global! define (make-macro 'define))
  (add-global! lambda (make-macro 'lambda))
  (add-global! begin (make-macro 'begin))
  (add-global! syntax-rules (make-macro 'syntax-rules))
  (add-global! let-syntax (make-macro 'let-syntax))
  (add-global! letrec-syntax (make-macro 'letrec-syntax))

  (add-global! define-syntax
               (make-macro
                (lambda (e use-env)
                  (if (special-form?))
                  (set-cdr!
                   (get-global (cadr e))
                   (make-macro
                    (macro-transformer-proc (meaning (caddr e) use-env #f))))
                  #f)))
  )
