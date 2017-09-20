
(load "define.scm")

;;;;;;;;;;;;;;;;;;;;        compile        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (meaning-toplevel e*)
  (define (a-meaning-toplevel e tail?)
    (if (and (pair? e) (eq? (car e) 'define))
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
   [(macro-transformer? e)
    e]
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
                    [(quote)      (meaning-quote (cadr e) cenv tail?)]
                    [(if)         (meaning-if (cadr e) (cddr e) cenv tail?)]
                    [(set!)       (meaning-set (cadr e) (caddr e) cenv tail?)]
                    [(define)     (compile-error "define only allowed at top level")]
                    [(lambda)     (meaning-lambda (cadr e) (cddr e) cenv tail?)]
                    [(begin)      (meaning-sequence (cdr e) cenv tail?)]
					[else         (error 'meaning "Unreachable" e)]
					)
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
  (let* ([m (append (gen-lambda-body args body cenv #t)
                    (instruction-encode 'return))]
         [size (length m)])
    (append (instruction-encode 'closure
                                (modulo size 256)
                                (quotient size 256))
            m)))

(define (gen-lambda-body args body cenv tail?)
  (let ([n (variadic? args)])
    (append 
      (if n
        (instruction-encode 'varfunc n)
        (instruction-encode 'func (length args)))
      (meaning-sequence
        body
        (extend-cenv args cenv)
        tail?))))

(define (meaning-application e+ cenv tail?)
  (let loop ([e* (cdr e+)])
    (if (null? e*)
      (if (and (pair? (car e+))
               (eq? 'lambda (caar e+)))
        (gen-closed-application (car e+) (length (cdr e+)) cenv tail?)
        (append (meaning (car e+) cenv #f)
                (instruction-encode (if tail? 'tail-call 'call)
                                    (length (cdr e+)))))
      (append (meaning (car e*) cenv #f)
              (instruction-encode 'push)
              (loop (cdr e*))))))

(define (gen-closed-application e argc cenv tail?)
  (append (instruction-encode 'extend-env argc)
          (gen-lambda-body (cadr e) (cddr e) cenv tail?)
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

(define-record-type local-address (fields depth index))
(define-record-type global-address (fields index))

(define (get-variable-address name cenv)
  (let loop ([cenv cenv] [i 0])
    (if (null? cenv)
        (make-global-address (get-global-index name))
       (let loop2 ([rib (car cenv)] [j 0])
         (cond
           [(null? rib)
            (loop (cdr cenv) (+ i 1))]
           [(eq? (car rib) name)
            (make-local-address i j)]
           [else
            (loop2 (cdr rib) (+ j 1))])))))


(define (init-cenv) '())

(define (extend-cenv v* cenv)
  (letrec ([f (lambda (v*)
                (cond
                  [(null? v*) '()]
                  [(symbol? v*) (cons v* '())]
                  [else
                    (cons (car v*) (f (cdr v*)))]))])
    (cons (f v*) cenv)))

(define (variadic? v*)
  (let loop ([v* v*] [i 0])
    (cond
     [(null? v*) #f]
     [(atom? v*) i]
     [else (loop (cdr v*) (+ i 1))])))
