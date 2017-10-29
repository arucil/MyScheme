
(load "define.scm")
(load "macroexpander.scm")

;;;;;;;;;;;;;;;;;;;;        compile        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (meaning-toplevel e*)
  (if (null? e*)
    (instruction-encode 'return)
    (let rec ([e* e*])
      (let ([e1 (macroexpand (car e*))])
        (if (and (pair? e1)
                 (eq? 'begin (car e1)))
            (rec (append (cdr e1) (cdr e*)))
            (if (null? (cdr e*))
                (append (meaning-expanded e1 (empty-cenv) #t)
                        (instruction-encode 'return))
                (let ([m1 (meaning-expanded e1 (empty-cenv) #f)])
                  (append m1 (rec (cdr e*))))))))))

(define (meaning-expanded e cenv tail?)
  (cond
   [(or (boolean? e)
        (number? e)
        (string? e)
        (char? e))
    (meaning-quote e cenv tail?)]
   [(symbol? e)
    (meaning-reference e cenv tail?)]
   [else
    (case (car e)
      [(quote)  (meaning-quote       (cadr e) cenv tail?)]
      [(if)     (meaning-if          (cadr e) (cddr e) cenv tail?)]
      [(set!)   (meaning-set         (cadr e) (caddr e) cenv tail?)]
      [(lambda) (meaning-lambda      (cadr e) (cddr e) cenv tail?)]
      [(begin)  (meaning-sequence    (cdr e) cenv tail?)]
      [else     (meaning-application e cenv tail?)])]))

(define (meaning-quote c cenv tail?)
  (case c
    [(#f) (instruction-encode 'const/false)]
    [(#t) (instruction-encode 'const/true)]
    [(()) (instruction-encode 'const/null)]
    [(0)  (instruction-encode 'const/0)]
    [(1)  (instruction-encode 'const/1)]
    [else (instruction-encode 'const (get-constant-index c))]))

(define (meaning-if e1 e* cenv tail?)
  (let* ([m1 (meaning-expanded e1 cenv #f)]
         [m2 (meaning-expanded (car e*) cenv tail?)]
         [m3 (meaning-expanded (if (pair? (cdr e*))
                                   (cadr e*)
                                   #f)
                               cenv tail?)]
         [m2/goto (append m2 (gen-goto 'goto (length m3)))])
    (append m1 (gen-goto 'goto-if-false (length m2/goto))
            m2/goto
            m3)))

(define (meaning-set name e cenv tail?)
  (append
   (meaning-expanded e cenv #f)
   (let ([addr (get-address name cenv)])
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
       (error 'meaning-set "Unreachable" name e)]))))

(define (meaning-reference name cenv tail?)
  (let ([addr (get-address name cenv)])
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
      (error 'meaing-reference "Unreachable" name)])))

(define (meaning-sequence e+ cenv tail?)
  (let loop ([e+ e+])
    (if (null? (cdr e+))
      (meaning-expanded (car e+) cenv tail?)
      (let ([m1 (meaning-expanded (car e+) cenv #f)])
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
  (let ([n (improper-list-length args)]
        [cenv (extend-cenv args cenv)])
    (append
      (if n
        (instruction-encode 'varfunc n)
        (instruction-encode 'func (length args)))
      (meaning-sequence body cenv tail?))))

(define (meaning-application e+ cenv tail?)
  (let loop ([e* (cdr e+)])
    (if (null? e*)
      (if (and (pair? (car e+))
               (eq? 'lambda (caar e+)))
        (meaning-closed-application (car e+) (length (cdr e+)) cenv tail?)
        (append (meaning-expanded (car e+) cenv #f)
                (instruction-encode (if tail? 'tail-call 'call)
                                    (length (cdr e+)))))
      (append (meaning-expanded (car e*) cenv #f)
              (instruction-encode 'push)
              (loop (cdr e*))))))

(define (meaning-closed-application e argc cenv tail?)
  (append (instruction-encode 'extend-env argc)
          (meaning-lambda-body (cadr e) (cddr e) cenv tail?)
          (if tail?
            '()
            (instruction-encode 'shrink-env))))

;;;;;;;;;;;;;;;;;;  auxiliary functions

(define (gen-goto code offset)
  (if (> offset 65535)
    (compile-error "Too long jump" offset)
    (instruction-encode code
                        (modulo offset 256)
                        (quotient offset 256))))

;; if proper list, return #f
(define (improper-list-length v*)
  (let loop ([v* v*] [i 0])
    (cond
     [(null? v*) #f]
     [(atom? v*) i]
     [else (loop (cdr v*) (+ i 1))])))

;;;;;;;;;;;;;;;;;;;;    compile-time env  ;;;;;;;;;;;;;;;;;;;;;

(define (empty-cenv) '())

(define (extend-cenv v* cenv)
  (cons v* cenv))


(define-record-type local-address (fields depth index))
(define-record-type global-address (fields index))

(define (get-address name cenv)
  (let loop ([cenv cenv] [i 0])
    (if (null? cenv)
        (make-global-address
         (get-global-index name))
        (let f ([ls (car cenv)] [j 0])
          (cond
           [(null? ls)
            (loop (cdr cenv) (+ i 1))]
           [(symbol? ls)
            (if (eq? ls name)
                (make-local-address i j)
                (loop (cdr cenv)
                      (+ i 1)))]
           [(eq? (car ls) name)
            (make-local-address i j)]
           [else
            (f (cdr ls) (+ j 1))])))))


;;;;;;;;;;;;;;;;;;       miscellanenous     ;;;;;;;;;;;;;;;;;;;

(define (initialize!)
  (init-constants!)
  (init-globals!)
  (reset-exit-flag!)
  (init-macroexpander!))
