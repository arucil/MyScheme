
;;;;;;;;;;;;;;;;;;           miscellaneous            ;;;;;;;;;;;;;;;;;;

(define error-tag "Error")

(define (runtime-error . args)
  (*exit* args))

(define (compile-error . args)
  (*exit* args))

(define (initialize!)
  (init-constants!)
  (init-globals!))

;;;;;;;;;;;;;;;;;;             constants         ;;;;;;;;;;;;;;;;;;;;;

(define *constants*)

(define (init-constants!)
  (set! *constants* '()))

(define (get-constant-index c)
  (let loop ([i 0]
             [ls *constants*])
    (cond
      [(null? ls)
       (set! *constants* (append *constants* (list c)))
       i]
      [(equal? (car ls) c) i]
      [else (loop (+ i 1) (cdr ls))])))

(define (get-constant i)
  (list-ref *constants* i))

;;;;;;;;;;;;;;;;        stack             ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *stack*)

(define (empty-stack!)
  (set! *stack* '()))

(define (init-stack!)
  (empty-stack!)
  (stack-push! #f) ; dummy env
  (stack-push! (instruction-encode 'exit))) ; pc for exiting program

(define (stack-push! val)
  (set! *stack* (cons val *stack*)))

(define (stack-pop!)
  (let ([val (car *stack*)])
    (set! *stack* (cdr *stack*))
    val))

;;;;;;;;;;;;;;;;;       activation record       ;;;;;;;;;;;;;;;;;;;;;;;

;;  [next ]  ->  [next ]  ->  ...  -> #f
;;  [frame]      [frame]
;;     |             \
;;     v              (...)
;;   (local0 local1 ...)

(define-record-type activation-record (fields next frame))

(define (local-reference i j env)
  (let loop ([i i] [env env])
    (if (zero? i)
      (list-ref (activation-record-frame env) j)
      (loop (- i 1)
            (activation-record-next env)))))

(define (local-assign i j v env)
  (let loop ([i i] [env env])
    (if (zero? i)
      (set-car! (list-tail (activation-record-frame env) j) v)
      (loop (- i 1)
            (activation-record-next env)))))

(define (extend-env n env)
  (define (make-list n ls)
    (if (zero? n)
      ls
      (make-list (- n 1) (cons #f ls))))

  (make-activation-record
    env
    (make-list (+ n 1) '()))) ; leave 1 slot for variadic functions

(define (shrink-env env)
  (activation-record-next env))

(define (activation-frame-size env)
  (length (activation-record-frame env)))

;;;;;;;;;;;;;;;;           globals              ;;;;;;;;;;;;;;;;;;;;;;;;

;;  ( (name . value)
;;    ...
;;  )

(define *globals*)

(define (empty-globals!)
  (set! *globals* '()))

(define (init-globals!)
  (empty-globals!)
  (add-primitives!))

(define undefined-tag "undefined")

(define (global-reference i)
  (let ([v (list-ref *globals* i)])
    (if (eq? undefined-tag (cdr v))
      (runtime-error "Undefined global variable" (car v))
      (cdr v))))

(define (global-assign i v)
  (set-cdr! (list-ref *globals* i) v))

(define (get-global-index name)
  (let loop ([g *globals*] [i 0])
    (cond
      [(null? g)
       (set! *globals* (append *globals* (list (cons name undefined-tag))))
       i]
      [(eq? (caar g) name)
       i]
      [else (loop (cdr g) (+ i 1))])))

(define (get-global i)
  (list-ref *globals* i))

;;;;;;;;;;;;;;;;;          registers            ;;;;;;;;;;;;;;;;;;;;;;;;

(define *acc*)

(define *env*)

(define *pc*)

(define *exit*)

;;;;;;;;;;;;;;;;;         closure          ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type closure (fields env code))

;;;;;;;;;;;;;;;;;         primitive          ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type primitive (fields func))

;;;;;;;;;;;;;;;;;            instruction set       ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-instruction-set
  (syntax-rules (define-instruction)
    [(_ (define-instruction (instruction arg ...) code . body) ...)
     (begin
       (set! instruction-encode
         (lambda (ins . params)
           (case ins
             [(instruction) (cons code params)]
             ...)))

       (set! instruction-decode
         (lambda (b)
           (case b
             [(code) 'instruction]
             ...)))

       (set! instruction-size
         (lambda (c)
           (case c
             [(code) (+ 1 (length '(arg ...)))]
             ...
             )))

       (set! run-instruction
         (lambda ()
           (case (fetch-byte)
             [(code)
              (let* ([arg (fetch-byte)] ...)
                . body)]
             ...))))]))

(define (fetch-byte)
  (let ([b (car *pc*)])
    (set! *pc* (cdr *pc*))
    b))

(define instruction-encode)
(define instruction-decode)
(define instruction-size)
(define run-instruction)

(define-instruction-set

  ;; constant

  (define-instruction (const/true) 1
    (set! *acc* #t))
  (define-instruction (const/false) 2
    (set! *acc* #f))
  (define-instruction (const/null) 3
    (set! *acc* '()))
  (define-instruction (const/0) 4
    (set! *acc* 0))
  (define-instruction (const/1) 5
    (set! *acc* 1))
  (define-instruction (const i) 6
    (set! *acc* (get-constant i)))

  ;; reference

  (define-instruction (shallow-ref i) 10
    (set! *acc* (local-reference 0 i *env*)))
  (define-instruction (deep-ref i j) 11
    (set! *acc* (local-reference i j *env*)))
  (define-instruction (global-ref i) 12
    (set! *acc* (global-reference i)))

  ;; assignment

  (define-instruction (shallow-set i) 13
    (local-assign 0 i *acc* *env*))
  (define-instruction (deep-set i j) 14
    (local-assign i j *acc* *env*))
  (define-instruction (global-set i) 15
    (global-assign i *acc*))

  ;; jump

  (define-instruction (goto offset1 offset2) 20
    (common-goto offset1 offset2))

  (define-instruction (goto-if-false offset1 offset2) 21
    (unless *acc*
      (common-goto offset1 offset2)))

  (define-instruction (goto-if-true offset1 offset2) 22
    (when *acc*
      (common-goto offset1 offset2)))

  ;; closure

  (define-instruction (closure offset1 offset2) 25
    (set! *acc* (make-closure *env* *pc*))
    (common-goto offset1 offset2))

  (define-instruction (func arity) 26     ; regular function
    (unless (= (- (activation-frame-size *env*) 1)
               arity)
      (runtime-error "Incorrect arity" arity size)))

  (define-instruction (varfunc arity) 27  ; variadic function
    (let ([size (- (activation-frame-size *env*) 1)])
      (if (>= size arity)
          (do ([i (- size 1) (- i 1)]
               [ls '() (cons (local-reference 0 i *env*) ls)])
              [(< i arity)
               (local-assign 0 arity ls *env*)])
          (runtime-error "Incorrect arity for variadic procedure" arity size ))))

  ;; application

  (define-instruction (exit) 30
    (*exit* *acc*))

  (define-instruction (return) 31
    (return))

  (define-instruction (push) 32 ; save argument
    (stack-push! *acc*))

  (define-instruction (extend-env n) 33 ; for closed applications
    (set! *env* (store-fixed-arguments
                  n
                  (extend-env n *env*))))

  (define-instruction (shrink-env) 34 ; for closed applications
    (set! *env* (shrink-env *env*)))

  (define-instruction (call n) 35
    (common-call n #f))

  (define-instruction (tail-call n) 36
    (common-call n #t))
  )

;; auxiliary functions

(define (common-goto offset1 offset2)
  (set! *pc* (list-tail *pc* (+ offset1 (* 256 offset2)))))

(define (common-call n tail?)
  (cond
    [(closure? *acc*)
     (let ([env (store-fixed-arguments
                  n
                  (extend-env n (closure-env *acc*)))])

       ;; save caller's context
       (unless tail?
         (stack-push! *env*)
         (stack-push! *pc*))

       (set! *pc* (closure-code *acc*))
       (set! *env* env))]
    [(primitive? *acc*)
     (let loop ([n n]
                [ls '()])
       (if (zero? n)
           (begin
             (unless tail?
               (stack-push! *env*)
               (stack-push! *pc*))
             (set! *acc*
               (apply (primitive-func *acc*)
                      ls))
             (return))
           (loop (- n 1)
                 (cons (stack-pop!) ls))))]
    [else
     (runtime-error "Attempt to apply non-procedure" *acc*)]))

(define (store-fixed-arguments n env)
  (do ([i (- n 1) (- i 1)])
      [(negative? i)
       env]
    (local-assign 0 i (stack-pop!) env)))

(define (return)
  (set! *pc* (stack-pop!))
  (set! *env* (stack-pop!)))



(define (disassemble code addr port)
  (define (align-right s len)
    (string-append
      (make-string (- len (string-length s)) #\space)
      s))
  (define (align-left s len)
    (string-append
      s
      (make-string (- len (string-length s)) #\space)))

  (unless (null? code)
    (display (align-right (number->string addr 16) 6) port)
    (display ": " port)
    (let ([ins (instruction-decode (car code))]
          [size (instruction-size (car code))])
      (if (> size 1)
        (display (align-left (symbol->string ins) 16) port)
        (display (symbol->string ins) port))
      (set! addr (+ addr size))
      (case size
        [(3)
         (case ins
           [(goto goto-if-true goto-if-false closure)
            (display (number->string
                       (+ addr (+ (cadr code)
                                  (* 256 (caddr code))))
                       16) port)]
           [else
             (display (cadr code) port)
             (display " " port)
             (display (caddr code) port)])
         (newline port)
         (disassemble (cdddr code) addr port)]
        [(2)
         (display (cadr code) port)
         (case ins
           [(const)
            (display "  ; " port)
            (write (get-constant (cadr code)) port)]
           [(global-ref global-set)
            (display "  ; " port)
            (display (car (get-global (cadr code))) port)]
           )
         (newline port)
         (disassemble (cddr code) addr port)]
        [(1)
         (newline port)
         (disassemble (cdr code) addr port)]))))

;;;;;;;;;;;;;;;;;;;;;;       primitives         ;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax add-primitive!
  (syntax-rules ()
    [(_ name f)
     (set-cdr! (list-ref *globals*
                         (get-global-index 'name))
               (make-primitive f))]))

(define (add-primitives!)
  (add-primitive! car car)
  (add-primitive! cdr cdr)
  (add-primitive! cons cons)
  (add-primitive! not not)
  (add-primitive! list list)

  (add-primitive! null? null?)
  (add-primitive! list? list?)
  (add-primitive! eq? eq?)
  (add-primitive! eqv? eqv?)
  (add-primitive! equal? equal?)

  (add-primitive! + +)
  (add-primitive! - -)
  (add-primitive! * *)
  (add-primitive! / /)
  (add-primitive! = =)
  (add-primitive! > >)
  (add-primitive! < <)
  (add-primitive! >= >=)
  (add-primitive! <= <=)

  ; (add-primitive! map (lambda (x) x))
  (add-primitive! apply (lambda (x) x))
  (add-primitive! call/cc (lambda (x) x))
  (add-primitive! eval (lambda (x) x))

  (add-primitive! display display)
  (add-primitive! newline newline)
  )
