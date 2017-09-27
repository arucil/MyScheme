
;;;;;;;;;;;;;;;;;;           miscellaneous            ;;;;;;;;;;;;;;;;;;

(define error-tag "Error")

(define (runtime-error . args)
  (*exit* args))

(define (compile-error . args)
  (*exit* args))


(define (string-trim s)
  (do ([i 0 (+ i 1)])
      [(or (>= i (string-length s))
           (not (char-whitespace? (string-ref s i))))
       (do ([j (- (string-length s) 1) (- j 1)])
           [(or (< j i)
                (not (char-whitespace? (string-ref s j))))
            (substring s i (+ j 1))])]))

;; works for both proper & improper lists
(define (for-each* f ls)
  (cond
   [(null? ls) #f]
   [(pair? ls)
    (f (car ls))
    (for-each* f (cdr ls))]
   [else
    (f ls)]))

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

(define-record-type activation-record (fields (mutable next) frame))

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
      (make-list (- n 1) (cons '() ls))))

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
  (add-primitives!)
  (add-macros!)) ;; defined in compiler.scm

(define undefined-tag "undefined")

(define (global-reference i)
  (let ([v (list-ref *globals* i)])
    (if (eq? undefined-tag (cdr v))
      (runtime-error "Undefined variable" (car v))
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

;; return a pair
(define (get-global name)
  (list-ref *globals*
            (get-global-index name)))

;;;;;;;;;;;;;;;;         continuation            ;;;;;;;;;;;;;;;;;;;;;;

(define-record-type continuation (fields stack))

(define (invoke-continuation k env)
  (set! *acc* (local-reference 0 0 env))
  (set! *stack* (continuation-stack k))
  (return) ; pop pc and env
  )

;;;;;;;;;;;;;;;;;          registers            ;;;;;;;;;;;;;;;;;;;;;;;;

(define *acc*)

(define *env*)

(define *pc*)

(define *exit*)

;;;;;;;;;;;;;;;;;         closure          ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type closure (fields env code))

;;;;;;;;;;;;;;;;;         primitive          ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type primitive (fields func))

(define (invoke-primitive p env)
  ((primitive-func p)
   ;; remove last ()
   (apply apply list
          (activation-record-frame env))))

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

(define (invoke env)
  (cond
   [(closure? *acc*)
    (set! *pc* (closure-code *acc*))
    (activation-record-next-set! env (closure-env *acc*))
    (set! *env* env)]
   [(primitive? *acc*)
    (invoke-primitive *acc* env)]
   [(continuation? *acc*)
    (invoke-continuation *acc* env)]
   [else
    (runtime-error "Attempt to apply non-procedure" *acc*)]))

(define (common-call n tail?)
  (let ([env (store-fixed-arguments
              n
              (extend-env n #f))])
    (unless tail?
      (stack-push! *env*)
      (stack-push! *pc*))
    (invoke env)))

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

(define-syntax add-global!
  (syntax-rules ()
    [(_ name val)
     (set-cdr! (get-global 'name) val)]))

(define-syntax add-primitive!
  (syntax-rules (..)
    [(_ name f)
     (add-global! name (make-primitive f))]
    [(_ name f arity)
     (add-primitive! name
                     (lambda (args)
                       (if (= (length args) arity)
                           (begin
                             (set! *acc* (apply f args))
                             (return))
                           (runtime-error "Incorrect arity for" 'name))))]
    [(_ name f arity ..)
     (add-primitive! name
                     (lambda (args)
                       (if (>= (length args) arity)
                           (begin
                             (set! *acc* (apply f args))
                             (return))
                           (runtime-error "Incorrect arity for" 'name))))]))

(define (add-primitives!)
  (add-primitive! car car 1)
  (add-primitive! cdr cdr 1)
  (add-primitive! cons cons 2)
  (add-primitive! set-car! set-car! 2)
  (add-primitive! set-cdr! set-cdr! 2)

  (add-primitive! null? null? 1)
  (add-primitive! list? list? 1)
  (add-primitive! pair? pair? 1)
  (add-primitive! number? number? 1)
  (add-primitive! boolean? boolean? 1)
  (add-primitive! string? string? 1)
  (add-primitive! char? char? 1)

  (add-primitive! eq? eq? 2)
  (add-primitive! eqv? eqv? 2)
  (add-primitive! equal? equal? 2)

  (add-primitive! + + 0 ..)
  (add-primitive! - - 1 ..)
  (add-primitive! * * 0 ..)
  (add-primitive! / / 1 ..)
  (add-primitive! = = 2 ..)
  (add-primitive! > > 2 ..)
  (add-primitive! < < 2 ..)
  (add-primitive! >= >= 2 ..)
  (add-primitive! <= <= 2 ..)
  (add-primitive! abs abs 1)
  (add-primitive! sqrt sqrt 1)

  (add-primitive! display display 1)
  (add-primitive! newline newline 0)

  (add-primitive! string string 0 ..)
  (add-primitive! make-string make-string 1)
  (add-primitive! string-ref string-ref 2)
  (add-primitive! string-set! string-set! 3)
  (add-primitive! string-length string-length 1)
  (add-primitive! string->list string->list 1)
  (add-primitive! list->string list->string 1)

  (add-primitive! vector vector 0 ..)
  (add-primitive! make-vector make-vector 1)
  (add-primitive! vector-ref vector-ref 2)
  (add-primitive! vector-set! vector-set! 3)
  (add-primitive! vector-length vector-length 1)
  (add-primitive! vector->list vector->list 1)
  (add-primitive! list->vector list->vector 1)

  (add-primitive! exit
                  (lambda (args)
                    (if (not (= 1 (length args)))
                        (runtime-error "Incorrect arity for" 'exit)
                        (*exit* (car args)))))

  (add-primitive! apply
                  (lambda (args)
                    (if (< (length args) 2)
                        (runtime-error "Incorrect arity for" 'apply)
                        (let loop ([ls (cdr args)]
                                   [size 0])
                          (if (null? (cdr ls))
                              (begin
                                (for-each stack-push! (car ls))
                                (set! *acc* (car args))
                                (common-call (+ size (length (car ls)))
                                             #t))
                              (begin
                                (stack-push! (car ls))
                                (loop (cdr ls)
                                      (+ size 1))))))))

  (add-primitive! call/cc
                  (lambda (args)
                    (if (not (= 1 (length args)))
                        (runtime-error "Incorrect arity for" 'call/cc)
                        (begin
                          (stack-push! (make-continuation *stack*))
                          (set! *acc* (car args))
                          (common-call 1 #t)))))

  (add-primitive! eval
                  (lambda (args)
                    (if (not (= 1 (length args)))
                        (runtime-error "Incorrect arity for" 'eval)
                        (set! *pc*
                          (meaning-toplevel args)))))
  )
