
;;;;;;;;;;;;;;;;;;                       ;;;;;;;;;;;;;;;;;;

(define (runtime-error)
  'stub)

;;;;;;;;;;;;;;;;;;             constants         ;;;;;;;;;;;;;;;;;;;;;

(define *constants* '())

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

(define (init-stack!)
  (set! *stack* '()))

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
;;     v              #(...)
;;   #(local0 local0 ...)

(define-record-type activation-record (fields next frame))

(define (local-reference i j)
  (let loop ([i i] [env *env*])
    (if (zero? i)
      (vector-ref (activation-record-frame env) j)
      (loop (- i 1)
            (activation-record-next env)))))

(define (local-assign i j v)
  (let loop ([i i] [env *env*])
    (if (zero? i)
      (vector-set! (activation-record-frame env) j v)
      (loop (- i 1)
            (activation-record-next env)))))

;;;;;;;;;;;;;;;;           globals              ;;;;;;;;;;;;;;;;;;;;;;;;

;;  ( (name . value)
;;    ...
;;  )

(define *global*)

(define undefined-tag "undefined")

(define (global-reference i)
  (let ([v (list-ref *global* i)])
    (if (eq? undefined-tag (cdr v))
      (runtime-error "Undefined global variable" (car v))
      (cdr v))))

(define (global-assign i v)
  (set-cdr! (list-ref *global* i) v))

(define (get-global-index name)
  (let loop ([g *global*] [i 0])
    (cond
      [(null? g)
       (set! *global* (append *global* (cons name undefined-tag)))
       i]
      [(eq? (caar g) name)
       i]
      [else (loop (cdr g) (+ i 1))])))

;;;;;;;;;;;;;;;;;          registers            ;;;;;;;;;;;;;;;;;;;;;;;;

(define *acc*)

(define *fun*)

(define *env*)

(define *pc*)

;;;;;;;;;;;;;;;;;         closure          ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type closure (fields env code))

;;;;;;;;;;;;;;;;;            instruction set       ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-instruction-set
  (syntax-rules (define-instruction)
    [(_ (define-instruction (instruction arg ...) code . body) ...)
     (begin
       (set! instruction-code
         (lambda (ins . params)
           (case ins
             [(instruction) (cons code params)]
             ...)))

       (set! instruction-size
         (lambda (c)
           (case c
             [(code) (+1 (length (list arg ...)))]
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

(define instruction-code)
(define instruction-size)
(define run-instruction)

(define-instruction-set
  ;; constant
  (define-instruction (const-true) 1
    (set! *acc* #t))
  (define-instruction (const-false) 2
    (set! *acc* #f))
  (define-instruction (const-null) 3
    (set! *acc* '()))
  (define-instruction (const-0) 4
    (set! *acc* 0))
  (define-instruction (const-1) 5
    (set! *acc* 1))
  (define-instruction (const i) 6
    (set! *acc* (get-constant i)))

  ;; reference
  (define-instruction (shallow-ref i) 10
    (set! *acc* (local-reference 0 i)))
  (define-instruction (deep-ref i j) 11
    (set! *acc* (local-reference i j)))
  (define-instruction (global-ref i) 12
    (set! *acc* (global-reference i)))

  ;; assignment
  (define-instruction (shallow-ref i) 13
    (local-assign 0 i *acc*))
  (define-instruction (deep-ref i j) 14
    (local-assign i j *acc*))
  (define-instruction (global-ref i) 15
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
    (set! *acc* (make-closure *env* *pc*)))

  ;; miscellaneous
  (define-instruction (exit) 30
    'stub)
  (define-instruction (return) 31
    (set! *pc* (stack-pop!))
    (set! *env* (stack-pop!)))
  )

;; auxiliary functions

(define (common-goto offset1 offset2)
  (set! *pc* (list-tail *pc* (+ offset1 (* 256 offset2)))))
