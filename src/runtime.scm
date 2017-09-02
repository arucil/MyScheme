
(load "compiler.scm")

(define *dasm* #f)

(define (init-library!)
  (init-runtime!)
  (run
   (meaning-toplevel
    '((define map
        (letrec ([map1 (lambda (f ls)
                         (if (null? ls)
                             '()
                             (cons (f (car ls))
                                   (map1 f (cdr ls)))))])
          (lambda (f . lss)
            (if (null? (car lss))
                '()
                (cons (apply f (map1 car lss))
                      (apply map f (map1 cdr lss)))))))

      (define (not val)
        (if val #f #t))

      (define length
        (letrec ([f (lambda (ls len)
                      (if (null? ls)
                          len
                          (f (cdr ls) (+ len 1))))])
          (f ls 0)))

      (define (list . x)
        x)
      ))))

(define (init-runtime!)
  (init-stack!)
  (set! *env* #f))

(define (run pc)
  (if *dasm*
      (disassemble pc 0 (current-output-port)))
  (set! *pc* pc)
  (call/cc
   (lambda (k)
     (set! *exit* k)
     (let loop ()
       (run-instruction)
       (loop)))))


(define (repl)
  (initialize!)
  (init-library!)
  (let loop ()
    (init-runtime!)
    (display "myscheme> ")
    (write (run (meaning-toplevel (list (read)))))
    (newline)
    (loop)))
