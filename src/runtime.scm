
(load "compiler.scm")

(define *dasm* #f)

(define (load-file filename)
  (with-input-from-file filename
    (lambda ()
      (let rec ([e (read)])
        (if (eof-object? e)
            '()
            (cons e
                  (rec (read))))))))

(define (init-library!)
  (init-runtime!)
  (run (load-file "prelude.scm")))

(define (init-runtime!)
  (init-stack!)
  (set! *env* #f))

(define (run e*)
  (call/cc
   (lambda (k)
     (set! *exit* k)
     (let ([pc (meaning-toplevel e*)])
       (if *dasm*
           (disassemble pc 0 (current-output-port)))
       (set! *pc* pc)
       (let loop ()
         (run-instruction)
         (loop))))))


(define (repl)
  (initialize!)
  (init-library!)
  (reset-exit-flag!)
  (let loop ()
    (init-runtime!)
    (display "myscheme> ")
    (let ([e (read)])
      (unless (eof-object? e)
        (write (run (cons e '())))
        (newline)
        (unless (exit-flag?)
          (loop))))))
