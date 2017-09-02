
(load "compiler.scm")

(define *dasm* #f)

(define (init-runtime!)
  (init-stack!)
  (set! *env* #f))

(define (run pc)
  (if *dasm*
      (disassemble pc 0 (current-output-port)))
  (set! *pc* pc)
  (let loop ()
    (run-instruction)
    (loop)))


(define (repl)
  (initialize!)
  (let loop ()
    (display
     (call/cc
      (lambda (k)
        (set! *exit* k)
        (init-runtime!)
        (display "myscheme> ")
        (run (meaning-toplevel (list (read)))))))
    (newline)
    (loop)))
