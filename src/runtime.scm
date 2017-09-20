
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
  (run
   (meaning-toplevel
    (load-file "library.scm"))))

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
	(let ([e (read)])
	  (unless (eof-object? e)
	    (write (run (meaning-toplevel (cons e '()))))
        (newline)
        (loop)))))
