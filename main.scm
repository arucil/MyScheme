
(load "runtime.scm")

(define (run-toplevel exprs)
  (init-runtime!)
  (run (meaning-toplevel exprs)))

(begin
  (if (null? (cdr (command-line)))
      (repl)
      (call/cc
       (lambda (k)
         (set! *exit* k)
         (initialize!)
         (for-each
          (lambda (filename)
            (run-toplevel (load-file filename)))
          (cdr (command-line)))))))
