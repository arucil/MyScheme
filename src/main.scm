
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
            (with-input-from-file filename
              (lambda ()
                (run-toplevel
                 (let rec ([e (read)])
                   (if (eof-object? e)
                       '()
                       (cons e (rec (read)))))))))
          (cdr (command-line)))))))
