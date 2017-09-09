(define map
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
    (lambda (ls)
      (f ls 0))))

(define (list . x)
  x)

(define last-pair
  (letrec ([f (lambda (ls)
                (if (null? (cdr ls))
                    ls
                    (f (cdr ls))))])
    (lambda (ls)
      (if (null? ls)
          '()
          (f ls)))))

(define append
  (letrec ([f (lambda (ls1 ls*)
                (cond
                 [(null? ls*) ls1]
                 [(null? ls1) (f (car ls*)
                                 (cdr ls*))]
                 [else (cons (car ls1) (f (cdr ls1) ls*))]))])
    (lambda lss
      (if (null? lss)
          '()
          (f (car lss) (cdr lss))))))
