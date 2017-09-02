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
