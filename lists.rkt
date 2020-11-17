(define a (cons 1 2))

(car a)

(cdr a)

(define b '(1 2 3 4))
b
(cons 0 b)
(cdr b)


(define (sum l)
  (if (equal? l '())
      0
      (+ (car l) (sum (cdr l)))))

(sum '(1 2 3 4 5))
(sum '())