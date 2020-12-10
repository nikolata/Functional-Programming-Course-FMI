#lang racket
(require rackunit)
(require rackunit/text-ui)

; zip
; може да видим какво се очаква да прави в тестовете
(define (append2 xs ys)
 (if (null? xs) ys
    (cons (car xs) (append2 (cdr xs) ys))))

(define (zip xs ys)
  (define (helper xs ys res)
    (cond
      ((or (equal? xs '()) (equal? ys '())) res)
      (else
       (helper (cdr xs) (cdr ys) (append2 res (list (list (car xs) (car ys))))))))
  (helper xs ys '())
)

(define tests
  (test-suite "Zip"
    (check-equal? (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
    (check-equal? (zip '(28 9 12) '(1 3)) '((28 1) (9 3)))
  )
)

(run-tests tests 'verbose)