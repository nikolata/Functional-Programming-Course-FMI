#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме функция, която приема списък и две числа и връща
; списък, състоящ се от елементите на списъка, които се намират на индекси от първото число до второто.
(define (reverse-helper lst acc)
  (if (null? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-helper lst '()))

(define (slice xs start end)
  (define (helper xs start end ind res)
    (cond
      ((or (> ind end) (equal? xs '())) (reverse res))
      ((>= ind start) (helper (cdr xs) start end (+ 1 ind) (cons (car xs) res)))
      (else
       (helper (cdr xs) start end (+ 1 ind) res))))
  (helper xs start end 0 '())
 )

(define tests
 (test-suite "Slice tests"
     (check-equal? (slice '(1 9 8 2) 1 2) '(9 8))
     (check-equal? (slice '(1 9 2 8 3) 2 10) '(2 8 3))
     (check-equal? (slice '(9 7 2 3) 0 2) '(9 7 2)) 
  )
)

(run-tests tests 'verbose)