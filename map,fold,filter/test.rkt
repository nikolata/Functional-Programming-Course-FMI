#lang racket
(require rackunit)
(require rackunit/text-ui)

; flatten
; функция, която премахва допълнителни нива на влагане в списъци
; за да я имплементираме, е хубаво да видим какво правят функциите atom? и/или list?
; хубаво е и да помислим дали не може да използваме някоя от map/filter/fold за проблема

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (give-element xs)
  (if (atom? (car xs))
      (car xs)
      (give-element (cdr xs))))


(give-element '((5)))
(define (flatten xs)
  (void)
)

(define tests
  (test-suite "flatten"
    (check-equal? (flatten '((1) 2 ((3 4 (5)) (((((6))))))))  '(1 2 3 4 5 6))
    (check-equal? (flatten '(1 2 3 (4)))  '(1 2 3 4))
    (check-equal? (flatten '(() () 3 (2))) '(3 2))
  )
)

(run-tests tests 'verbose)