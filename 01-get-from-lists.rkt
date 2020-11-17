#lang racket
(require rackunit)
(require rackunit/text-ui)

; Нека имаме следния списък
(define my-list '(1 2 3 (4 5) (6 (7 8))))

; Искаме с подходящи извиквания на car и cdr да вземем всяко число.
; Първите две са за пример.

(define get-one (car my-list))

(define get-two (cadr my-list))

(define get-three (caddr my-list))

(define get-four (car (cadddr my-list)))

(define get-five (car( cdr (cadddr my-list))))

(define get-six (car (car(cddddr my-list))))

(define get-seven (car(car(cdr(car(cddddr my-list))))))

(define get-eight (cadr(car(cdr(car(cddddr my-list))))))

(define tests
  (test-suite "dummy tests"
    (check-equal? get-one 1)
    (check-equal? get-two 2)
    (check-equal? get-three 3)
    (check-equal? get-four 4)
    (check-equal? get-five 5)
    (check-equal? get-six 6)
    (check-equal? get-seven 7)
    (check-equal? get-eight 8)
  )
)

(run-tests tests 'verbose)
