#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да вземем i-тия елемент от списъка lst, като броим от 0.

(define (list-ref lst i)
  (define (helper lst ind i)
    (cond
      ((equal? lst '()) #f)
      ((= ind i) (car lst))
      (else
       (helper (cdr lst) (+ 1 ind) i))))
  (helper lst 0 i)
)

(define tests
  (test-suite "List ref tests"
    (check-equal? (list-ref '(5 9 2) 0) 5)
    (check-equal? (list-ref '(1 8 6 2 3) 4) 3)
  )
)

(run-tests tests 'verbose)