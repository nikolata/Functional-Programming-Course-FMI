#lang racket
(require rackunit)
(require rackunit/text-ui)

; remove-duplicates 
; премахва всички повтарящи се елементи от списъка

(define (append xs ys)
  (if (null? xs) ys
      (cons (car xs) (append (cdr xs) ys))))

(define (check-if-in? list number)
  (cond
    ((null? list) #f)
    ((equal? (car list) number) #t)
    (else
     (check-if-in? (cdr list) number))))

(define (remove-duplicates xs)
  (define (helper xs res)
    (cond
      ((null? xs) res)
      ((eq? (check-if-in? (cdr xs) (car xs)) #f) (helper (cdr xs) (append res (list (car xs)))))
      (else
       (helper (cdr xs) res))))
  (helper xs '())
)

(define tests
  (test-suite "remove-duplicates"
    (check-equal? (remove-duplicates '(1 1 2 2 1 3 3 2 3))  '(1 2 3))
    (check-equal? (remove-duplicates '(1 2 3))  '(1 2 3))
  )
)

(run-tests tests 'verbose)