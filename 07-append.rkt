#lang racket
(require rackunit)
(require rackunit/text-ui)
(require racket/trace)
; Търсим функция, която конкатенира два списъка
(define (reverse-helper lst acc)
  (if (null? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-helper lst '()))


(define (append xs ys)
  (define (helper rev ys)
    (if (equal? rev '())
        ys
        (helper (cdr rev) (cons (car rev) ys))))
  (helper (reverse xs) ys)
)

(define tests
  (test-suite "append tests"
    (check-equal? (append '(5 9 2) '(1)) '(5 9 2 1))
    (check-equal? (append '() '(2 3)) '(2 3))
    (check-equal? (append '(2 3) '()) '(2 3))
    (check-equal? (append '(1 8 6 2 3) '(2 3)) '(1 8 6 2 3 2 3))
  )
)
(run-tests tests 'verbose)