#lang racket
(require rackunit)
(require rackunit/text-ui)
(require racket/trace)

; Търсим функция, която връща списък от първите n елемента на даден такъв.

(define (reverse-helper lst acc)
  (if (null? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-helper lst '()))

(define (take n xs)
  (define (helper n xs ind res)
    (if (or (= ind n) (equal? xs '()))
        res
        (helper n (cdr xs) (+ 1 ind) (cons (car xs) res))))
  (helper n (reverse xs) 0 '())
        
)

(define tests
  (test-suite "Take tests"
     (check-equal? (take 2 '(1 2 3)) '(1 2))
     (check-equal? (take 0 '(2 9 2)) '())
     (check-equal? (take 2134 '(9 7 2 3)) '(9 7 2 3))
  )
)
(trace take)
(run-tests tests 'verbose)