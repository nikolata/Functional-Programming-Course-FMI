#lang racket
(require rackunit)
(require rackunit/text-ui)
; линк към задачата - https://github.com/triffon/fp-2019-20/blob/master/exercises/computer-science-3/exercises/01.introduction/04-count-digits.rkt
; 1.5 - Търсим процедура, която намира броя цифри на дадено число
; Трябва да работи и за отрицателни числа.

; опашкова рекурсия

#|(define (count-digits number)
  (define (helper count number)
    (if (= (quotient number 10) 0)
        (+ count 1)
        (helper (+ count 1) (quotient number 10))))
  (helper 0 number)
)|#

; нормална рекурсия
(define (count-digits number)
  (if (= (quotient number 10) 0)
  1
  (+ 1 (count-digits (quotient number 10)))))

(define tests
  (test-suite
    "Count digits tests"

    (test-case "Should count correctly"
      (check-equal? (count-digits 1024) 4)
    )
    (test-case "Should work alright with negative numbers"
      (check-equal? (count-digits -987421245) 9)
    )
    (test-case "Should work alright with digits"
      (check-equal? (count-digits 9) 1)
      (check-equal? (count-digits 0) 1)
    )
  )
)


(run-tests tests 'verbose)