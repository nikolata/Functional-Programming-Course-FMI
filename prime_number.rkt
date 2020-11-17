#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да проверим дали число е просто.

(define (prime? number)
  (define (helper curr number)
    (if (= number 1)
        #f
        (if (>= curr number)
            #t
            (if (and (= (remainder number curr) 0) (not (= curr number)) (not (= curr 1)))
                #f
                (helper (+ curr 1) number)))

        ))
  (helper 1 number)
)

(define tests
  (test-suite "prime? tests"
    (check-false (prime? 1))
    (check-true (prime? 5))
    (check-false (prime? 1729))
    (check-false (prime? 41041))
  )
)

(run-tests tests 'verbose)