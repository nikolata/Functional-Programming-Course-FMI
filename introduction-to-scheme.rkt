#lang racket

(define a 5)

(define c 10)

(println c)

(+ a c 5 6 7 8)

(remainder 3 2)

(quotient 5 2)

(/ 5 2)

(* 5 (+ 2 3))

(define (inc x)
  (+ x 1)
)

(inc 5)

;;; funcita za factoriel
(define (factorial x)
  (if (= x 0)
      1
      (* x (factorial (- x 1)))))

(factorial 24)

;;; da subere chislata ot start do end
(define (sum-interval start end)
  (if (= start end)
      end
      (+ start (sum-interval (+ 1 start) end)))
  )

(sum-interval 1 5)

;;; umnojenie na chisla s rekursiq

(define (multiply a b)
  (if (= b 0)
      0
      (+ a (multiply a (- b 1))))
  )

(multiply 2 15)