#lang racket

(require racket/trace)
(define (accumulate start end null-value op next term)
  (define (loop index result)
    (if (> index end)
        result
        (loop (next index) (op result (term index)))))
  (loop start null-value)
)

;Напишете функция, която намира сумата от цифрите на число.
(define (sum-numbers number)
  (define (helper result number)
    (if (= (quotient number 10) 0)
        (+ result number)
        (helper (+ result (remainder number 10)) (quotient number 10))))
  (helper 0 number))

(sum-numbers 54321)


;Напишете функция, която проверява дали число съдържа две еднакви цифри.
(define (wrapper-helper curr number)
  (cond
    ((and (= (quotient number 10) 0) (not (= number curr))) 1)
    ((and (= (quotient number 10) 0) (= number curr)) 0)
    ((= (remainder number 10) curr) 0)
    (else
     (wrapper-helper curr (quotient number 10)))))

(define (equal-numbers? number)
  (define (wrapper curr number)
    (cond
      ((= (quotient number 10) 0) #f)
      ((= (wrapper-helper curr number) 0) #t)
      (else
       (wrapper (remainder number 10) (quotient number 10)))))
  (wrapper (remainder number 10) (quotient number 10)))

(equal-numbers? -121)

; Напишете функция, която намира броя на единиците в двоичния запис на подадено число.
(define (one-count number)
  (define bin-string (number->string number 2))
  (define bin (string->number bin-string))
  (define (inner count number)
    (cond
      ((and (= (quotient number 10) 0) (= number 0)) count)
      ((and (= (quotient number 10) 0) (= number 1)) (+ 1 count))
      ((= (remainder number 10) 1) (inner (+ count 1) (quotient number 10)))
      (else
       (inner count (quotient number 10)))))
  (inner 0 bin)
  )
(one-count 34)


(define (stepen e x)
  (accumulate 1 x 0 (lambda (result index)(+ result e)) (lambda (x) (+ x 1)) (lambda (x) x)))


(stepen 2 4)



(define (sum-in-interval start end)
  (accumulate start end 0 (lambda (result index) (if (odd? (sum-numbers index)) (+ result index) result)) (lambda (x) (+ x 1)) (lambda (x) x)))

(sum-in-interval 10 15) 


(define (constantly c)
  (lambda (x) (+ c 1)))
(define forever-21 (constantly 21))
(forever-21 5)