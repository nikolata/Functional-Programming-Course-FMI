#lang racket
(define matrix '((1 2 3 4) (5 6 7 8)))

(define (number-rows m)
  (length m))

(define (number-cols m)
  (if (null? m)
      0
      (length (car m))))

(define (first-row m)
  (car m))

(define (remove-first-row m)
  (cdr m))

(define (get-row m n)
  (list-ref m n))

(define (get-elem m r c)
  (list-ref (list-ref m r) c))

(define (remove-nth l n)
  (cond
    ((null? l) l)
    ((= n 0) (cdr l))
    (else
     (cons (car l) (remove-nth (cdr l) (- n 1))))))

(define (remove-row m n)
  (remove-nth m n))

(define (remove-col m n)
  (map (lambda (x) (remove-nth x n)) m))

(define (transpose M)
  (apply map list M)
)
