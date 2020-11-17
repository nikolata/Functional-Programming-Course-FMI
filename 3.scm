(define (next x)
  (+ x 1))

(define (accumulate start end null-value op next term)
  (define (loop index result)
    (if (> index end)
        result
        (loop (next index) (op result (term index)))))
  (loop start null-value)
)
(define (remove-spaces str)
  (accumulate 0 (- (string-length str) 1) "" (lambda (res index) (if (equal? (string (string-ref str index)) " ") res (string-append res (string (string-ref str index)))))
              next (lambda (x) x)))


(define (check-next index expr)
  (cond
    ((= index (string-length expr)) 1)
    ((char-numeric? (string-ref expr index)) 0)
    (else
     1)))

(define (check-prev index expr)
  (cond
    ((< index 0) 1)
    ((char-numeric? (string-ref expr index)) 0)
    (else
     1)))

(define (expr-valid? expr)
  (define (helper index expr)
    (cond
      ((= index (string-length expr)) #t)
      ((equal? (string (string-ref expr index)) "+") (if (and (= (check-next (+ index 1) expr) 0) (= (check-prev (- index 1) expr) 0)) (helper (+ index 1) expr) #f))
      ((equal? (string (string-ref expr index)) "-") (if (and (= (check-next (+ index 1) expr) 0) (= (check-prev (- index 1) expr) 0)) (helper (+ index 1) expr) #f))
       ((equal? (string (string-ref expr index)) "*") (if (and (= (check-next (+ index 1) expr) 0) (= (check-prev (- index 1) expr) 0)) (helper (+ index 1) expr) #f))
       ((equal? (string (string-ref expr index)) "/") (if (and (= (check-next (+ index 1) expr) 0) (= (check-prev (- index 1) expr) 0)) (helper (+ index 1) expr) #f))
       ((equal? (string (string-ref expr index)) "^") (if (and (= (check-next (+ index 1) expr) 0) (= (check-prev (- index 1) expr) 0)) (helper (+ index 1) expr) #f))
       (else (helper (+ index 1) expr))))
  (helper 0 (remove-spaces expr)))