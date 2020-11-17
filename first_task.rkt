(define a "\u250C") ;┌
(define b "\u2510") ;┐
(define c "\u2500") ;─
(define d "\u2514") ;└
(define e "\u2518") ;┘
(define f "\u2502") ;|

(define (next number)
  (+ 1 number))

(define (dash number)
      c
)

(define (line-left number)
  (string-append f " ")
)

(define (line-right number)
  (string-append " " f)
)

(define (accumulate start end null-value op next term)
  (define (loop index result)
    (if (> index end)
        result
        (loop (next index) (op result (term index)))))
  (loop start null-value)
)

(define (print-up-row number)
  (if (even? number)
      (string-append (accumulate 0 (* number 4) a string-append  next dash) b)
      (string-append (accumulate 0 (* number 4) a string-append  next dash) b)
  ))

(define (print-down-row number)
  (if (even? number)
      (string-append (accumulate 0 (* number 4) d string-append  next dash) e)
      (string-append (accumulate 0 (* number 4) d string-append  next dash) e)
  ))

(define (put-vertical-line number isItLeft)
  (if (= isItLeft 0)
      (accumulate 1 number "" string-append next line-left)
      (accumulate 1 number "" string-append next line-right)
  ))

(define (abs1 number)
  (if (< number 0)
      (* -1 number)
      number))

(define (display-square index number)
  (cond
    ((< number 1)(display ""))
    ((= 0 index) (display (print-up-row (- number 1))))
    ((= (* number 2) index) (display (print-down-row (- number 1))))
    ((< index number) (display (string-append (put-vertical-line index 0) (print-up-row (abs1(- number (+ index 1)))) (put-vertical-line index 1))))
    ((> index number) (display (string-append (put-vertical-line (- number (- index number)) 0) (print-down-row (- index (+ number 1))) (put-vertical-line (- number (- index number)) 1))))
    (else
     (display ""))))
     

(define (squares n)
  (accumulate 0 (* n 2) "" (lambda (_ index)(if (= index n) (display-square index n) (and (display-square index n) (display "\n")))) next (lambda (x) x)))
