(define (to-binary-string number)
  (number->string number 2))

(define (string-to-number str)
  (string->number str 2))

(define (next x)
  (+ x 1))

(define (accumulate start end null-value op next term)
  (define (loop index result)
    (if (> index end)
        result
        (loop (next index) (op result (term index)))))
  (loop start null-value)
)

(define (find-smaller str1 str2)
  (if (<= (string-length str1) (string-length str2))
      (string-length str1)
     (string-length str2)))


(define (len set)
  (string-length (to-binary-string set)))

(define (set-add set elem)
  (define bin (to-binary-string set))
  (cond
    ((= (len set) elem) set)
    ((> elem (len set)) (string-to-number(string-append (string-append "1" (make-string (- elem (len set)) #\0)) bin)))
    (else
     (string-to-number(string-append (substring bin 0 (- (len set) 1 elem)) "1" (substring bin (- (len set) elem)))))))

(define (set-remove set elem)
  (define bin (to-binary-string set))
  (string-to-number (string-append (substring bin 0 (- (len set) 1 elem)) "0" (substring bin (- (len set) elem)))))

(define (set-contains? set elem)
  (define bin (to-binary-string set))
  (if (> elem (- (len set) 1))
      #f
      (equal? (string (string-ref bin (- (len set) 1 elem))) "1")))

(define (set-empty? set)
  (if (= set 0)
      #t
      #f))

(define (set-size set)
  (define bin (to-binary-string set))
  (accumulate 0 (- (len set) 1) 0 (lambda (res index) (if (equal? (string (string-ref bin index)) "1") (+ res 1) res)) next (lambda (x) x)))



(define (reverse-string str)
  (accumulate 0 (- (string-length str) 1) "" (lambda (res index) (string-append (string (string-ref str index)) res)) next (lambda (x) x)))

(define (set-intersect s1 s2)
  (define bin1 (to-binary-string s1))
  (define bin2 (to-binary-string s2))
  (define rev-bin1 (reverse-string bin1))
  (define rev-bin2 (reverse-string bin2))
  (string-to-number(reverse-string (accumulate 0 (- (find-smaller bin1 bin2) 1) "" (lambda (res index) (if (and (equal? (string (string-ref rev-bin1 index)) "1") (equal? (string (string-ref rev-bin2 index)) "1")) (string-append res "1") (string-append res "0"))) next (lambda (x) x))))
)


(define (set-union-helper s1 s2)
  (define bin1 (to-binary-string s1))
  (define bin2 (to-binary-string s2))
  (define rev-bin1 (reverse-string bin1))
  (define rev-bin2 (reverse-string bin2))
  (if (>= (len s1) (len s2))
      (substring rev-bin1 ( len s2))
      (substring rev-bin2 (len s1))))
      
(define (set-union s1 s2)
  (define bin1 (to-binary-string s1))
  (define bin2 (to-binary-string s2))
  (define rev-bin1 (reverse-string bin1))
  (define rev-bin2 (reverse-string bin2))
  (string-to-number(reverse-string (string-append (accumulate 0 (- (find-smaller bin1 bin2) 1) ""
                                (lambda (res index) (if (or (equal? (string (string-ref rev-bin1 index)) "1") (equal? (string (string-ref rev-bin2 index)) "1"))
                                                        (string-append res "1") (string-append res "0"))) next (lambda (x) x))
                                  (set-union-helper s1 s2)))))



(define (set-difference s1 s2)
  (define bin1 (to-binary-string s1))
  (define bin2 (to-binary-string s2))
  (define rev-bin1 (reverse-string bin1))
  (define rev-bin2 (reverse-string bin2))
  (if (= s1 0)
      0
      (string-to-number(reverse-string (string-append (accumulate 0 (- (len s2) 1) ""
                                (lambda (res index) (if (and (equal? (string (string-ref rev-bin1 index)) "1") (equal? (string (string-ref rev-bin2 index)) "0"))
                                                        (string-append res "1") (string-append res "0"))) next (lambda (x) x))
                                   (substring rev-bin1 ( len s2)))))))

(define (w i)
  (+ i 1))

(define (p i)
  (+ i 1))

(define (knapsack c n w p)
  (define (knapsack-helper c n w p index set)
    (cond
      ((= n 0) 0)
      ((> (w index) c) (knapsack-helper c (- n 1) w p (+ index 1) set))
      (else
       (max (knapsack-helper c (- n 1) w p (+ index 1) set) (+ (set-add set index) (knapsack-helper (- c (w index)) (- n 1) w p (+ index 1) set))))))
  (knapsack-helper c n w p 0 0))
