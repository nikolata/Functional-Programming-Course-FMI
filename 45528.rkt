;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 1
; 2020-11-07
;
; Начален час на контролното: 9
; Име: Никола Методиев
; ФН: 445528
; Специалност: Информатика
; Курс: 3
; Административна група: 2 
;


(define (to-binary-string number)
  (number->string number 2))


(define (binary-to-number str)
  (string->number str 2))


(define (accumulate start end null-value op next term)
  (define (loop index result)
    (if (> index end)
        result
        (loop (next index) (op result (term index)))))
  (loop start null-value))
(define (reverse-str str)
  (accumulate 0 (- (string-length str) 1) "" (lambda (result index) (string-append (string (string-ref str index)) result)) (lambda (x) (+ x 1)) (lambda (x) x)))

(define (elem-bigger-helper str elem)
  (string-append "1" (accumulate 0 (- elem (string-length str) 1) "" (lambda (result index) (string-append result "0")) (lambda (x) (+ x 1)) (lambda (x) x)) str))

(define (elem-smaller-helper index bin elem res)
  (cond
    ((> index (string-length bin)) (reverse-str res))
    ((= index elem) (elem-smaller-helper (+ 1 index) bin elem(string-append res 1)))
    (else
     (elem-smaller-helper (+ 1 index) bin (string-append result elem (string (string-ref bin index)))))))

                          


(define (nset-add s elem)
  (define bin (to-binary-string s))
  (define rev-bin (reverse-str (to-binary-string s)))
   (if (> elem (string-length bin))
       (elem-bigger-helper bin elem)
       (elem-smaller-helper 0 rev-bin "")))





  