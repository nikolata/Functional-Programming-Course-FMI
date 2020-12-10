#lang racket
(require racket/trace)
(provide (all-defined-out))
(require racket/string)
(require racket/stream)

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)


;проверява дали дърво представено като лист е валидно
(define (valid-list-tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (valid-list-tree? (cadr t))
           (valid-list-tree? (caddr t)))))

;взима ми индекса след края на числото
(define (get_number_index str ind)
  (cond
    ((not (char-numeric? (string-ref str ind))) ind)
    (else
     (get_number_index str (+ 1 ind)))))

;обхождам стринга елемент по елемент
;ако е спейс продължавам
;разписах си варианти на дървета и забелязах, че имам 4 типа елементи: {, }, * и число
;пазя си кой ми е предишния елемент и с проби разбрах какъв елемент може да следва след него
;та за предния елемент проверявам текущия дали е търсеният и така докрая, а ако има грешка гърмя
;пазя броя на скобите (нещо като стек) за да проверя накрая дали са правилни
;забелязах и, че броя на числата (и * броя като число) ми е равен на 2*брой отворени скоби + 1 (отново с опити)
;и благодарение на това премахвам опасността да имам повече от 2 деца
(define (loop str ind prevChar bracketCount starCount numbersCount openBracketCount )
  (cond
    ((and (= ind (string-length str)) (not (= 0 bracketCount))) #f) 
    ((and (= ind (string-length str)) (= bracketCount 0) (equal? "}" prevChar) (= (+ 1 (* 2 openBracketCount)) numbersCount))  #t)
    ((and (= ind (string-length str)) (= bracketCount 0) (equal? "}" prevChar) (< (+ 1 (* 2 openBracketCount)) numbersCount))  #f)
    ((equal? " " (string (string-ref str ind))) (loop str (+ 1 ind) prevChar bracketCount starCount numbersCount openBracketCount ))
    ((and (= bracketCount 0) (not (= ind (string-length str)))) #f)
    ((< bracketCount 0) #f) 
    ((and (char-numeric? (string-ref str ind)) (equal? "{" prevChar)) (loop str (get_number_index str ind) "number" bracketCount 0 (+ 1 numbersCount) openBracketCount))
    ((and (char-numeric? (string-ref str ind)) (not (equal? "{" prevChar))) #f)
    ((and (equal? prevChar "{") (and (not (char-numeric? (string-ref str ind))) (not (equal? " " (string (string-ref str ind)))))) #f)
    ((and (equal? (string (string-ref str ind)) "{") (equal? prevChar "number")) (loop str (+ 1 ind) "{" (+ 1 bracketCount) 0 numbersCount (+ 1 openBracketCount)))
    ((and (equal? (string (string-ref str ind)) "{") (equal? prevChar "*")) (loop str (+ 1 ind) "{" (+ 1 bracketCount) 0 numbersCount (+ 1 openBracketCount)))
    ((and (equal? (string (string-ref str ind)) "{") (equal? prevChar "}")) (loop str (+ 1 ind) "{" (+ 1 bracketCount) 0 numbersCount (+ 1 openBracketCount)))
    ((and (equal? (string (string-ref str ind)) "{") (equal? prevChar "{")) #f)
    ((and (equal? (string (string-ref str ind)) "*") (equal? prevChar "number")) (loop str (+ 1 ind) "*" bracketCount (+ 1 starCount) (+ 1 numbersCount) openBracketCount))
    ((and (equal? (string (string-ref str ind)) "*") (equal? prevChar "}")) (loop str (+ 1 ind) "*" bracketCount (+ 1 starCount ) (+ 1 numbersCount) openBracketCount))
    ((and (equal? (string (string-ref str ind)) "*") (equal? prevChar "*") (= starCount 1)) (loop str (+ 1 ind) "*" bracketCount (+ 1 starCount) (+ 1 numbersCount) openBracketCount))
    ((and (equal? (string (string-ref str ind)) "*") (equal? prevChar "*") (not (= starCount 1))) #f)
    ((and (equal? (string (string-ref str ind)) "*") (not (equal? prevChar "number")) (not (equal? prevChar "*"))) #f)
    ((and (equal? (string (string-ref str ind)) "}") (equal? prevChar "*")) (loop str (+ 1 ind) "}" (- bracketCount 1) 0 numbersCount openBracketCount))
    ((and (equal? (string (string-ref str ind)) "}") (equal? prevChar "}")) (loop str (+ 1 ind) "}" (- bracketCount 1) 0 numbersCount openBracketCount))
    ((and (equal? (string (string-ref str ind)) "}") (not (equal? prevChar "*")) (not (equal? prevChar "}"))) #f)
    (else
     #f))) 

;извиква ми реалната валидираща функция като премахва първата скоба
(define (tree? str)
  (cond
    ((= 0 (string-length str)) #f)
    ((equal? " " (string (string-ref str 0))) (tree? (substring str 1)))
    ((and (equal? "*" (string (string-ref str 0))) (= 1 (string-length str))) #t)
    ((equal? "{" (string (string-ref str 0))) (loop (substring str 1) 0 "{" 1 0 0 1))
    (else #f)))




(define (get_index_of_closing_bracket str ind bracketsCount)
  (cond
    ((equal? (string (string-ref str ind)) "{") (get_index_of_closing_bracket str (+ 1 ind) (+ 1 bracketsCount)))
    ((and (<= bracketsCount 1) (equal? (string (string-ref str ind)) "}")) ind)
    ((equal? (string (string-ref str ind)) "}") (get_index_of_closing_bracket str (+ 1 ind) (- bracketsCount 1)))
    (else
     (get_index_of_closing_bracket str (+ 1 ind) bracketsCount))))

(define (get_root_number str ind)
  (string->number (substring str ind (get_number_index str ind))))
  
(define (remove-spaces str ind res)
  (cond
    ((= ind (string-length str)) res)
    ((not (equal? (string (string-ref str ind)) " ")) (remove-spaces str (+ 1 ind) (string-append res (string (string-ref str ind)))))
    (else
     (remove-spaces str (+ 1 ind) res))))

;проверявам дали стринга е валиден и ако е вика функцията, която прави дървото
(define (string->tree str)
  (define (make-tree str ind)
    (cond
      ((= ind (string-length str)) '())
      ((equal? (string (string-ref str ind)) "*") '())
      ((char-numeric? (string-ref str ind)) (list (get_root_number str ind) (make-tree str (get_number_index str ind))
                                                  (make-tree str
                                                             (if (equal? (string (string-ref str (get_number_index str ind))) "*")
                                                                 (+ 1 (get_number_index str ind))
                                                                 (+ 1 (get_index_of_closing_bracket str ind 0))))))
      (else
       (make-tree str (+ 1 ind)))))
  (if (tree? str)
      (make-tree (remove-spaces str 0 "") 0)
      #f))

;използвам я да взимам всички елементи от дадено ниво
(define (tree-level tree n)
  (define (helper n tree ind res)
    (cond
      ((empty-tree? tree) '())
      ((= ind n) (root-tree tree))
      (else (append res (list (helper n (left-tree tree) (+ ind 1) res)) (list (helper n (right-tree tree) (+ ind 1) res))))))
  (helper n tree 0 '()))


;дава ми височината на дървото
(define (maxdepth tree)
  (cond 
    [(null? tree) 0]
    [else (+ 1 (max (maxdepth (left-tree  tree))
                    (maxdepth (right-tree tree))))]))

;проверка дали е валидно и ако е, дали е балансирано
(define (balanced? tree)
  (define (helper tree)
    (if
       (empty-tree? tree)
       #t
       (and (<= (abs (- (maxdepth (left-tree tree)) (maxdepth (right-tree tree)))) 1) (helper (left-tree tree)) (helper (right-tree tree)))))
  
  (if (not (valid-list-tree? tree))
      #f
      (helper tree)))
      

;проверка дали е валидно и ако е, дали е подредено
(define (ordered? tree)
  (define (helper tree prevRoot op?)
    (cond
      ((empty-tree? tree) 0)
      ((equal? prevRoot "first") (+ 0 (helper (left-tree tree) (root-tree tree) <) (helper (right-tree tree) (root-tree tree) >)))
      ((not (op? (root-tree tree) prevRoot)) 1)
      (else
       (+ 0 (helper (left-tree tree) (root-tree tree) <) (helper (right-tree tree) (root-tree tree) >)))))

  (if (not (valid-list-tree? tree))
      #f
      (if (> (helper tree "first" <) 0) #f #t)))


;от дърво към стринг
(define (tree->string tree)
  (define (inner tree)
    (cond
      ((empty-tree? tree) "*")
      ((and (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))) (~a "{" (root-tree tree) " " "* *}"))
      ((or (empty-tree? (right-tree tree)) (empty-tree? (left-tree tree))) (~a "{" (root-tree tree) " " (inner (left-tree tree)) " " (inner (right-tree tree)) "}"))
      (else
       (~a "{" (root-tree tree) " " (inner (left-tree tree)) " " (inner (right-tree tree)) "}"))))

  (if (not (valid-list-tree? tree))
      #f
      (inner tree))
  )

;преордер на дървото
(define (stream-preorder tree)
  (if (empty? tree) empty-stream
      (stream-append (stream (root-tree tree)) (stream-preorder (left-tree tree)) (stream-preorder (right-tree tree)))))

;инордер на дървото
(define (stream-inorder tree)
  (if (empty? tree) empty-stream
      (stream-append (stream-inorder (left-tree tree)) (stream (root-tree tree)) (stream-inorder (right-tree tree)))))

;постордер на дървото
(define (stream-postorder tree)
  (if (empty? tree) empty-stream
      (stream-append (stream-postorder (left-tree tree)) (stream-postorder (right-tree tree)) (stream (root-tree tree)))))

;превръща от дърво към поток
(define (tree->stream tree order)
  (cond
    ((equal? order 'preorder) (stream-preorder tree))
    ((equal? order 'inorder) (stream-inorder tree))
    ((equal? order 'postorder) (stream-postorder tree))
    (else
     #f)))

;дава ми колко елемента има, които не са празни
(define (list-num-counter l res)
  (cond
    ((empty? l) res)
    ((equal? (car l) '()) (list-num-counter (cdr l) res))
    (else
     (list-num-counter (cdr l) (+ 1 res)))))


;широчина на дърво
(define (width tree)
  (define (helper ind height tree) 
    (cond
      ((= ind (- height 1)) 0)
      ((empty-tree? tree) 0)
      ((and (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))) 0)
      (else
        (max (+ (list-num-counter (tree-level (left-tree tree) 1) 0) (list-num-counter (tree-level (right-tree tree) 1) 0)) (helper (+ 1 ind) height (left-tree tree)) (helper (+ 1 ind) height (right-tree tree))))))
  (if (eq? (maxdepth tree) 2)
      (list-num-counter (tree-level tree 1) 0)
      (helper 0 (maxdepth tree) tree)))

;строи ми колоните
(define (down tree spaces h)
  (cond
    ((empty-tree? tree) "")
    ((and (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))) (~a  "\n" (make-string spaces #\space) "|\n"  (make-string spaces #\space) (number->string (root-tree tree))))
    (else
     (~a "\n" (make-string spaces #\space) "| \n" (make-string spaces #\space) (number->string (root-tree tree)) (right (right-tree tree) spaces (width (left-tree tree)))
         (down (left-tree tree) spaces (- (maxdepth (right-tree tree)) 1))))))

;строи ми редовете
(define (right tree spaces wd)
  (cond
    ((empty-tree? tree) "")
    ((and (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))) (~a "-" (number->string (root-tree tree))))
    (else
     (~a (make-string (* wd 2) #\-) (number->string (root-tree tree)) (right (right-tree tree) (+ (* 2 wd) spaces (string-length (number->string (root-tree tree)))) (width (left-tree tree)))
         (down (left-tree tree) (- (+ (* 2 wd) spaces (string-length (number->string (root-tree tree)))) 1) (- (maxdepth (right-tree tree)) 1))))))

;Функцията не работи хубаво. Има доста пропуски! Стигнато е дотук с проба-грешка метод.
(define (visualise tree)
  (if (empty-tree? tree) (display "")
      (display (~a (number->string (root-tree tree)) (right (right-tree tree) (string-length (number->string (root-tree tree))) (width (left-tree tree))) (down (left-tree tree) 0 (- (maxdepth (right-tree tree)) 1))))))


;тук се опитах да добавя липсващите празни линии от visualize, но не стана.
#|
(define (get-width str ind res)
  (if (equal? (string (string-ref str ind)) "\n")
      res
      (get-width str (+ 1 ind) (+ 1 res))))
(define (get-height str ind res)
  (cond
    ((= ind (string-length str)) res)
    ((equal? (string (string-ref str ind)) "\n") (get-height str (+ 1 ind) (+ 1 res)))
    (else
     (get-height str (+ 1 ind) res))))


(define (get-indexes-of-numbers-first-row str ind res)
  (define wd (get-width str 0 0))
  (define first-row (substring str 0 wd))
  (cond
    ((>= ind wd) res)
    ((char-numeric? (string-ref str ind)) (get-indexes-of-numbers-first-row str (get_number_index str ind) (append res (list ind))))
    (else
     (get-indexes-of-numbers-first-row str (+ 1 ind) res)))
  )

(define (make-string-to-lists str)
  (string-split str "\n"))

(define (line-setter index strings)
  (define (inner index string)
    (cond
      ((>= index (string-length string)) string)
      ((equal? (string-ref string index) #\|) string)
       (else
        (~a (substring string 0 index) "|" (substring string (+ 1 index))))))


  (define (looper index strings res)
    (cond
      ((null? strings) res)
      (equal? (inner index (car strings)) (car strings)) (append res strings)
      (else
       (looper index (cdr strings) (append res (list (inner index (car string))))))))
  (looper index (cdr strings) '()))



(define (append-matrix indexes strings)
  (define (helper indexes strings res)
    (if (null? indexes)
        res
        (helper (cdr indexes) (cdr strings) (append (list (car strings)) (line-setter (car indexes) strings)))))
  (helper indexes (cdr strings) '())
  )
|#