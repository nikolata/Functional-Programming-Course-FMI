#lang racket
(require racket/trace)
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
      (tree? (cadr t))
      (tree? (caddr t))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (list root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define example (make-tree 1
                           (make-tree 3 (make-leaf 8) empty-tree)
                           (make-tree 7 empty-tree (make-tree 9
                                                              (make-leaf 10)
                                                              (make-leaf 11)))))
(define example-bst (make-tree 8
                               (make-tree 3 (make-leaf 1) (make-tree 6 (make-leaf 4) (make-leaf 7)))
                               (make-tree 10 empty-tree (make-tree 14 (make-leaf 13) empty-tree))))

(define (member-tree? x tree)
  (cond
    ((empty-tree? tree) #f)
    ((equal? x (root-tree tree)) #t)
    (else
     (or (member-tree? x (left-tree tree)) (member-tree? x (right-tree tree))))))

(define (sum-tree tree)
  (if (empty-tree? tree)
      0
      (+ (root-tree tree) (sum-tree (left-tree tree)) (sum-tree (right-tree tree)))))

(define (tree-level n tree)
  (define (helper n tree ind res)
    (cond
      ((empty-tree? tree) '())
      ((= ind n) (root-tree tree))
      (else (append res (list (helper n (left-tree tree) (+ ind 1) res)) (list (helper n (right-tree tree) (+ ind 1) res))))))
  (helper n tree 0 '()))

(define (tree-map f tree)
  (cond
    ((empty-tree? tree)'())
    (else (make-tree (f (root-tree tree)) (tree-map f (left-tree tree)) (tree-map f (right-tree tree))))))

(define (tree->list tree)
  (cond
    ((empty-tree? tree) '())
    (else (append (list (root-tree tree)) (tree->list (left-tree tree)) (tree->list (right-tree tree))))))


(define (bst-member? x tree)
  (cond
    ((empty-tree? tree) #f)
    ((equal? x (root-tree tree)) #t)
    ((< x (root-tree tree)) (bst-member? x (left-tree tree)))
    (else
     (bst-member? x (right-tree tree)))))


(define (bst-insert x tree)
  (cond
    ((null? tree) (make-tree x '() '()))
    ((< x (root-tree tree)) (make-tree (root-tree tree) (bst-insert x (left-tree tree)) (right-tree tree)))
    (else (make-tree (root-tree tree) (left-tree tree) (bst-insert x (right-tree tree) )))))