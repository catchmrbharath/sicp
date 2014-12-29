;;Section 2.2.2

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

(count-leaves x)
(count-leaves (list x x))

;;Exercise 2.27
;;Deep reverse

(define (deep-reverse items)
  (define (deep-reverse-iter items ans)
    (cond ((null? items) ans)
          ((not (pair? items)) items)
          (else (deep-reverse-iter (cdr items)
                                   (cons (deep-reverse-iter (car items) nil)
                                         ans)))))
  (deep-reverse-iter items nil))

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)

;;Exercise 2.28
(define (fringe items)
  (define (fringe-iter list1 ans)
    (cond ((null? list1) ans)
          ((pair? list1) (append (fringe-iter (car list1) nil) (fringe-iter (cdr list1) nil)))
          (else (list list1))))
  (fringe-iter items nil)
  )

(fringe x)

;; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (structure-is-mobile? structure)
  (pair? structure))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

;;Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree))) 
       tree))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

;;Exercise 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

;;Exercise 2.32
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))
(subsets (list 1 2 3))
