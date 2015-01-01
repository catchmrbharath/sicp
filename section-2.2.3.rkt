;; Defining the basic higher order functions needed for the exercise

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))


;;Exercise 2.33

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1)
  )

(define x (list 1 2 3 4 5))
(map square x)
(define y (map square x))
(append x y)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;; Exercise 2.34
(define (horner-eval x coeff-seq)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff
                                                   (* x higher-terms)))
              0
              coeff-seq
              ))

(horner-eval 2 (list 1 3 0 5 0 1))


;;Exercise 2.35
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y)) 0 (map 
                                       (lambda (a)
                                         (if (not (pair? a))
                                           1
                                           (count-leaves a)))
                                       t
                                       )))

(define tree (cons (list 1 2) (list 20 4)))
(count-leaves tree)

;; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(accumulate-n + 0 s)

;; Exercise 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (y) (dot-product v y)) m))

(define mat (list (list 1 2 3) (list 4 5 6) (list 6 7 8)))
(define v (list 1 2 3 4))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define mat2 (list (list 1 2) (list 3 4)))


;;Exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(display ( fold-right list nil (list 1 2 3)))
(display ( fold-left list nil (list 1 2 3)))

;;Exercise 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence)
  )

(reverse (list 1 2 3))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
    (cons low (enumerate-interval (+ low 1) high))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j)  (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

