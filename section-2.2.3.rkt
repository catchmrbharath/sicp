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
